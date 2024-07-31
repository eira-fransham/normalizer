#![doc(include_str = "README.md")]

use bs1770::{ChannelLoudnessMeter, Power, Windows100ms};
use clap::Parser;
use itertools::Itertools as _;
use rodio::{decoder::DecoderError, source::Source, Decoder};
use std::{
    ffi::OsStr,
    fmt::Display,
    fs::{self, File},
    io::{self, BufRead, BufReader, Seek},
    mem,
    path::{Path, PathBuf},
    process::Command,
};
use tempfile::NamedTempFile;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path to audio file
    file: PathBuf,
    /// Output file (if not specified, report loudness)
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Max interations to try (in case the loudness is still too far from the target)
    #[arg(long, default_value_t = 2)]
    max_iterations: usize,
    /// Target integrated loudness (in LUFS)
    #[arg(long, default_value_t = -10.)]
    target_integrated: f32,
    /// Target upper bound momentary loudness offset from upper bound (in LUFS)
    #[arg(long, default_value_t = 1.)]
    momentary_offset: f32,
    /// Target instantaneous loudness offset from upper bound (in LUFS)
    #[arg(long, default_value_t = 2.)]
    instantaneous_offset: f32,
    /// Target lower bound momentary loudness (in LUFS)
    #[arg(long, default_value_t = -18.)]
    target_lower: f32,
    #[arg(short, long, default_value_t = 0.1)]
    trim_amt: f32,
    #[arg(long, default_value_t = -1.)]
    headroom: f32,
    /// Whether to force-overwrite the final output file
    #[arg(short, long)]
    force: bool,
    /// If any of the volume levels are this far away from the target, we run the process again.
    #[arg(long, default_value_t = 1.)]
    max_error: f32,
    /// If true, we always enable `dynaudnorm`
    #[arg(short, long)]
    dynaudnorm: bool,
    /// The lower threshold for integrated loudness compared to target after an iteration to enable `dynaudnorm`.
    /// `dynaudnorm` may introduce artefacts but can be useful to reduce whole-track dynamics.
    /// Setting this to 0 or more disables, as it is only intended to reduce dynamics (and
    /// therefore boost integrated loudness compared to momentary loudness).
    #[arg(long, default_value_t = 0.)]
    dynaudnorm_threshold: f32,
}

impl Cli {
    fn process_params(&self) -> Params {
        Params {
            target_integrated: self.target_integrated,
            target_lower: self.target_lower,
            target_momentary: self.target_momentary(),
            target_instantaneous: self.target_instantaneous(),
            headroom: self.headroom,
            trim_amt: self.trim_amt,
        }
    }

    fn target_momentary(&self) -> f32 {
        self.target_integrated + self.momentary_offset
    }

    fn target_instantaneous(&self) -> f32 {
        self.target_integrated + self.instantaneous_offset
    }
}

fn get_range_without_outliers(vals: &mut Vec<f32>, trim_amt: f32) -> (f32, f32) {
    vals.sort_unstable_by(|x, y| x.partial_cmp(&y).unwrap());
    let lower_bound = vals.len() as f32 * trim_amt;
    let upper_bound = (vals.len() - 1) as f32 * (1. - trim_amt);
    let lower_bound_interp = lower_bound - lower_bound.floor();
    let upper_bound_interp = upper_bound - upper_bound.floor();
    let (lower_a, lower_b) = (
        vals.get((lower_bound.floor() as usize).min(vals.len() - 1)),
        vals.get((lower_bound.ceil() as usize).min(vals.len() - 1)),
    );
    let lower = match (lower_a, lower_b) {
        (Some(a), Some(b)) => a * (1. - lower_bound_interp) + b * lower_bound_interp,
        (None, Some(a)) | (Some(a), None) => *a,
        (None, None) => 0.,
    };
    let (upper_a, upper_b) = (
        vals.get((upper_bound.floor() as usize).min(vals.len() - 1)),
        vals.get((upper_bound.ceil() as usize).min(vals.len() - 1)),
    );
    let upper = match (upper_a, upper_b) {
        (Some(a), Some(b)) => a * (1. - upper_bound_interp) + b * upper_bound_interp,
        (None, Some(a)) | (Some(a), None) => *a,
        (None, None) => 0.,
    };
    (lower, upper)
}

fn get_rms_range<P: AsRef<[Power]>>(
    Windows100ms { inner: powers }: Windows100ms<P>,
    window_ms: usize,
    trim_amt: f32,
) -> (f32, f32) {
    let window_size = window_ms / 100;
    let mut non_finite_count = 0;
    let mut vals = powers
        .as_ref()
        .windows(window_size)
        .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
        .filter(|v| {
            if v.is_finite() {
                true
            } else {
                non_finite_count += 1;
                false
            }
        })
        .collect::<Vec<_>>();
    if cfg!(debug_assertions) {
        eprintln!("Num non-finite LUFS values: {}", non_finite_count);
    }
    get_range_without_outliers(&mut vals, trim_amt)
}

fn db_to_amp(db: f32) -> f32 {
    10f32.powf(db / 20.)
}

fn amp_to_db(amp: f32) -> f32 {
    20. * amp.log10()
}

fn lufs_multiplier(current: f32, target: f32) -> f32 {
    db_to_amp(target) / db_to_amp(current)
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Params {
    /// Target overall loudness
    target_integrated: f32,
    /// Target lower bound for momentary loudness
    target_lower: f32,
    /// Target maximum momentary loudness (in LUFS)
    target_momentary: f32,
    /// Target maximum instantaneous loudness (in LUFS)
    target_instantaneous: f32,
    /// Target headroom (in dB)
    headroom: f32,
    /// Proportion of high/low outliers to trim from loudness range calculations
    trim_amt: f32,
}

#[derive(Debug, Copy, Clone)]
struct ProcessResult {
    integrated_error: f32,
    min_momentary_error: f32,
    max_momentary_error: f32,
    instantaneous_error: f32,
}

impl Display for ProcessResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Integrated error: {:.2}LUFS", self.integrated_error)?;
        writeln!(
            f,
            "Lower momentary error: {:.2}LUFS",
            self.min_momentary_error
        )?;
        writeln!(
            f,
            "Upper momentary error: {:.2}LUFS",
            self.max_momentary_error
        )?;
        write!(
            f,
            "Instantaneous error: {:.2}LUFS",
            self.instantaneous_error
        )?;

        Ok(())
    }
}

const INTEGRATED_ERROR_MULTIPLIER: f32 = 1.1;
const MIN_MOMENTARY_ERROR_MULTIPLIER: f32 = 10.;
const MAX_MOMENTARY_ERROR_MULTIPLIER: f32 = 1.;
const INSTANTANEOUS_ERROR_MULTIPLIER: f32 = 1.5;

impl ProcessResult {
    fn better_than(&self, other: &Self) -> bool {
        (self.integrated_error.abs() - other.integrated_error.abs()) / INTEGRATED_ERROR_MULTIPLIER
            + (self.max_momentary_error.abs() - other.max_momentary_error.abs())
                / MAX_MOMENTARY_ERROR_MULTIPLIER
            + (self.min_momentary_error.abs() - other.min_momentary_error.abs())
                / MIN_MOMENTARY_ERROR_MULTIPLIER
            + (self.instantaneous_error.abs() - other.instantaneous_error.abs())
                / INSTANTANEOUS_ERROR_MULTIPLIER
            < 0.
    }

    fn within_acceptable_bounds(&self, max_error: f32) -> bool {
        self.integrated_error.abs() <= max_error * INTEGRATED_ERROR_MULTIPLIER
            && self.max_momentary_error.abs() <= max_error * MAX_MOMENTARY_ERROR_MULTIPLIER
            && self.min_momentary_error.abs() <= max_error * MIN_MOMENTARY_ERROR_MULTIPLIER
            && self.instantaneous_error.abs() <= max_error * INSTANTANEOUS_ERROR_MULTIPLIER
    }

    /// Whether to enable `dynaudnorm`, reducing overall track dynamics. We
    /// factor momentary error into the calculation as `dynaudnorm` is best used to
    /// boost overall track dynamics compared to the peak momentary loudness
    fn enable_dynaudnorm(&self, threshold: f32) -> bool {
        if threshold >= 0. {
            false
        } else {
            self.integrated_error - self.max_momentary_error - threshold >= 0.
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Stats {
    name: &'static str,
    integrated: f32,
    min_momentary: f32,
    max_momentary: f32,
    min_instantaneous: f32,
    max_instantaneous: f32,
    peak: f32,
}

impl Display for Stats {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} loudness (integrated): {:.2}LUFS",
            self.name, self.integrated
        )?;
        writeln!(
            f,
            "{} loudness range (momentary): {:.2}LUFS..{:.2}LUFS",
            self.name, self.min_momentary, self.max_momentary
        )?;
        writeln!(
            f,
            "{} loudness range (instantaneous): {:.2}LUFS..{:.2}LUFS",
            self.name, self.min_instantaneous, self.max_instantaneous
        )?;
        write!(f, "{} peak: {:.2}", self.name, self.peak)?;

        Ok(())
    }
}

fn stats<R: BufRead + Seek + Send + Sync + 'static>(
    name: &'static str,
    file: R,
    trim_amt_loudness: f32,
    trim_amt_peak: f32,
) -> Result<Stats, DecoderError> {
    let source = Decoder::new(file)?;
    let sample_rate = source.sample_rate();
    let channels = source.channels();
    let mut mono =
        rodio::source::ChannelVolume::new(source.convert_samples(), vec![0.5; channels as usize])
            .chunks(channels as _)
            .into_iter()
            .filter_map(|mut chunk| chunk.nth(0))
            .collect::<Vec<_>>();
    let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
    loudness_meter.push(mono.iter().copied());
    let integrated = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();

    let (min_momentary, max_momentary) =
        get_rms_range(loudness_meter.as_100ms_windows(), 3000, trim_amt_loudness);
    let (min_instantaneous, max_instantaneous) =
        get_rms_range(loudness_meter.as_100ms_windows(), 400, trim_amt_loudness);

    let (low_peak, high_peak) = get_range_without_outliers(&mut mono, trim_amt_peak);
    let peak = low_peak.abs().min(high_peak.abs());

    Ok(Stats {
        name,
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        peak,
    })
}

fn process(
    params: Params,
    input_file: &Path,
    output_file: &Path,
    dynaudnorm: bool,
) -> io::Result<ProcessResult> {
    let input_stats @ Stats {
        name: _,
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        peak: _,
    } = stats(
        "Input",
        BufReader::new(File::open(&input_file)?),
        params.trim_amt,
        0.0001,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let required_amp = lufs_multiplier(integrated, params.target_instantaneous).max(1.);
    let required_amp_db = amp_to_db(required_amp);

    println!("{input_stats}");

    let compressed = NamedTempFile::new().unwrap().into_temp_path();
    std::fs::remove_file(&compressed).unwrap();
    let compressed = compressed.with_extension("wav");

    let target_lower = params.target_lower;
    let target_upper = params.target_integrated;
    let target_instantaneous = params.target_instantaneous;

    let headroom_amp = db_to_amp(params.headroom);

    let input_path = format!("{}", input_file.display());
    let output_path = format!("{}", compressed.display());
    let lower_bound = min_momentary - 20.;
    let lower_bound_mapped = required_amp_db + lower_bound;
    let compressor_points = [
        (lower_bound, lower_bound_mapped),
        (
            min_instantaneous,
            target_lower - (params.target_integrated - integrated) * 0.25
                + (target_instantaneous - target_upper),
        ),
        (
            min_momentary,
            target_lower + (params.target_integrated - integrated) * 0.5,
        ),
        (max_momentary, target_upper),
        (max_instantaneous, target_instantaneous),
    ];
    let (mut compressor_from, mut compressor_to): (Vec<_>, Vec<_>) =
        <_>::into_iter(compressor_points).unzip();
    compressor_from.sort_unstable_by(|x, y| x.partial_cmp(&y).unwrap());
    compressor_to.sort_unstable_by(|x, y| x.partial_cmp(&y).unwrap());
    let compressor_points = compressor_from
        .into_iter()
        .zip(compressor_to)
        .map(|(from, to)| format!("{from}/{to}"))
        .collect::<Vec<_>>();
    let compressor_params = compressor_points.join("|");
    let mut compressor = Command::new("ffmpeg");
    let dynaudnorm_headroom = -6.;
    let compressor = compressor.args([
        "-hide_banner",
        "-loglevel",
        "error",
        "-stats",
        "-i",
        &input_path,
        "-filter_complex",
        &if dynaudnorm {
            format!(
                "compand=.3|.3:.3|.3:{compressor_params}:3:0.3:{lower_bound},\
                dynaudnorm=g=51:m=1.5:r={dynaudnorm_headroom}:c=1:b=1",
            )
        } else {
            format!("compand=.3|.3:.3|.3:{compressor_params}:3:0.3:{lower_bound}",)
        },
        &output_path,
    ]);
    let limiter_result = compressor.status().unwrap();
    if !limiter_result.success() {
        eprintln!("Could not run limiter");
    }

    let compressed_stats @ Stats {
        name: _,
        integrated,
        min_momentary: _,
        max_momentary,
        min_instantaneous: _,
        max_instantaneous,
        peak,
    } = stats(
        "Compressed",
        BufReader::new(File::open(&compressed)?),
        params.trim_amt,
        0.0001,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let max_amp = 1. / (peak * headroom_amp);
    let desired_amp_inst = lufs_multiplier(max_instantaneous, params.target_instantaneous);
    let desired_amp_max_mom = lufs_multiplier(max_momentary, params.target_momentary);
    let desired_amp_min_mom = lufs_multiplier(min_momentary, params.target_lower);
    let desired_amp_integrated = lufs_multiplier(integrated, params.target_integrated);
    let amps = [
        (desired_amp_inst, INSTANTANEOUS_ERROR_MULTIPLIER),
        (desired_amp_min_mom, MIN_MOMENTARY_ERROR_MULTIPLIER),
        (desired_amp_max_mom, MAX_MOMENTARY_ERROR_MULTIPLIER),
        (desired_amp_integrated, INTEGRATED_ERROR_MULTIPLIER),
    ]
    .map(|(desired, mul)| (desired, mul.recip()));

    // Weighted average based on the allowable error levels
    let required_amp = (amps.map(|(d, m)| d * m).into_iter().sum::<f32>()
        / amps.map(|(_, m)| m).into_iter().sum::<f32>())
    .max(max_amp);

    println!("{compressed_stats}");
    println!("Required final amp: {:.1}dB", amp_to_db(required_amp));

    if let Ok(()) = std::fs::remove_file(output_file) {
        eprintln!("Warning: output file already exists");
    }

    let mut limiter = Command::new("ffmpeg");
    let limiter = limiter.args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-stats",
            "-i",
            &format!("{}", compressed.display()),
            "-filter_complex",
            &format!(
                "volume={required_amp},compand=attacks=0.01:points=-80/-80|-12.4/-12.4|0/0|20/0:soft-knee=2:delay=0.01,compand=attacks=0:points=-80/-80|-12.4/-12.4|-2/-1|-1/-0.5|0/0|20/0,volume={headroom_amp}",
            ),
            &format!("{}", output_file.display()),
        ]);

    let normalizer_result = limiter.status().unwrap();
    if !normalizer_result.success() {
        eprintln!("Could not run limiter");
    }

    std::fs::remove_file(compressed).unwrap();

    let Stats {
        name: _,
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        peak,
    } = stats(
        "Final",
        BufReader::new(File::open(output_file)?),
        params.trim_amt,
        0.0001,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    println!(
        "Final adjusted loudness (integrated): {:.2}LUFS",
        integrated - params.headroom
    );
    println!(
        "Final peak: {:.2} (true) {:.2} (adjusted)",
        peak,
        peak / headroom_amp
    );
    println!(
        "Final adjusted loudness range (momentary): {:.2}LUFS..{:.2}LUFS",
        min_momentary - params.headroom,
        max_momentary - params.headroom
    );
    println!(
        "Final adjusted loudness range (instantaneous): {:.2}LUFS..{:.2}LUFS",
        min_instantaneous - params.headroom,
        max_instantaneous - params.headroom
    );

    Ok(ProcessResult {
        integrated_error: integrated - params.headroom - params.target_integrated,
        max_momentary_error: max_momentary - params.headroom - params.target_momentary,
        min_momentary_error: min_momentary - params.headroom - params.target_lower,
        instantaneous_error: max_instantaneous - params.headroom - params.target_instantaneous,
    })
}

fn temp_wav() -> io::Result<PathBuf> {
    let out = NamedTempFile::new()?.into_temp_path();
    fs::remove_file(&out)?;
    Ok(out.with_extension("wav"))
}

fn convert(input: &Path, output: &Path, force: bool) -> io::Result<()> {
    let mut converter = Command::new("ffmpeg");

    let mut converter = if force {
        converter.arg("-y")
    } else {
        &mut converter
    };

    converter = converter.args([
        "-hide_banner",
        "-loglevel",
        "error",
        "-stats",
        "-i",
        &format!("{}", input.display()),
    ]);
    if output.extension() == Some("mp3".as_ref()) {
        converter = converter.args(["-c:a", "libmp3lame", "-q:a", "0"]);
    }
    converter = converter.arg(&format!("{}", output.display()));
    let converter_result = converter.status()?;
    if !converter_result.success() {
        eprintln!("Could not run convert");
    }
    Ok(())
}

// Some flac files seem to not be supported by `rodio`, so we convert to wav for safety
fn file_extension_always_supported(extension: Option<&OsStr>) -> bool {
    extension == Some("wav".as_ref()) || extension == Some("mp3".as_ref())
}

fn main() {
    let cli = Cli::parse();

    println!("Normalizer");
    if let Some(filename) = cli.file.file_name() {
        println!("Processing \"{}\"", filename.to_string_lossy());
    }

    if let Some(output) = &cli.output {
        let (mut buffer_a, mut buffer_b, mut delete_prev_buffer) =
            if file_extension_always_supported(cli.file.extension()) {
                (cli.file.clone(), temp_wav().unwrap(), false)
            } else {
                let buffer_a = temp_wav().unwrap();
                convert(&cli.file, &buffer_a, false).unwrap();
                (buffer_a, temp_wav().unwrap(), true)
            };

        let mut dynaudnorm_enabled = cli.dynaudnorm;
        let mut last_processed = None;

        for _ in 0..cli.max_iterations {
            let processed = process(
                cli.process_params(),
                &buffer_a,
                &buffer_b,
                dynaudnorm_enabled,
            )
            .unwrap();

            let not_improving = if last_processed
                .map(|last_processed| processed.better_than(&last_processed))
                .unwrap_or(true)
            {
                mem::swap(&mut buffer_a, &mut buffer_b);
                false
            } else {
                true
            };

            last_processed = Some(processed);

            if delete_prev_buffer {
                assert_ne!(buffer_b, cli.file);
                fs::remove_file(&buffer_b).unwrap();
            }

            buffer_b = temp_wav().unwrap();
            delete_prev_buffer = true;

            dynaudnorm_enabled |= processed.enable_dynaudnorm(cli.dynaudnorm_threshold);

            if not_improving || processed.within_acceptable_bounds(1.) {
                break;
            }
        }

        convert(&buffer_a, &output, cli.force).unwrap();

        assert_ne!(buffer_a, cli.file);
        fs::remove_file(&buffer_a).unwrap();
    } else {
        let input_stats = stats(
            "Input",
            BufReader::new(File::open(&cli.file).unwrap()),
            cli.trim_amt,
            0.0001,
        )
        .unwrap();

        println!("{input_stats}");
    }
}
