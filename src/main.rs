#![doc = include_str!("../README.md")]

use bs1770::{ChannelLoudnessMeter, Power, Windows100ms};
use clap::Parser;
use itertools::Itertools as _;
use rodio::{cpal::FromSample, decoder::DecoderError, source::Source, Decoder};
use std::{
    cmp::Ordering,
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
    /// 0 will disable normalization, and just convert the file.
    #[arg(long, default_value_t = 1)]
    max_iterations: usize,
    /// Target integrated loudness (in LUFS)
    #[arg(long, default_value_t = -10.)]
    target_integrated: f32,
    /// Frequency to hipass the input at for volume calculations (to normalize tracks with different levels of sub-bass)
    // TODO: It appears the rodio hipass filter has some weird issues
    #[arg(long, default_value_t = 100)]
    hipass: u32,
    /// Target upper bound momentary loudness offset from upper bound (in LUFS)
    #[arg(long, default_value_t = 0.7)]
    momentary_offset: f32,
    /// Target instantaneous loudness offset from upper bound (in LUFS)
    #[arg(long, default_value_t = 1.)]
    instantaneous_offset: f32,
    /// Target lower bound momentary loudness (in LUFS)
    #[arg(long, default_value_t = -15.)]
    target_lower: f32,
    #[arg(short, long, default_value_t = 0.12)]
    trim_amt: f32,
    #[arg(long, default_value_t = -0.1)]
    headroom: f32,
    /// Whether to force-overwrite the final output file
    #[arg(short, long)]
    force: bool,
    /// If any of the volume levels are this far away from the target, we run the process again.
    #[arg(long, default_value_t = 1.5)]
    max_error: f32,
    /// If true, we always enable `dynaudnorm`
    #[arg(short, long)]
    dynaudnorm: bool,
    /// Ratio to upsample the audio to
    #[arg(short, long, default_value_t = 4)]
    resample_amt: u32,
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
            resample_ratio: self.resample_amt.max(1),
            hipass: self.hipass,
            max_error: self.max_error,
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
    max_error: f32,
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
    /// Ratio to upsample the audio to
    resample_ratio: u32,
    /// Frequency to hipass the audio at for volume calculations
    hipass: u32,
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
    sample_rate: u32,
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
        write!(f, "{} peak: {:.2}dB", self.name, self.peak)?;

        Ok(())
    }
}

fn make_mono<S: Source>(source: S, hipass: u32) -> Vec<f32>
where
    <S as Iterator>::Item: rodio::Sample,
    f32: FromSample<<S as Iterator>::Item>,
{
    let channels = source.channels();
    let source = rodio::source::ChannelVolume::new(
        source.convert_samples(),
        vec![1. / channels as f32; channels as usize],
    );

    if hipass > 0 {
        source
            .high_pass(hipass)
            .chunks(channels as _)
            .into_iter()
            .filter_map(|mut chunk| chunk.nth(0))
            .collect::<Vec<_>>()
    } else {
        source
            .chunks(channels as _)
            .into_iter()
            .filter_map(|mut chunk| chunk.nth(0))
            .collect::<Vec<_>>()
    }
}

struct MonoSource {
    inner: <Vec<f32> as IntoIterator>::IntoIter,
    sample_rate: u32,
}

impl Iterator for MonoSource {
    type Item = f32;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl Source for MonoSource {
    fn current_frame_len(&self) -> Option<usize> {
        None
    }

    fn channels(&self) -> u16 {
        1
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }

    fn total_duration(&self) -> Option<std::time::Duration> {
        None
    }
}

fn stats<R: BufRead + Seek + Send + Sync + 'static>(
    name: &'static str,
    file: R,
    trim_amt_loudness: f32,
    trim_amt_peak: f32,
    hipass: u32,
) -> Result<Stats, DecoderError> {
    let source = Decoder::new(file)?;
    let sample_rate = source.sample_rate();
    let mut mono_full = make_mono(source, 0);
    let mono = make_mono(
        MonoSource {
            inner: mono_full.clone().into_iter(),
            sample_rate,
        },
        hipass,
    );
    let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
    loudness_meter.push(mono.iter().copied());
    let integrated = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();

    let (min_momentary, max_momentary) =
        get_rms_range(loudness_meter.as_100ms_windows(), 3000, trim_amt_loudness);
    let (min_instantaneous, max_instantaneous) =
        get_rms_range(loudness_meter.as_100ms_windows(), 400, trim_amt_loudness);

    let (low_peak, high_peak) = get_range_without_outliers(&mut mono_full, trim_amt_peak);
    let peak = amp_to_db(low_peak.abs().min(high_peak.abs()));

    Ok(Stats {
        name,
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        peak,
        sample_rate,
    })
}

fn process(
    params: Params,
    input_file: &Path,
    output_file: &Path,
    dynaudnorm: bool,
) -> io::Result<ProcessResult> {
    let Stats {
        peak,
        integrated,
        min_momentary,
        max_momentary,
        max_instantaneous,
        ..
    } = stats(
        "Input",
        BufReader::new(File::open(&input_file)?),
        params.trim_amt,
        0.0001,
        0,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let result = ProcessResult {
        integrated_error: integrated - params.headroom - params.target_integrated,
        max_momentary_error: max_momentary - params.headroom - params.target_momentary,
        min_momentary_error: min_momentary - params.headroom - params.target_lower,
        instantaneous_error: max_instantaneous - params.headroom - params.target_instantaneous,
    };

    if result.within_acceptable_bounds(params.max_error) {
        convert(None, input_file, output_file, false)?;
        return Ok(result);
    }

    let normalized = temp_wav()?;

    let amp = params.headroom - peak;

    let peak_normalizer_result = Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-stats",
            "-i",
            &format!("{}", input_file.display()),
            "-filter_complex",
            &format!("volume={amp}"),
            &format!("{}", normalized.display()),
        ])
        .status()?;

    if !peak_normalizer_result.success() {
        eprintln!("Could not run limiter");
    }

    let input_stats @ Stats {
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        sample_rate,
        peak,
        ..
    } = stats(
        "Input",
        BufReader::new(File::open(&normalized)?),
        params.trim_amt,
        0.0001,
        0,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let result = ProcessResult {
        integrated_error: integrated - params.headroom - params.target_integrated,
        max_momentary_error: max_momentary - params.headroom - params.target_momentary,
        min_momentary_error: min_momentary - params.headroom - params.target_lower,
        instantaneous_error: max_instantaneous - params.headroom - params.target_instantaneous,
    };

    if result.within_acceptable_bounds(params.max_error) {
        convert(None, &normalized, &output_file, false)?;
        std::fs::remove_file(normalized).unwrap();
        return Ok(result);
    }

    println!("{input_stats}");

    let compressed = temp_wav()?;

    let headroom = params.headroom;

    let upsampled_rate = sample_rate * params.resample_ratio;
    let input_path = format!("{}", normalized.display());
    let output_path = format!("{}", compressed.display());

    let lower_bound = min_momentary - 20.;
    let max_instantaneous = max_instantaneous
        .max(max_momentary + (params.target_instantaneous - params.target_momentary));
    let integrated_mapped = if params.target_integrated < integrated {
        let max_diff = 2.;
        let integrated = integrated.min(params.target_integrated + max_diff);
        let amount_more = integrated - params.target_integrated;
        let half_life = 0.5;
        let scale = max_diff / half_life;
        let lerp = (amount_more / half_life).powi(2) / scale;
        params.target_integrated + lerp * max_diff
    } else {
        params.target_integrated
    };
    let mut compressor_points = [
        (
            min_instantaneous,
            params.target_lower * (min_instantaneous / min_momentary),
        ),
        (min_momentary, params.target_lower),
        (max_momentary, params.target_momentary),
        (
            max_instantaneous,
            params.target_instantaneous.max(max_instantaneous),
        ),
        (integrated, integrated_mapped),
        (peak, 0.),
        (peak + 20., 10.),
    ];

    compressor_points.sort_unstable_by(|x, y| x.0.partial_cmp(&y.0).unwrap_or(Ordering::Equal));

    let mut last = (f32::NEG_INFINITY, f32::NEG_INFINITY);
    let compressor_points = compressor_points.into_iter().filter(|(left, right)| {
        if !left.is_finite() || !right.is_finite() {
            false
        } else if *left > last.0 && *right > last.1 {
            last = (*left, *right);
            true
        } else {
            false
        }
    });
    let (compressor_from, compressor_to): (Vec<_>, Vec<_>) = compressor_points.unzip();
    let compressor_points = compressor_from
        .iter()
        .copied()
        .zip(compressor_to.iter().copied())
        .map(|(from, to)| format!("{from}/{to}"))
        .collect::<Vec<_>>();
    let compressor_points_lesser = compressor_from
        .into_iter()
        .zip(compressor_to.iter().copied())
        .map(|(from, to)| {
            const LESSER_COMP_AMT: f32 = 0.5;
            let to = to * LESSER_COMP_AMT + from * (1. - LESSER_COMP_AMT);
            format!("{from}/{to}")
        })
        .collect::<Vec<_>>();
    let compressor_params = compressor_points.join("|");
    let compressor_params_lesser = compressor_points_lesser.join("|");
    let mut compressor = Command::new("ffmpeg");
    let dynaudnorm_headroom = db_to_amp(-6.);
    let dynaudnorm = if dynaudnorm {
        String::new()
    } else {
        format!("dynaudnorm=g=97:m=1.5:r={dynaudnorm_headroom:.3}:o=0.1:c=1:b=1,")
    };
    let compressor = compressor.args([
        "-hide_banner",
        "-loglevel",
        "error",
        "-stats",
        "-i",
        &input_path,
        "-filter_complex",
        &format!(
            "aresample={upsampled_rate},\
                compand=attacks=1:decays=1:points=-90/-90|{compressor_params_lesser}:soft-knee=1:delay=.95:volume={lower_bound},\
                compand=attacks=.3:decays=.2:points=-90/-90|{compressor_params}:soft-knee=1:delay=.2:volume={lower_bound},\
                {dynaudnorm}\
                aresample={sample_rate}"
        ),
        &output_path,
    ]);
    let limiter_result = compressor.status()?;
    if !limiter_result.success() {
        eprintln!("Could not run limiter");
    }

    let compressed_stats @ Stats {
        integrated,
        max_momentary,
        min_instantaneous: _,
        max_instantaneous,
        sample_rate,
        peak,
        ..
    } = stats(
        "Compressed",
        BufReader::new(File::open(&compressed)?),
        params.trim_amt,
        0.05,
        params.hipass,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    let max_amp = db_to_amp(-peak);
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
    .min(max_amp);

    println!("{compressed_stats}");
    println!("Required final amp: {:.2}dB", amp_to_db(required_amp));

    if let Ok(()) = std::fs::remove_file(output_file) {
        eprintln!("Warning: output file already exists");
    }

    let mut limiter = Command::new("ffmpeg");
    let upsampled_rate = sample_rate * params.resample_ratio;
    let limiter = limiter.args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-stats",
            "-i",
            &format!("{}", compressed.display()),
            "-filter_complex",
            &format!(
                "volume={required_amp},\
                    aresample={upsampled_rate},\
                    compand=attacks=0.7:points=-80/-80|-12.4/-12.4|-1.5/-3|-1/-2|0/0|20/10:soft-knee=1:delay=0.3,\
                    compand=attacks=0.2:points=-80/-80|-12.4/-12.4|0/0|20/5:soft-knee=0.5:delay=0.1,\
                    compand=attacks=0.01:points=-80/-80|-12.4/-12.4|-1/-1.5|-0.5/-0.75|0/0|20/0:delay=0.01,\
                    compand=attacks=0:points=-80/-80|-12.4/-12.4|0/0|20/0,\
                    aresample={sample_rate},
                    volume={headroom}dB",
            ),
            &format!("{}", output_file.display()),
        ]);

    let normalizer_result = limiter.status()?;
    if !normalizer_result.success() {
        eprintln!("Could not run limiter");
    }

    std::fs::remove_file(normalized).unwrap();
    std::fs::remove_file(compressed).unwrap();

    let Stats {
        integrated,
        min_momentary,
        max_momentary,
        min_instantaneous,
        max_instantaneous,
        peak,
        ..
    } = stats(
        "Final",
        BufReader::new(File::open(output_file)?),
        params.trim_amt,
        0.0001,
        0,
    )
    .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e))?;

    println!(
        "Final adjusted loudness (integrated): {:.2}LUFS",
        integrated - params.headroom
    );
    println!(
        "Final peak: {:.2}dB (true) {:.2}dB (adjusted)",
        peak,
        peak - params.headroom,
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

fn convert(metadata: Option<&Path>, input: &Path, output: &Path, force: bool) -> io::Result<()> {
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

    if let Some(meta) = metadata {
        converter = converter.args([
            "-i",
            &format!("{}", meta.display()),
            "-map",
            "0:a",
            "-map",
            "1:v?",
            "-id3v2_version",
            "3",
            "-write_id3v2",
            "1",
            "-map_metadata:s:a",
            "1:s:a",
            "-map_metadata:s:v",
            "1:s:v",
            "-map_metadata:g",
            "1:g",
            "-map_metadata",
            "1",
            "-c:v",
            "copy",
        ]);
    }

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
    extension == Some("wav".as_ref())
        || extension == Some("mp3".as_ref())
        || extension == Some("flac".as_ref())
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
                convert(None, &cli.file, &buffer_a, false).unwrap();
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
                println!("Greater error than previous step, reverting");
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

        convert(Some(&cli.file), &buffer_a, &output, cli.force).unwrap();

        assert_ne!(buffer_a, cli.file);
        fs::remove_file(&buffer_a).unwrap();
    } else {
        let input_stats = stats(
            "Input",
            BufReader::new(File::open(&cli.file).unwrap()),
            cli.trim_amt,
            0.0001,
            cli.hipass,
        )
        .unwrap();

        println!("{input_stats}");
    }
}
