use bs1770::{ChannelLoudnessMeter, Power, Windows100ms};
use clap::Parser;
use rodio::{source::Source, Decoder};
use std::{fs::File, io::BufReader, path::PathBuf, process::Command};
use tempfile::NamedTempFile;

#[derive(Debug, Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path to audio file
    file: PathBuf,
    /// Output file (if not specified, report loudness)
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Target upper bound loudness (in dB)
    #[arg(short, long, default_value_t = -14.)]
    target_upper: f32,
    /// Target instantaneous loudness (in dB)
    #[arg(short, long, default_value_t = -12.)]
    target_instantaneous: f32,
    /// Target lower bound loudness (in dB)
    #[arg(short, long, default_value_t = -18.)]
    target_lower: f32,
    #[arg(short, long, default_value_t = 0.1)]
    trim_amt: f32,
}

fn get_range_without_outliers(vals: &mut Vec<f32>, trim_amt: f32) -> (f32, f32) {
    vals.sort_unstable_by(|x, y| x.partial_cmp(&y).unwrap());
    let lower_bound = vals.len() as f32 * trim_amt;
    let upper_bound = (vals.len() - 1) as f32 * (1. - trim_amt);
    let lower_bound_interp = lower_bound - lower_bound.floor();
    let lower = vals[(lower_bound.floor() as usize).min(vals.len() - 1)] * (1. - lower_bound_interp) + vals[(lower_bound.ceil() as usize).min(vals.len() - 1)] * lower_bound_interp;
    let upper_bound_interp = upper_bound - upper_bound.floor();
    let upper = vals[(upper_bound.floor() as usize).min(vals.len() - 1)] * (1. - upper_bound_interp) + vals[(upper_bound.ceil() as usize).min(vals.len() - 1)] * upper_bound_interp;
    (lower, upper)
}

fn get_loudness_range<P: AsRef<[Power]>>(
    Windows100ms { inner: powers }: Windows100ms<P>,
    window_ms: usize,
    trim_amt: f32,
) -> (f32, f32) {
    let window_size = window_ms / 100;
    let mut vals = powers
        .as_ref()
        .windows(window_size)
        .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
        .filter(|v| v.is_finite())
        .collect::<Vec<_>>();
    get_range_without_outliers(&mut vals, trim_amt)
}

fn main() {
    let cli = Cli::parse();

    let file = BufReader::new(File::open(&cli.file).unwrap());
    let source = Decoder::new(file).unwrap();
    let sample_rate = source.sample_rate();
    let channels = source.channels();
    let mono =
        rodio::source::ChannelVolume::new(source.convert_samples(), vec![0.5; channels as usize])
            .collect::<Vec<f32>>();
    let mono = mono
        .chunks(channels as _)
        .filter_map(|chunk| chunk.first().copied())
        .collect::<Vec<_>>();
    let loudness_lufs = lufs::loudness(mono.iter().copied());
    let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
    loudness_meter.push(mono.iter().copied());
    let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();

    let (min_momentary, max_momentary) =
        get_loudness_range(loudness_meter.as_100ms_windows(), 3000, cli.trim_amt);
    let (min_instantaneous, max_instantaneous) =
        get_loudness_range(loudness_meter.as_100ms_windows(), 400, cli.trim_amt);

    let peak = mono
        .iter()
        .filter(|v| v.is_finite())
        .max_by(|x, y| x.abs().partial_cmp(&y.abs()).unwrap())
        .unwrap()
        .abs();

    let required_amp = lufs::multiplier(loudness_lufs, cli.target_upper).max(1.);

    println!("Input peak: {:.2}", peak);
    println!(
        "Input loudness (integrated): {:.2}LUFS/{:.2}dB",
        loudness_lufs, loudness_db
    );
    println!(
        "Input loudness range (momentary): {:.2}LUFS..{:.2}LUFS",
        min_momentary, max_momentary
    );
    println!(
        "Input loudness range (instantaneous): {:.2}LUFS..{:.2}LUFS",
        min_instantaneous, max_instantaneous
    );
    if required_amp > 1. {
        println!("Required amplification: {}", required_amp);
    }
    println!();

    if let Some(output) = cli.output {
        let limited = NamedTempFile::new()
            .unwrap()
            .into_temp_path()
            .with_extension("wav");

        let target_lower = cli.target_lower;
        let target_upper = cli.target_upper;

        let input_path = format!("{}", cli.file.display());
        let output_path = format!("{}", limited.display());
        let mut limiter = Command::new("python3");
        let limiter = limiter.args([
                "-m",
                "ffmpeg_normalize",
                "--progress",
                "--pre-filter",
                &format!(
                    "volume={required_amp},compand=.3|.3:.3|.3:-90/-90|{min_momentary}/{target_lower}|{max_momentary}/{target_upper}|0/0:6:0.3:-90",
                ),
                "--post-filter",
                &format!("dynaudnorm=g=51:m=5:r=1.0:c=1:b=1:o=0.01"),
                "-f",
                &input_path,
                "-o",
                &output_path,
            ]);
        let limiter_result = limiter.status().unwrap();
        if !limiter_result.success() {
            eprintln!("Could not run limiter");
        }

        let file = BufReader::new(File::open(&limited).unwrap());
        let source = Decoder::new(file).unwrap();
        let channels = source.channels();
        let mono = rodio::source::ChannelVolume::new(
            source.convert_samples(),
            vec![0.5; channels as usize],
        )
        .collect::<Vec<f32>>();
        let mut mono = mono
            .chunks(channels as _)
            .filter_map(|chunk| chunk.first().copied())
            .filter(|v| v.is_finite())
            .collect::<Vec<_>>();
        let loudness_lufs = lufs::loudness(mono.iter().copied());
        let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
        loudness_meter.push(mono.iter().copied());
        let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();
        let (low_peak, high_peak) = get_range_without_outliers(&mut mono, 0.0);
        let peak = low_peak.abs().min(high_peak.abs());

        println!(
            "Limited loudness: {:.2}LUFS/{:.2}dB",
            loudness_lufs, loudness_db
        );
        println!("Limited peak: {:.2}", peak);

        let (_, max_inst) =
            get_loudness_range(loudness_meter.as_100ms_windows(), 400, cli.trim_amt);
        let peak_norm_amp = 1. / peak;
        let desired_amp = lufs::multiplier(max_inst, cli.target_instantaneous);
        let required_amp = desired_amp.max(peak_norm_amp).max(1.);

        if let Ok(()) = std::fs::remove_file(&output) {
            eprintln!("Warning: output file already exists");
        }

        let output_sample_rate =
            if (48000 - sample_rate as i32).abs() < (44100i32 - sample_rate as i32).abs() {
                48000
            } else {
                44100
            };

        let mut normalizer = Command::new("ffmpeg");
        let mut normalizer = normalizer.args([
            "-hide_banner",
            "-loglevel",
                "error",
            "-i",
            &format!("{}", limited.display()),
            "-ar",
            &format!("{}", output_sample_rate),
        ]);
        if required_amp > 1. {
            normalizer = normalizer.args([
                "-filter_complex",
                &format!(
                    "volume={required_amp},compand=attacks=0.01:points=-80/-80|-0.1/-0.25|-0.05/-0.15|0/-0.05|20/-0.05:delay=0.01",
                ),
            ]);
        }
        if output.extension() == Some("mp3".as_ref()) {
            normalizer = normalizer.args(["-c:a", "libmp3lame", "-q:a", "0"]);
        }
        normalizer = normalizer.arg(&format!("{}", output.display()));

        let normalizer_result = normalizer.status().unwrap();
        if !normalizer_result.success() {
            eprintln!("Could not run limiter");
        }

        let file = BufReader::new(File::open(&output).unwrap());
        let source = Decoder::new(file).unwrap();
        let channels = source.channels();
        let mono = rodio::source::ChannelVolume::new(
            source.convert_samples(),
            vec![0.5; channels as usize],
        )
        .collect::<Vec<f32>>();
        let mono = mono
            .chunks(channels as _)
            .filter_map(|chunk| chunk.first().copied())
            .collect::<Vec<_>>();
        let loudness_lufs = lufs::loudness(mono.iter().copied());
        let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
        loudness_meter.push(mono.iter().copied());
        let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();
        let peak = mono
            .iter()
            .filter(|v| v.is_finite())
            .max_by(|x, y| x.abs().partial_cmp(&y.abs()).unwrap())
            .unwrap()
            .abs();

        let (min_momentary, max_momentary) =
            get_loudness_range(loudness_meter.as_100ms_windows(), 3000, cli.trim_amt);
        let (min_instantaneous, max_instantaneous) =
            get_loudness_range(loudness_meter.as_100ms_windows(), 400, cli.trim_amt);

        println!(
            "Final loudness (integrated): {:.2}LUFS/{:.2}dB",
            loudness_lufs, loudness_db
        );
        println!("Final peak: {:.2}", peak);
        println!(
            "Final loudness range (momentary): {:.2}LUFS..{:.2}LUFS",
            min_momentary, max_momentary
        );
        println!(
            "Final loudness range (instantaneous): {:.2}LUFS..{:.2}LUFS",
            min_instantaneous, max_instantaneous
        );
    }
}
