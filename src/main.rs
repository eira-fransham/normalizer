use bs1770::{ChannelLoudnessMeter, Windows100ms};
use clap::Parser;
use itertools::Itertools;
use rodio::{source::Source, Decoder};
use std::{cmp::Ordering, fs::File, io::BufReader, path::PathBuf, process::Command};
use tempfile::NamedTempFile;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Path to audio file
    file: PathBuf,
    /// Output file (if not specified, report loudness)
    #[arg(short, long)]
    output: Option<PathBuf>,
    /// Target loudness
    #[arg(short, long, default_value_t = -14.)]
    target: f32,
}

fn main() {
    let cli = Cli::parse();

    let file = BufReader::new(File::open(&cli.file).unwrap());
    let source = Decoder::new(file).unwrap();
    let sample_rate = source.sample_rate();
    let source = source.convert_samples().collect::<Vec<f32>>();
    let loudness_lufs = lufs::loudness(source.iter().copied());
    let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
    loudness_meter.push(source.iter().copied());
    let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();

    let (min_momentary, max_momentary) = loudness_meter
        .as_100ms_windows()
        .inner
        .windows(30)
        .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
        .minmax_by(|x, y| x.partial_cmp(&y).unwrap_or(Ordering::Equal))
        .into_option()
        .unwrap_or_default();

    let (min_instantaneous, max_instantaneous) = loudness_meter
        .as_100ms_windows()
        .inner
        .windows(4)
        .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
        .minmax_by(|x, y| x.partial_cmp(&y).unwrap_or(Ordering::Equal))
        .into_option()
        .unwrap_or_default();

    let peak = source
        .iter()
        .max_by(|x, y| x.abs().partial_cmp(&y.abs()).unwrap())
        .unwrap()
        .abs();

    let required_amp = lufs::multiplier(loudness_lufs, cli.target);

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
    println!("Required amplification: {}", required_amp);
    println!();

    if let Some(output) = cli.output {
        let limited = NamedTempFile::new()
            .unwrap()
            .into_temp_path()
            .with_extension("wav");

        let input_path = format!("{}", cli.file.display());
        let output_path = format!("{}", limited.display());
        let mut limiter = Command::new("python3");
        let limiter = limiter.args([
            "-m",
            "ffmpeg_normalize",
            "--progress",
            "--pre-filter",
            &format!(
                "volume={},compand=.3|.3:1|1:-90/-50|-60/-30|-40/-25|-20/-20|0/0:6:0:-90:0.3",
                required_amp
            ),
            "--post-filter",
            &format!("dynaudnorm=g=31:m=5:r=1.0:c=1:b=1:o=0.05"),
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
        let source = source.convert_samples().collect::<Vec<f32>>();
        let loudness_lufs = lufs::loudness(source.iter().copied());
        let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
        loudness_meter.push(source.iter().copied());
        let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();
        let peak = source
            .iter()
            .max_by(|x, y| x.abs().partial_cmp(&y.abs()).unwrap())
            .unwrap()
            .abs();

        println!(
            "Limited loudness: {:.2}LUFS/{:.2}dB",
            loudness_lufs, loudness_db
        );
        println!("Limited peak: {:.2}", peak);

        let required_amp = 1. / peak;

        if let Ok(()) = std::fs::remove_file(&output) {
            eprintln!("Warning: output file already exists");
        }

        let mut normalizer = Command::new("ffmpeg");
        let normalizer = normalizer.args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-i",
            &format!("{}", limited.display()),
            "-filter_complex",
            &format!(
                "volume={},compand=attacks=0:points=-80/-80|-1/-0.5|0/0|20/0",
                required_amp
            ),
            &format!("{}", output.display()),
        ]);
        let normalizer_result = normalizer.status().unwrap();
        if !normalizer_result.success() {
            eprintln!("Could not run limiter");
        }

        let file = BufReader::new(File::open(&output).unwrap());
        let source = Decoder::new(file).unwrap();
        let source = source.convert_samples().collect::<Vec<f32>>();
        let loudness_lufs = lufs::loudness(source.iter().copied());
        let mut loudness_meter = ChannelLoudnessMeter::new(sample_rate);
        loudness_meter.push(source.iter().copied());
        let loudness_db = bs1770::gated_mean(loudness_meter.as_100ms_windows()).loudness_lkfs();
        let peak = source
            .iter()
            .max_by(|x, y| x.abs().partial_cmp(&y.abs()).unwrap())
            .unwrap()
            .abs();

        let (min_momentary, max_momentary) = loudness_meter
            .as_100ms_windows()
            .inner
            .windows(30)
            .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
            .minmax_by(|x, y| x.partial_cmp(&y).unwrap())
            .into_option()
            .unwrap_or_default();

        let (min_instantaneous, max_instantaneous) = loudness_meter
            .as_100ms_windows()
            .inner
            .windows(4)
            .map(|window| bs1770::gated_mean(Windows100ms { inner: window }).loudness_lkfs())
            .minmax_by(|x, y| x.partial_cmp(&y).unwrap_or(Ordering::Equal))
            .into_option()
            .unwrap_or_default();

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
