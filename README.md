# Normalizer

"Good-enough" remastering to roughly equalise dynamic range and overall loudness, intended for
DJs who play older tracks and/or tracks across many genres. Not intended for non-dance music
genres, but should work well enough for them anyway.

This works by combining many FFMPEG filters, tweaking their parameters according to various
statistics detected directly from the tracks. This program _will_ change the sound of your
tracks, and while I am currently testing using it for my entire library it is recommended that
you try it for individual tracks or albums and listen through the tracks before playing them
in a set.

## Installation

You will need `ffmpeg`, the python library `ffmpeg_normalize` (install with `pip3 install ffmpeg-normalize`),
and `cargo` (see [rustup](https://rustup.rs/))

## Details

We first detect the integrated loudness of the whole track, along with the IPR (inter-percentile
range) of momentary loudness - with the proportion of outliers being configurable but defaulting
to excluding the loudest and quietest 10%. We use the range to configure a compressor, with the
compressor designed so parts of the tracks at the lower of the volume range are mapped to a
configurable dB value (defaulting to -18dB), and the parts at the higher end are mapped to the
target overall loudness (defaulting to -14dB). We add pre-gain according to the difference between
the integrated loudness and the target loudness. From testing this appears to give pretty good
results and balances the needs of increasing dynamic range in overly-loud tracks while reducing
dynamic range in tracks which can sound too quiet on club speakers. After the compressor, we run
the track through FFMPEG's `loudnorm` algorithm. This algorithm scales the track based on overall
perceptual loudness. We then use `dynaudnorm`, which is based on RMS loudness and not perceptual
loudness, but works in overlapping chunks and so can better-handle much-quieter parts of the track.

These three steps - the compressor, `loudnorm` and `dynaudnorm`, work together to approximately
equalise loudness and dynamic range. However, by themselves they can't get the level of loudness
expected for modern tracks - `loudnorm` is intended to reduce the volume of tracks, not increase it,
and `dynaudnorm` is intended to handle dynamic range, not overall loudness.

The final step is to scale the track, using a heuristic that attempts to balance peak normalisation
along with trying to scale the instantaneous loudness to a configurable value (defaulting to -12dB).
We then apply a very fast limiter which clips off the peaks. This part takes into account possible
DC offset (i.e. if the low peak is -0.9 and the high peak is 0.8, it will normalise the track to
bring that 0.8 to 1.0), it also removes a very small number of outliers from the peak detection,
although removing too many outliers makes it scale the track too much and makes it too loud. From
testing, simple peak normalisation at this step almost always makes the track too quiet, so this
slightly-more-complex method is closer to the transient-preserving clipping done in the mastering
of most modern dance music.

In writing this tool, I tried many different methods to automatically remaster tracks. Right now, the
biggest issue is that it doesn't handle tracks that are already mastered too loud and with too little
dynamic range. It will try to add a small amount of dynamic range but ultimately if the master already
looks like a flat sausage there's not much I can do to save it. It currently will usually not increase
the integrated loudness further, but occasionally it will and this is an issue that will require further
testing to figure out if it causes the final masters to sound worse or if the integrated loudness is
only higher because the track contained very quiet parts that were boosted.

In general, this tool needs extensive battle-testing by being used on real tracks by real DJs. It will
never be a replacement for manually remastering a track and certainly it's no replacement for just getting
a better master of the track to begin with, but it should be good enough and do a better job than LANDR
(since LANDR isn't built to handle already-mastered tracks).

_This tool is not intended to be used to automatically master unmastered tracks_. If you're interested,
try it out - it would be interesting to see what it does with audio that has much more dynamic range
than what it expects. It's not the intended use though. This is intended to take tracks that were maybe
mastered in an earlier, pre-digital era and bring them up to current loudness and dynamic range standards for
club play - as well as approximately equalising loudness between genres. Any other use is not necessarily
going to be supported.
