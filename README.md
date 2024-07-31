# Normalizer

"Good-enough" remastering to roughly equalise dynamic range and overall loudness, intended for
DJs who play older tracks and/or tracks across many genres. Not intended for non-dance music
genres, but should work well enough for them anyway.

This works by configuring some limiters and compressors in `ffmpeg` based on the detected
[perceptual loudness](https://en.wikipedia.org/wiki/Loudness) of the track across various
time periods. It works completely differently from [`ffmpeg-normalize`](https://github.com/slhck/ffmpeg-normalize),
which is intended to preserve the waveform, only scaling it down so that different tracks are the same
overall loudness when the whole track is measured. This program _will_ change the sound of your tracks.
While `ffmpeg-normalize` is intended to trust the master that is given to it and only prevent you
from needing to adjust your volume when putting many tracks in a playlist, this tool is intended to
take tracks mastered in many different styles with different loudness profiles across the track and
optimise dynamics for club play. It uses compression and limiting to remaster the track, giving you a
reasonable baseline to make mixing tracks and making mashups much easier. It can also

It can also be used as a quick-and-dirty way to master unmastered tracks for club play, without using
an AI service like LANDR.  I have tried it on unmastered tracks and it does appear to work pretty well,
producing masters that are at least reasonably listenable. For one of the tracks I tried from my album,
it produced something that actually sounds better on my phone speakers than the professional master due
to the reduced dynamic range, although on better speakers the transients are too loud and it's obviously
not even close to the quality of a professional master - nor is it intended to be. It currently makes no
attempt to handle the frequency distribution of a track (e.g. normalising tracks that have much more bass
vs tracks with much less), as `mcompand` (`ffmpeg`'s multiband compressor) appears to be bugged and
produces extremely ugly artifacts no matter the settings.

## Installation

You will need `ffmpeg` (see [their website](https://ffmpeg.org/)) and `cargo` (see [rustup](https://rustup.rs/))

## Details

We first detect the integrated loudness of the whole track, along with the IPR (inter-percentile
range) of momentary and instantaneous loudness - with the proportion of outliers being configurable
but defaulting to excluding the loudest and quietest 10%. We use the range to configure a compressor, with the
compressor designed so parts of the tracks at the lower of the volume range are mapped to a
configurable dB value (defaulting to -18dB), and the parts at the higher end are mapped to the
target overall loudness (defaulting to -14dB). We add pre-gain according to the difference between
the integrated loudness and the target loudness. From testing this appears to give pretty good
results and balances the needs of increasing dynamic range in overly-loud tracks while reducing
dynamic range in tracks which can sound too quiet on club speakers.

We then detect loudness over the different time ranges on the compressed signal, and run that through
two limiters - a slow, more-gentle one and a fast one which acts more like a clipper. These limiters
and the pre-gain going into them are configured based on the detected loudness. If the track is still
too far from the target it will run the process again a configurable number of times, by default a
maximum of twice. If the dynamic range is still too much (i.e. if the integrated loudness is too far from
the upper-percentile momentary loudness), it can enable `dynaudnorm` on subsequent iterations, which is a
more-complex form of compression which occasionally introduces some artifacts which sound like someone
adjusting the volume on the track as it plays. By default it will never enable `dynaudnorm`, but the
difference between integrated and momentary loudness at which it will enable it for the next iteration
is configurable. If the result of an iteration is even worse from the target than the previous iteration,
it will discard the most-recent result and use the previous result as the output. This also goes for if
the original track is closer to the target than the processed track.

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

_This tool is not intended to be used to automatically master unmastered tracks_, however, 
