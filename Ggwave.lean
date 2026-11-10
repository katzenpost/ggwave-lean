import Ggwave.Basic
import Ggwave.Protocol
import Ggwave.ReedSolomon
import Ggwave.Fft
import Ggwave.Resampler
import Ggwave.Tone
import Ggwave.Marker
import Ggwave.Encoder
import Ggwave.Decoder

/-!
# ggwave

A port of [ggwave](https://github.com/ggerganov/ggwave), Georgi Gerganov's
data-over-sound modem, to pure Lean: `Ggwave.encode` turns a payload into
samples and `Ggwave.Decoder.step` turns samples back into payloads. Nothing
here does I/O; audio comes from and goes to the caller.
-/
