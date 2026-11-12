import Ggwave.Basic
import Ggwave.Fft
import Ggwave.Protocol

/-!
Tone generation: the per-bit sine frames and the amplitude envelope the
encoder sums them with (the helpers of `src/encoder.rs` in the Rust crate).
Arithmetic is `Float32` wherever C ggwave computes in `float`, so the samples
match C's.
-/

namespace Ggwave

/-- `GGWave::bitFreq`: float arithmetic, widened to double at the end. -/
def Config.bitFreq (c : Config) (p : ProtocolParams) (bit : Nat) : Float :=
  (c.hzPerSample * p.freqStart.toFloat32 + c.freqDeltaHz * bit.toFloat32).toFloat

/-- One frame of a sine at `freq` Hz starting at `phase`, rounded to `float` as C stores it. -/
def Config.tone (c : Config) (freq phase : Float) : Samples c.spf :=
  let curIhz := 1 / c.hzPerSample.toFloat
  let isf := c.isamplesPerFrame.toFloat
  Samples.ofFn c.spf fun i => roundF32 ((2 * pi) * (i.toFloat * isf) * (freq * curIhz) + phase).sin

/-- Adds `src` scaled by `scalar` to `dst`, with a linear fade over the first
and last 15% of an `nPerCycle`-frame symbol; `cycleMod` is the frame's place in it. -/
def addAmplitudeSmooth (src dst : Samples n) (scalar : Float32) (cycleMod nPerCycle : Nat) : Samples n :=
  let nTotal := (nPerCycle * n).toFloat32
  let frac : Float32 := 0.15
  let ids := 1 / (frac * nTotal)
  let nBegin := (frac * nTotal).floor
  let nEnd := ((1 - frac) * nTotal).floor
  dst.mapIdx fun i d =>
    let k := (cycleMod * n + i).toFloat32
    let s := (src.get i).toFloat32
    let d := d.toFloat32
    let v :=
      if k < nBegin then d + scalar * s * (k * ids)
      else if k > nEnd then d + scalar * s * ((nTotal - k) * ids)
      else d + scalar * s
    v.toFloat

end Ggwave
