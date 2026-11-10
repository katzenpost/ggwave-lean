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
def Config.tone (c : Config) (freq phase : Float) : FloatArray := Id.run do
  let curIhz := 1 / c.hzPerSample.toFloat
  let isf := c.isamplesPerFrame.toFloat
  let mut t := FloatArray.emptyWithCapacity c.spf
  for i in [0:c.spf] do
    t := t.push (roundF32 ((2 * pi) * (i.toFloat * isf) * (freq * curIhz) + phase).sin)
  return t

/-- Adds `src` scaled by `scalar` to `dst`, with a linear fade over the first
and last 15% of an `nPerCycle`-frame symbol; `cycleMod` is the frame's place in it. -/
def addAmplitudeSmooth (src dst : FloatArray) (scalar : Float32) (cycleMod nPerCycle : Nat) :
    FloatArray := Id.run do
  let finalId := dst.size
  let nTotal := (nPerCycle * finalId).toFloat32
  let frac : Float32 := 0.15
  let ids := 1 / (frac * nTotal)
  let nBegin := (frac * nTotal).floor
  let nEnd := ((1 - frac) * nTotal).floor
  let mut dst := dst
  for i in [0:finalId] do
    let k := (cycleMod * finalId + i).toFloat32
    let s := src[i]!.toFloat32
    let d := dst[i]!.toFloat32
    let v :=
      if k < nBegin then d + scalar * s * (k * ids)
      else if k > nEnd then d + scalar * s * ((nTotal - k) * ids)
      else d + scalar * s
    dst := dst.set! i v.toFloat
  return dst

end Ggwave
