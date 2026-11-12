import Ggwave.Basic
import Ggwave.Fft

/-!
Windowed-sinc resampler, ported from `GGWave::Resampler` by way of
`src/resampler.rs` in the Rust crate, so that output at a non-native rate
matches C ggwave sample for sample. It sums in double precision, as C does,
and rounds to `float` where C stores one.
-/

namespace Ggwave

def Resampler.width : Nat := 64
def Resampler.delaySize : Nat := 140
def Resampler.samplesPerZeroCrossing : Nat := 32

open Resampler in
/-- The windowed sinc, sampled `samplesPerZeroCrossing` times per zero crossing, in `float`. -/
def Resampler.sincTable : FloatArray := Id.run do
  let n := width * samplesPerZeroCrossing
  let winFreq := pi / width.toFloat / samplesPerZeroCrossing.toFloat
  let mut t := (FloatArray.emptyWithCapacity n).push 1
  for i in [1:n] do
    let temp := i.toFloat * pi / samplesPerZeroCrossing.toFloat
    let s := roundF32 (temp.sin / temp)
    let win := 0.5 + 0.5 * (winFreq * i.toFloat).cos
    t := t.push (roundF32 (s * win))
  return t

/-- Resampler state between calls. -/
structure Resampler where
  delayBuffer : FloatArray := zeros (3 * Resampler.width)
  edgeSamples : FloatArray := zeros Resampler.width
  nSamplesTotal : Int := 0
  timeInt : Int := 0
  timeLast : Int := 0
  timeNow : Float := 0
  deriving Inhabited

namespace Resampler

/-- The initial state; `reset` returns to it. -/
def new : Resampler := {}

def reset (_ : Resampler) : Resampler := new

def sinc (x : Float) : Float :=
  if x.abs >= (width - 1).toFloat then 0 else
    let temp := x.abs * samplesPerZeroCrossing.toFloat
    let low := temp.toUSize
    let delta := temp - low.toFloat
    let a := fget sincTable low
    let b := fget sincTable (low + 1)
    a + (b - a) * delta

/-- Shifts the delay line down by one from `i` on and puts `d` at its end. -/
def shift (db : FloatArray) (d : Float) (i : Nat) : FloatArray :=
  if i < delaySize - 5 then shift (db.set! i db[i + 1]!) d (i + 1)
  else db.set! (delaySize - 5) d
termination_by delaySize - 5 - i

/-- `acc` plus `n` sinc-weighted delay-line samples from `db[i]` on, the first
at time `j`, summed in C's order. The loop counts in `USize` and `Float`
because `Nat` arithmetic made it twenty times slower. -/
def window (db : FloatArray) (up : Bool) (oneOverFactor timeNow : Float) (i : USize) (j : Float) :
    Nat → Float → Float
  | 0, acc => acc
  | n + 1, acc =>
    let data := fget db i
    let term := if up then data * sinc (timeNow - j)
      else data * oneOverFactor * sinc (oneOverFactor * (timeNow - j))
    window db up oneOverFactor timeNow (i + 1) (j + 1) n (acc + term)

/-- Resamples `input` by `factor` (input rate / output rate), appending to
`out`. Output lags input by `width` samples, as in C. `input` must be longer
than `width`, and `factor` at least 1/96 (rates of 1 to 96 kHz). -/
def resample (r : Resampler) (factor : Float32) (input out : FloatArray) : Resampler × FloatArray := Id.run do
  let n := input.size
  let samplesInp := pushSlice r.edgeSamples input 0 n
  let nSamplesTotal := r.nSamplesTotal + n
  let factorD := factor.toFloat
  let oneOverFactor := 1 / factorD
  let mut db := r.delayBuffer
  let mut timeInt := r.timeInt
  let mut timeLast := r.timeLast
  let mut timeNow := r.timeNow
  let mut idx := 0
  let mut out := out
  -- each pass moves timeNow on by factor >= 1/96 and stops once n samples are consumed
  for _ in [0:97 * (n + 2)] do
    -- C's inner loop: pull input samples until timeLast catches up with timeInt
    let mut fed := true
    for _ in [0:n + 1] do
      if timeLast >= timeInt then break
      if idx >= n then
        fed := false
        break
      db := shift db samplesInp[idx]! 0
      idx := idx + 1
      timeLast := timeLast + 1
    if !fed then break

    let leftLimit := max 0 (truncInt (timeNow - width.toFloat + 1))
    let rightLimit := min (truncInt (timeNow + width.toFloat)) (nSamplesTotal + width)
    -- C reads delay-line sample j - timeInt + width for each j in [leftLimit, rightLimit)
    let first := (leftLimit - timeInt + width).toNat.toUSize
    let sum := window db (factor < 1) oneOverFactor timeNow first (Float.ofInt leftLimit)
      (rightLimit - leftLimit).toNat 0
    out := out.push (roundF32 sum)

    timeNow := timeNow + factorD
    timeLast := timeInt
    timeInt := truncInt timeNow
  return ({ delayBuffer := db, edgeSamples := slice input (n - width) n, nSamplesTotal,
            timeInt, timeLast, timeNow }, out)

end Resampler

end Ggwave
