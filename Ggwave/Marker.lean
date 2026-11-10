import Ggwave.Basic
import Ggwave.Protocol
import Ggwave.Tone

/-!
Start and end marker detection (`marker_detected` in `src/decoder.rs` of the
Rust crate). A marker frame sounds the even marker bits on their "1" tone and
the odd ones on their "0" tone, one bin higher; the end marker swaps them.
-/

namespace Ggwave

/-- FFT bin of marker bit `i`'s "1" tone. -/
def Config.markerBin (c : Config) (p : ProtocolParams) (i : Nat) : Nat :=
  (c.bitFreq p i * c.ihzPerSample.toFloat).round.toUInt64.toNat

/-- True if every marker bit in `spectrum` has the start marker's tone (`start`)
or the end marker's. -/
def Config.markerDetected (c : Config) (spectrum : Samples n) (p : ProtocolParams)
    (start : Bool) : Bool :=
  (List.range bitsInMarker).all fun i =>
    let bin := c.markerBin p i
    let a := spectrum.get bin
    let b := c.threshold.toFloat * spectrum.get (bin + 1)
    if (i % 2 == 0) == start then a > b else a < b

end Ggwave
