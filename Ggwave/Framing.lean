import Ggwave.Basic
import Ggwave.Protocol
import Ggwave.ReedSolomon

/-!
Framing and symbols (the first half of `GGWave::encode`; inline in
`src/encoder.rs` of the Rust crate): a payload becomes the coded bytes sent on
air, and the coded bytes become one tone choice per tone group per Tx.
-/

namespace Ggwave

open ReedSolomon (eccBytesForLength)

/-- Coded bytes for `len` data bytes: the length header (variable-length mode
only), the data, and its parity. -/
def Config.codedLength (c : Config) (len : Nat) : Nat :=
  c.encodedDataOffset + len + eccBytesForLength len

/-- The length header: `len` and its two parity bytes, or nothing in fixed-length mode. -/
def Config.lengthHeader (c : Config) (len : Nat) : Vector UInt8 c.encodedDataOffset :=
  if h : c.fixedLen.isSome then
    (#v[] : Vector UInt8 0).cast (by simp [Config.encodedDataOffset, h])
  else
    let block := ReedSolomon.encode .lengthHeader #v[⟨len.toUInt8⟩]
    (block.map (·.val)).cast (by simp [Config.encodedDataOffset, h, encodedDataOffsetVariable]; rfl)

/-- The coded bytes for `data`: length header, data, parity. -/
def Config.frame (c : Config) (data : Vector UInt8 len) (h : len ≤ maxLengthVariable) :
    Vector UInt8 (c.codedLength len) :=
  let block := ReedSolomon.encode (.payload len h) (data.map (⟨·⟩))
  (c.lengthHeader len ++ block.map (·.val)).cast (by
    simp [Config.codedLength, ReedSolomon.Code.payload, Nat.add_assoc])

/-- One of the 16 tones of a group. -/
abbrev Tone := Fin 16

/-- Nibble `hi` (0 low, 1 high) of `b`. -/
def nibble (b : UInt8) (hi : Bool) : Tone :=
  ⟨(if hi then b.toNat / 16 else b.toNat) % 16, Nat.mod_lt _ (by decide)⟩

namespace ProtocolParams

/-- The tones of each Tx, one per tone group. Groups `2j` and `2j + 1` carry
the low and high nibble of the Tx's byte `j`; a mono-tone Tx carries one
nibble, low then high over two Txs. Bytes past the end read as zero, as C
reads its zeroed buffer. -/
def symbols (p : ProtocolParams) (coded : Vector UInt8 m) :
    Vector (Vector Tone p.tonesPerTx) (p.txCount m) :=
  Vector.ofFn fun t => Vector.ofFn fun g =>
    if p.extra == 1 then nibble (coded[t.val * p.bytesPerTx + g.val / 2]?.getD 0) (g.val % 2 == 1)
    else nibble (coded[t.val / 2]?.getD 0) (t.val % 2 == 1)

/-- C's index of tone `v` of group `g`: the data bit it sounds, whose frequency
is `bitFreq (k / 2)`, one bin higher when `k` is odd. -/
def toneBit (p : ProtocolParams) (g : Fin p.tonesPerTx) (v : Tone) : Nat :=
  g * p.extra * 16 + v

end ProtocolParams

end Ggwave
