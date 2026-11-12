/-!
The twelve built-in protocols (`src/protocol.rs` in the Rust crate).
-/

namespace Ggwave

/-- A ggwave transmission protocol. Frequencies assume the default 48 kHz /
1024 samples per frame (46.875 Hz per bin). `Protocol.id` gives the C
`ggwave_ProtocolId`. -/
inductive Protocol where
  /-- Audible, base 1875 Hz, 9 frames per 3 bytes. -/
  | audibleNormal
  /-- Audible, base 1875 Hz, 6 frames per 3 bytes. -/
  | audibleFast
  /-- Audible, base 1875 Hz, 3 frames per 3 bytes. -/
  | audibleFastest
  /-- Ultrasound, base 15000 Hz, 9 frames per 3 bytes. -/
  | ultrasoundNormal
  /-- Ultrasound, base 15000 Hz, 6 frames per 3 bytes. -/
  | ultrasoundFast
  /-- Ultrasound, base 15000 Hz, 3 frames per 3 bytes. -/
  | ultrasoundFastest
  /-- Dual-tone, base 1125 Hz, 9 frames per byte. -/
  | dtNormal
  /-- Dual-tone, base 1125 Hz, 6 frames per byte. -/
  | dtFast
  /-- Dual-tone, base 1125 Hz, 3 frames per byte. -/
  | dtFastest
  /-- Mono-tone, base 1125 Hz, 9 frames per nibble (fixed length only). -/
  | mtNormal
  /-- Mono-tone, base 1125 Hz, 6 frames per nibble (fixed length only). -/
  | mtFast
  /-- Mono-tone, base 1125 Hz, 3 frames per nibble (fixed length only). -/
  | mtFastest
  deriving Repr, DecidableEq, Inhabited

/-- A protocol's constants, from ggwave.h `Protocols::kDefault()`, with the
facts the framing relies on. -/
structure ProtocolParams where
  /-- FFT bin index of the lowest frequency. -/
  freqStart : Nat
  /-- Number of frames used to transmit one chunk of data (a Tx). -/
  framesPerTx : Nat
  /-- Number of bytes in one chunk of data. -/
  bytesPerTx : Nat
  /-- 2 for mono-tone protocols, which spend two Txs on each byte, 1 otherwise. -/
  extra : Nat
  framesPerTx_pos : 0 < framesPerTx
  bytesPerTx_pos : 0 < bytesPerTx
  /-- Mono-tone protocols send one nibble per Tx. -/
  extra_cases : extra = 1 ∨ (extra = 2 ∧ bytesPerTx = 1)

namespace ProtocolParams

/-- Tone groups sounded in each Tx (`nTones` in C): one per nibble sent. -/
def tonesPerTx (p : ProtocolParams) : Nat := 2 * p.bytesPerTx / p.extra

/-- Txs needed for `bytes` coded bytes. -/
def txCount (p : ProtocolParams) (bytes : Nat) : Nat :=
  p.extra * ((bytes + p.bytesPerTx - 1) / p.bytesPerTx)

/-- Data frames needed for `bytes` coded bytes. -/
def dataFrames (p : ProtocolParams) (bytes : Nat) : Nat := p.txCount bytes * p.framesPerTx

end ProtocolParams

namespace Protocol

/-- Every protocol, in C protocol-id order. -/
def all : List Protocol :=
  [audibleNormal, audibleFast, audibleFastest, ultrasoundNormal, ultrasoundFast,
   ultrasoundFastest, dtNormal, dtFast, dtFastest, mtNormal, mtFast, mtFastest]

/-- The C `ggwave_ProtocolId`. -/
def id : Protocol → Nat
  | audibleNormal => 0 | audibleFast => 1 | audibleFastest => 2
  | ultrasoundNormal => 3 | ultrasoundFast => 4 | ultrasoundFastest => 5
  | dtNormal => 6 | dtFast => 7 | dtFastest => 8
  | mtNormal => 9 | mtFast => 10 | mtFastest => 11

/-- The protocol with the given C `ggwave_ProtocolId`, if it is a built-in one. -/
def ofId? (id : Nat) : Option Protocol := all[id]?

/-- The protocol name as C ggwave reports it. -/
def name : Protocol → String
  | audibleNormal => "Normal" | audibleFast => "Fast" | audibleFastest => "Fastest"
  | ultrasoundNormal => "[U] Normal" | ultrasoundFast => "[U] Fast"
  | ultrasoundFastest => "[U] Fastest"
  | dtNormal => "[DT] Normal" | dtFast => "[DT] Fast" | dtFastest => "[DT] Fastest"
  | mtNormal => "[MT] Normal" | mtFast => "[MT] Fast" | mtFastest => "[MT] Fastest"

/-- (freqStart, framesPerTx, bytesPerTx, extra) from ggwave.h `Protocols::kDefault()`. -/
def params : Protocol → ProtocolParams
  | audibleNormal => ⟨40, 9, 3, 1, by decide, by decide, by decide⟩
  | audibleFast => ⟨40, 6, 3, 1, by decide, by decide, by decide⟩
  | audibleFastest => ⟨40, 3, 3, 1, by decide, by decide, by decide⟩
  | ultrasoundNormal => ⟨320, 9, 3, 1, by decide, by decide, by decide⟩
  | ultrasoundFast => ⟨320, 6, 3, 1, by decide, by decide, by decide⟩
  | ultrasoundFastest => ⟨320, 3, 3, 1, by decide, by decide, by decide⟩
  | dtNormal => ⟨24, 9, 1, 1, by decide, by decide, by decide⟩
  | dtFast => ⟨24, 6, 1, 1, by decide, by decide, by decide⟩
  | dtFastest => ⟨24, 3, 1, 1, by decide, by decide, by decide⟩
  | mtNormal => ⟨24, 9, 1, 2, by decide, by decide, by decide⟩
  | mtFast => ⟨24, 6, 1, 2, by decide, by decide, by decide⟩
  | mtFastest => ⟨24, 3, 1, 2, by decide, by decide, by decide⟩

instance : Inhabited ProtocolParams := ⟨audibleNormal.params⟩

/-- Mono-tone protocols (MT) only work with fixed-length payloads. -/
def isMonoTone (p : Protocol) : Bool := p.params.extra == 2

end Protocol

instance : ToString Protocol := ⟨Protocol.name⟩

end Ggwave
