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

structure ProtocolParams where
  /-- FFT bin index of the lowest frequency. -/
  freqStart : Nat
  /-- Number of frames used to transmit one chunk of data. -/
  framesPerTx : Nat
  /-- Number of bytes in one chunk of data. -/
  bytesPerTx : Nat
  /-- 2 for mono-tone protocols, 1 otherwise. -/
  extra : Nat
  deriving Repr, Inhabited

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
def params (p : Protocol) : ProtocolParams :=
  let (freqStart, framesPerTx, bytesPerTx, extra) := match p with
    | audibleNormal => (40, 9, 3, 1)
    | audibleFast => (40, 6, 3, 1)
    | audibleFastest => (40, 3, 3, 1)
    | ultrasoundNormal => (320, 9, 3, 1)
    | ultrasoundFast => (320, 6, 3, 1)
    | ultrasoundFastest => (320, 3, 3, 1)
    | dtNormal => (24, 9, 1, 1)
    | dtFast => (24, 6, 1, 1)
    | dtFastest => (24, 3, 1, 1)
    | mtNormal => (24, 9, 1, 2)
    | mtFast => (24, 6, 1, 2)
    | mtFastest => (24, 3, 1, 2)
  { freqStart, framesPerTx, bytesPerTx, extra }

/-- Mono-tone protocols (MT) only work with fixed-length payloads. -/
def isMonoTone (p : Protocol) : Bool := p.params.extra == 2

end Protocol

instance : ToString Protocol := ⟨Protocol.name⟩

end Ggwave
