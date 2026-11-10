import Ggwave

/-! Test helpers: a failure counter, payloads, noise, sample formats and decoding loops. -/

open Ggwave

namespace Tests

/-- Tests run with a counter of failed checks. -/
abbrev T := ReaderT (IO.Ref Nat) IO

def check (ok : Bool) (msg : String) : T Unit := do
  unless ok do
    IO.eprintln s!"FAIL: {msg}"
    (← read).modify (· + 1)

def expectOk {α ε} [ToString ε] (r : Except ε α) (what : String) : T (Option α) := do
  match r with
  | .ok a => return some a
  | .error e =>
    check false s!"{what}: {e}"
    return none

/-- Deterministic test payload of `len` bytes, as in ggwave-rs `tests/common`. -/
def payload (len seed : Nat) : ByteArray :=
  ⟨(Array.range len).map fun i => (((i * 37 + seed) % 256).toUInt8 ^^^ 0x5a)⟩

def hex (b : ByteArray) : String :=
  b.foldl (init := "") fun s x =>
    let d := fun (n : UInt8) => Char.ofNat (if n < 10 then 48 + n.toNat else 87 + n.toNat)
    (s.push (d (x >>> 4))).push (d (x &&& 15))

def unhex (s : String) : ByteArray := Id.run do
  let v := fun (c : Char) => if c.isDigit then c.toNat - '0'.toNat else c.toLower.toNat - 'a'.toNat + 10
  let cs := s.toList.toArray
  let mut b := ByteArray.empty
  for i in [0:cs.size / 2] do
    b := b.push (v cs[2 * i]! * 16 + v cs[2 * i + 1]!).toUInt8
  return b

def params (fixed : Option Nat) (rateInp rateOut : Float) (rate : Float := 48000)
    (spf : Nat := 1024) (dss := false) : Parameters :=
  { payloadLength := fixed, sampleRateInp := rateInp, sampleRateOut := rateOut,
    sampleRate := rate, samplesPerFrame := spf, dss }

/-- Adds uniform noise in +-level/2 from a fixed seed (xorshift64), clamped to -1..1. -/
def addNoise (samples : FloatArray) (level : Float) (seed : UInt64) : FloatArray := Id.run do
  let mut x := seed ||| 1
  let mut out := samples
  for i in [0:samples.size] do
    x := x ^^^ (x <<< 13)
    x := x ^^^ (x >>> 7)
    x := x ^^^ (x <<< 17)
    let r := (x >>> 40).toFloat / 16777216
    out := out.set! i (max (-1) (min 1 (out[i]! + (r - 0.5) * level)))
  return out

/-- Silence before and after, as a real recording would have. -/
def pad (samples : FloatArray) (lead tail : Nat) : FloatArray :=
  pushSlice (pushSlice (zeros lead) samples 0 samples.size) (zeros tail) 0 tail

/-- Protocols usable in the given mode (MT needs fixed length). -/
def protocols (fixed : Bool) : List Protocol :=
  Protocol.all.filter fun p => fixed || !p.isMonoTone

/-- Feeds `samples` in blocks of `chunk` and collects every payload. -/
def decodeAll (s : DecoderState) (samples : FloatArray) (chunk : Nat) : Array ByteArray := Id.run do
  let mut s := s
  let mut got := #[]
  for c in [0:(samples.size + chunk - 1) / chunk] do
    let (s', r) := Decoder.step s (slice samples (c * chunk) (min samples.size ((c + 1) * chunk)))
    s := s'
    if let some r := r then got := got.push r
  return got

def maxAbsDiff (a b : FloatArray) : Float := Id.run do
  let mut m := 0
  for i in [0:min a.size b.size] do
    m := max m (a[i]! - b[i]!).abs
  return m

def f32Bytes (s : FloatArray) : ByteArray :=
  s.foldl (init := ByteArray.emptyWithCapacity (4 * s.size)) fun b x =>
    let u := x.toFloat32.toBits
    b.push u.toUInt8 |>.push (u >>> 8).toUInt8 |>.push (u >>> 16).toUInt8 |>.push (u >>> 24).toUInt8

def ofF32Bytes (b : ByteArray) : FloatArray := Id.run do
  let mut s := FloatArray.emptyWithCapacity (b.size / 4)
  for i in [0:b.size / 4] do
    let u := b[4 * i]!.toUInt32 ||| (b[4 * i + 1]!.toUInt32 <<< 8) |||
      (b[4 * i + 2]!.toUInt32 <<< 16) ||| (b[4 * i + 3]!.toUInt32 <<< 24)
    s := s.push (Float32.ofBits u).toFloat
  return s

/-- Little-endian i16 samples. -/
def ofI16Bytes (b : ByteArray) : Array Int := Id.run do
  let mut s := Array.emptyWithCapacity (b.size / 2)
  for i in [0:b.size / 2] do
    let u := b[2 * i]!.toNat + 256 * b[2 * i + 1]!.toNat
    s := s.push (if u ≥ 32768 then (u : Int) - 65536 else u)
  return s

/-- i16 samples scaled by 1/32768, as C ggwave reads them. -/
def ofI16 (s : Array Int) : FloatArray :=
  s.foldl (init := FloatArray.emptyWithCapacity s.size) fun a x => a.push (Float.ofInt x / 32768)

/-- `x` in scientific notation with three significant digits. -/
def fmt (x : Float) : String :=
  if x == 0 then "0" else
    let e := (Float.log10 x.abs).floor
    let m := (x.abs / Float.pow 10 e * 100).round.toUInt64.toNat
    let d := toString (m % 100)
    s!"{m / 100}.{if d.length < 2 then "0" ++ d else d}e{e.toInt64}"

end Tests
