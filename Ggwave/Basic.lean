/-!
Parameters, errors and the values derived from them (`src/lib.rs` in the Rust
crate). C ggwave and the Rust crate compute these in `float`; here they are
`Float32` so that tone frequencies and FFT bins come out exactly as in C.
-/

namespace Ggwave

/-- Largest variable-length payload, in bytes. -/
def maxLengthVariable : Nat := 140
/-- Largest fixed-length payload, in bytes. -/
def maxLengthFixed : Nat := 64

/-- Modem parameters, mirroring `ggwave_Parameters`; the defaults are those of
`ggwave_getDefaultParameters()`.

Both ends must agree on `payloadLength`, and on `sampleRate` / `samplesPerFrame`
(their ratio sets the tone spacing). `sampleRateInp` and `sampleRateOut` are the
rates of the audio given to the decoder and returned by the encoder; when they
differ from `sampleRate` the samples are resampled as C ggwave does. -/
structure Parameters where
  /-- `none` for variable-length payloads (with start/end markers), or the fixed
  payload length in bytes (no markers; 1 to `maxLengthFixed`). -/
  payloadLength : Option Nat := none
  /-- Sample rate of audio given to `Decoder.step`. -/
  sampleRateInp : Float := 48000
  /-- Sample rate of audio returned by `encode`. -/
  sampleRateOut : Float := 48000
  /-- Operating sample rate of the modem. -/
  sampleRate : Float := 48000
  /-- Samples per frame at the operating rate: 128, 256, 512 or 1024. -/
  samplesPerFrame : Nat := 1024
  /-- Marker detection threshold. -/
  soundMarkerThreshold : Float := 3
  /-- XOR the payload with ggwave's DSS sequence (`GGWAVE_OPERATING_MODE_USE_DSS`). -/
  dss : Bool := false
  deriving Repr, Inhabited

/-- Errors from building a decoder, or from encoding. -/
inductive Error where
  /-- A `Parameters` field is out of range; the text names it. -/
  | invalidParameters (what : String)
  /-- The payload is empty. -/
  | emptyPayload
  /-- The payload is longer than the mode allows. -/
  | payloadTooLong (len max : Nat)
  /-- Volume must be 0 to 100. -/
  | invalidVolume (volume : Nat)
  /-- Mono-tone (MT) protocols need a fixed payload length. -/
  | monoToneNeedsFixedLength
  deriving Repr, DecidableEq, Inhabited

instance : ToString Error where
  toString
    | .invalidParameters what => s!"invalid parameters: {what}"
    | .emptyPayload => "empty payload"
    | .payloadTooLong len max => s!"payload of {len} bytes is longer than {max}"
    | .invalidVolume v => s!"volume {v} is not in 0..=100"
    | .monoToneNeedsFixedLength => "mono-tone protocols need a fixed payload length"

def markerFrames : Nat := 16
def encodedDataOffsetVariable : Nat := 3
def bitsInMarker : Nat := 16

/-- Valid frame sizes: powers of two from 128 to 1024. -/
def ValidFrameSize (spf : Nat) : Prop := spf = 128 ∨ spf = 256 ∨ spf = 512 ∨ spf = 1024

instance : DecidablePred ValidFrameSize := fun n =>
  inferInstanceAs (Decidable (n = 128 ∨ n = 256 ∨ n = 512 ∨ n = 1024))

/-- Valid payload modes: variable length, or a fixed length of 1 to `maxLengthFixed`. -/
def ValidFixedLen : Option Nat → Prop
  | none => True
  | some n => 0 < n ∧ n ≤ maxLengthFixed

instance : DecidablePred ValidFixedLen
  | none => inferInstanceAs (Decidable True)
  | some n => inferInstanceAs (Decidable (0 < n ∧ n ≤ maxLengthFixed))

/-- Values derived from `Parameters`, named as in ggwave.cpp, with the checks
`Parameters` passed carried as proofs where they are about lengths. -/
structure Config where
  fixedLen : Option Nat
  sampleRateInp : Float32
  sampleRateOut : Float32
  sampleRate : Float32
  spf : Nat
  hzPerSample : Float32
  ihzPerSample : Float32
  isamplesPerFrame : Float32
  freqDeltaHz : Float32
  threshold : Float32
  dss : Bool
  spf_valid : ValidFrameSize spf
  fixedLen_valid : ValidFixedLen fixedLen

namespace Config

/-- The modem at sample rate `rate` with `spf`-sample frames, sending and
receiving at that rate, in the mode `fixedLen` gives. -/
def mk' (rate : Float32) (spf : Nat) (fixedLen : Option Nat) (threshold : Float32) (dss : Bool)
    (rateInp rateOut : Float32) (hspf : ValidFrameSize spf) (hlen : ValidFixedLen fixedLen) : Config :=
  let hzPerSample := rate / spf.toFloat32
  { fixedLen, sampleRateInp := rateInp, sampleRateOut := rateOut, sampleRate := rate, spf,
    hzPerSample, ihzPerSample := 1 / hzPerSample, isamplesPerFrame := 1 / spf.toFloat32,
    freqDeltaHz := 2 * hzPerSample, threshold, dss, spf_valid := hspf, fixedLen_valid := hlen }

/-- ggwave's defaults: 48 kHz, 1024-sample frames, variable-length payloads. -/
def default : Config := mk' 48000 1024 none 3 false 48000 48000 (by decide) trivial

/-- 24 kHz with 512-sample frames: the same 46.875 Hz bins and frame length as the default. -/
def halfRate : Config := mk' 24000 512 none 3 false 24000 24000 (by decide) trivial

instance : Inhabited Config := ⟨default⟩

def new (p : Parameters) : Except Error Config := do
  let (rate, rateInp, rateOut) := (p.sampleRate.toFloat32, p.sampleRateInp.toFloat32, p.sampleRateOut.toFloat32)
  let rateOk (r : Float32) : Bool := 1000 ≤ r && r ≤ 96000
  unless rateOk rate && rateOk rateInp && rateOk rateOut do
    throw (.invalidParameters "sample rates must be 1000 to 96000 Hz")
  if hspf : ValidFrameSize p.samplesPerFrame then
    if hlen : ValidFixedLen p.payloadLength then
      return mk' rate p.samplesPerFrame p.payloadLength p.soundMarkerThreshold.toFloat32 p.dss
        rateInp rateOut hspf hlen
    else throw (.invalidParameters "payloadLength must be 1 to 64")
  else throw (.invalidParameters "samplesPerFrame must be 128, 256, 512 or 1024")

def maxLength (c : Config) : Nat := c.fixedLen.getD maxLengthVariable

/-- Start and end marker frames: none in fixed-length mode. -/
def nMarkerFrames (c : Config) : Nat := if c.fixedLen.isSome then 0 else markerFrames

/-- Bytes of the coded length header before the data: none in fixed-length mode. -/
def encodedDataOffset (c : Config) : Nat := if c.fixedLen.isSome then 0 else encodedDataOffsetVariable

/-- C resamples (even at factor 1) when either side's rate differs from the operating rate. -/
def needResampling (c : Config) : Bool :=
  c.sampleRateOut != c.sampleRate || c.sampleRateInp != c.sampleRate

/-- The payload length sent on air: a shorter payload is zero-padded in fixed-length mode. -/
def dataLength (c : Config) (n : Nat) : Nat := c.fixedLen.getD n

theorem dataLength_le (c : Config) (h : n ≤ c.maxLength) : c.dataLength n ≤ maxLengthVariable := by
  have hv := c.fixedLen_valid
  unfold dataLength
  unfold maxLength at h
  cases hf : c.fixedLen with
  | none => simp_all
  | some m => simp_all [ValidFixedLen, maxLengthFixed, maxLengthVariable]; omega

end Config

-- XORed into payloads when DSS is on (kDSSMagic in ggwave.cpp).
def dssMagicTable : ByteArray := ⟨#[
  0x96, 0x9f, 0xb4, 0xaf, 0x1b, 0x91, 0xde, 0xc5, 0x45, 0x75, 0xe8, 0x2e, 0x0f, 0x32, 0x4a, 0x5f,
  0xb4, 0x56, 0x95, 0xcb, 0x7f, 0x6a, 0x54, 0x6a, 0x48, 0xf2, 0x0b, 0x7b, 0xcd, 0xfb, 0x93, 0x6d,
  0x3c, 0x77, 0x5e, 0xc3, 0x33, 0x47, 0xc0, 0xf1, 0x71, 0x32, 0x33, 0x27, 0x35, 0x68, 0x47, 0x1f,
  0x4e, 0xac, 0x23, 0x42, 0x5f, 0x00, 0x37, 0xa4, 0x50, 0x6d, 0x48, 0x24, 0x91, 0x7c, 0xa1, 0x4e]⟩

def dssMagic (i : Nat) : UInt8 := dssMagicTable[i % 64]!

/-- XORs `data` with the DSS sequence. -/
def applyDss (data : ByteArray) : ByteArray := Id.run do
  let mut d := data
  for i in [0:d.size] do
    d := d.set! i (d[i]! ^^^ dssMagic i)
  return d

/-! Small `FloatArray` helpers missing from core. -/

/-- `n` zeros. -/
def zeros (n : Nat) : FloatArray := Id.run do
  let mut a := FloatArray.emptyWithCapacity n
  for _ in [0:n] do
    a := a.push 0
  return a

/-- Appends `src[start:stop]` to `dst`. -/
def pushSlice (dst src : FloatArray) (start stop : Nat) : FloatArray := Id.run do
  let mut d := dst
  for i in [start:stop] do
    d := d.push src[i]!
  return d

/-- `src[start:stop]` as a new array. -/
def slice (src : FloatArray) (start stop : Nat) : FloatArray :=
  pushSlice (FloatArray.emptyWithCapacity (stop - start)) src start stop

/-- `a[i]`, or 0 past the end. A `USize` index keeps hot loops free of `Nat` arithmetic. -/
@[inline] def fget (a : FloatArray) (i : USize) : Float :=
  if h : i.toNat < a.size then a.uget i h else 0

/-- Rounds to the nearest `Float32`, for values C keeps in a `float`. -/
@[inline] def roundF32 (x : Float) : Float := x.toFloat32.toFloat

/-- Truncates toward zero, as C's float-to-int conversion does for in-range values. -/
def truncInt (x : Float) : Int := x.toInt64.toInt

theorem FloatArray.size_push' (a : FloatArray) (x : Float) : (a.push x).size = a.size + 1 := by
  cases a; simp [FloatArray.push, FloatArray.size]

theorem FloatArray.size_set!' (a : FloatArray) (i : Nat) (x : Float) : (a.set! i x).size = a.size := by
  cases a; simp [FloatArray.set!, FloatArray.size]

/-- Exactly `n` samples, unboxed. -/
abbrev Samples (n : Nat) := { a : FloatArray // a.size = n }

namespace Samples

@[specialize] def pushFn (f : Nat → Float) : Nat → Nat → FloatArray → FloatArray
  | 0, _, a => a
  | k + 1, i, a => pushFn f k (i + 1) (a.push (f i))

theorem size_pushFn (f : Nat → Float) (k i : Nat) (a : FloatArray) :
    (pushFn f k i a).size = a.size + k := by
  induction k generalizing i a with
  | zero => rfl
  | succ k ih => simp only [pushFn, ih, FloatArray.size_push']; omega

/-- `f 0, ..., f (n - 1)`. -/
@[inline] def ofFn (n : Nat) (f : Nat → Float) : Samples n :=
  ⟨pushFn f n 0 (FloatArray.emptyWithCapacity n), by
    rw [size_pushFn]; simp [FloatArray.emptyWithCapacity, FloatArray.size]⟩

@[specialize] def setFn (f : Nat → Float → Float) : Nat → Nat → FloatArray → FloatArray
  | 0, _, a => a
  | k + 1, i, a => setFn f k (i + 1) (a.set! i (f i a[i]!))

theorem size_setFn (f : Nat → Float → Float) (k i : Nat) (a : FloatArray) :
    (setFn f k i a).size = a.size := by
  induction k generalizing i a with
  | zero => rfl
  | succ k ih => simp only [setFn, ih, FloatArray.size_set!']

/-- `f i s[i]` at each index `i`, in place when `s` is not shared. -/
@[inline] def mapIdx (s : Samples n) (f : Nat → Float → Float) : Samples n :=
  ⟨setFn f n 0 s.1, by rw [size_setFn]; exact s.2⟩

def zeros (n : Nat) : Samples n := ofFn n fun _ => 0

instance : Inhabited (Samples n) := ⟨zeros n⟩

/-- `n` samples of `a` from `start`, zero past its end. -/
def slice (a : FloatArray) (start n : Nat) : Samples n := ofFn n fun i => a[start + i]!

@[inline] def get (s : Samples n) (i : Nat) : Float := s.1[i]!

end Samples

end Ggwave
