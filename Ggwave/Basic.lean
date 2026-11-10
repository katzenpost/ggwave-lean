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

/-- Values derived from `Parameters`, named as in ggwave.cpp. -/
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
  nMarkerFrames : Nat
  encodedDataOffset : Nat
  threshold : Float32
  dss : Bool
  deriving Inhabited

def Config.new (p : Parameters) : Except Error Config := do
  let (rate, rateInp, rateOut) := (p.sampleRate.toFloat32, p.sampleRateInp.toFloat32, p.sampleRateOut.toFloat32)
  let rateOk (r : Float32) : Bool := 1000 ≤ r && r ≤ 96000
  unless rateOk rate && rateOk rateInp && rateOk rateOut do
    throw (.invalidParameters "sample rates must be 1000 to 96000 Hz")
  unless [128, 256, 512, 1024].contains p.samplesPerFrame do
    throw (.invalidParameters "samplesPerFrame must be 128, 256, 512 or 1024")
  if let some len := p.payloadLength then
    if len == 0 || len > maxLengthFixed then
      throw (.invalidParameters "payloadLength must be 1 to 64")
  let spf := p.samplesPerFrame.toFloat32
  let hzPerSample := rate / spf
  let fixed := p.payloadLength.isSome
  return {
    fixedLen := p.payloadLength
    sampleRateInp := rateInp
    sampleRateOut := rateOut
    sampleRate := rate
    spf := p.samplesPerFrame
    hzPerSample
    ihzPerSample := 1 / hzPerSample
    isamplesPerFrame := 1 / spf
    freqDeltaHz := 2 * hzPerSample
    nMarkerFrames := if fixed then 0 else markerFrames
    encodedDataOffset := if fixed then 0 else encodedDataOffsetVariable
    threshold := p.soundMarkerThreshold.toFloat32
    dss := p.dss }

def Config.maxLength (c : Config) : Nat := c.fixedLen.getD maxLengthVariable

/-- C resamples (even at factor 1) when either side's rate differs from the operating rate. -/
def Config.needResampling (c : Config) : Bool :=
  c.sampleRateOut != c.sampleRate || c.sampleRateInp != c.sampleRate

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

end Ggwave
