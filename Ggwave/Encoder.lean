import Ggwave.Basic
import Ggwave.Framing
import Ggwave.Protocol
import Ggwave.Resampler
import Ggwave.Tone

/-!
Payload to waveform (`src/encoder.rs` in the Rust crate). `encodeWith` takes
a payload whose length is known to fit and cannot fail; `encode` checks
untrusted input and calls it.
-/

namespace Ggwave

/-- Volume, 0 to 100. -/
abbrev Volume := Fin 101

/-- Payload lengths `c` can send with protocol `p`: 1 to the mode's limit, and
mono-tone protocols only in fixed-length mode. -/
def Config.Sendable (c : Config) (p : Protocol) (n : Nat) : Prop :=
  0 < n ∧ n ≤ c.maxLength ∧ (p.isMonoTone = true → c.fixedLen.isSome = true)

instance (c : Config) (p : Protocol) (n : Nat) : Decidable (c.Sendable p n) :=
  inferInstanceAs (Decidable (_ ∧ _ ∧ _))

/-- The frames of a transmission at the operating rate, before any resampling:
start marker, data, end marker. -/
def encodeFrames (c : Config) (protocol : Protocol) (volume : Volume) (payload : Vector UInt8 n)
    (h : c.Sendable protocol n) : Array (Samples c.spf) := Id.run do
  let p := protocol.params
  let len := c.dataLength n
  let data : Vector UInt8 len := Vector.ofFn fun i =>
    (payload[i.val]?.getD 0) ^^^ if c.dss then dssMagic i else 0
  let coded := c.frame data (c.dataLength_le h.2.1)
  let symbols := p.symbols coded
  let sendVolume := (volume.val.toFloat / 100).toFloat32

  let bpt := p.bytesPerTx
  let nBits := max (16 * bpt) bitsInMarker
  let mut bit1 : Array (Samples c.spf) := #[]
  let mut bit0 : Array (Samples c.spf) := #[]
  for k in [0:nBits] do
    let freq := c.bitFreq p k
    let phase := (pi * k.toFloat) / (8 * bpt).toFloat
    bit1 := bit1.push (c.tone freq phase)
    bit0 := bit0.push (c.tone (freq + c.hzPerSample.toFloat) phase)

  let nm := c.nMarkerFrames
  let dataFrames := p.dataFrames (c.codedLength len)
  let fpt := p.framesPerTx
  let mut frames := Array.emptyWithCapacity (2 * nm + dataFrames)
  for frameId in [0:2 * nm + dataFrames] do
    let mut output := Samples.zeros c.spf
    let mut nFreq := bitsInMarker
    if frameId < nm then
      for i in [0:bitsInMarker] do
        let src := if i % 2 == 0 then bit1[i]! else bit0[i]!
        output := addAmplitudeSmooth src output sendVolume frameId nm
    else if frameId < nm + dataFrames then
      let d := frameId - nm
      let tones := symbols[d / fpt]!
      nFreq := p.tonesPerTx
      for h : g in [0:p.tonesPerTx] do
        let k := p.toneBit ⟨g, h.upper⟩ tones[g]
        let src := if k % 2 == 1 then bit0[k / 2]! else bit1[k / 2]!
        output := addAmplitudeSmooth src output sendVolume (d % fpt) fpt
    else
      let fId := frameId - (nm + dataFrames)
      for i in [0:bitsInMarker] do
        let src := if i % 2 == 0 then bit0[i]! else bit1[i]!
        output := addAmplitudeSmooth src output sendVolume fId nm
    let scale : Float32 := 1 / (max nFreq 1).toFloat32
    frames := frames.push (output.mapIdx fun _ x => (x.toFloat32 * scale).toFloat)
  return frames

/-- The waveform for `payload` at `sampleRateOut`: the frames of `encodeFrames`,
resampled as C does when a rate differs from the operating rate. -/
def encodeWith (c : Config) (protocol : Protocol) (volume : Volume) (payload : Vector UInt8 n)
    (h : c.Sendable protocol n) : FloatArray := Id.run do
  let frames := encodeFrames c protocol volume payload h
  if c.needResampling then
    let factor := c.sampleRate / c.sampleRateOut
    let mut r := Resampler.new
    let mut out := FloatArray.emptyWithCapacity (frames.size * c.spf)
    for f in frames do
      (r, out) := r.resample factor f.1 out
    return out
  return frames.foldl (init := FloatArray.emptyWithCapacity (frames.size * c.spf))
    fun out f => pushSlice out f.1 0 c.spf

/-- Encodes `payload` with `protocol` at `volume` (0 to 100; C ggwave's default
is 10) and returns mono samples in -1..1 at `sampleRateOut`: the samples C
ggwave produces for `GGWAVE_SAMPLE_FORMAT_F32`, each exactly a `float`.

In fixed-length mode a shorter payload is zero-padded, as in C. -/
def encode (params : Parameters) (protocol : Protocol) (volume : Nat) (payload : ByteArray) :
    Except Error FloatArray := do
  let c ← Config.new params
  if hv : volume ≤ 100 then
    if payload.size == 0 then throw .emptyPayload
    if payload.size > c.maxLength then throw (.payloadTooLong payload.size c.maxLength)
    if h : c.Sendable protocol payload.size then
      return encodeWith c protocol ⟨volume, by omega⟩ ⟨payload.data, rfl⟩ h
    throw .monoToneNeedsFixedLength
  throw (.invalidVolume volume)

/-- Converts samples to 16-bit the way C ggwave produces
`GGWAVE_SAMPLE_FORMAT_I16` output (`32768 * x`, truncated). -/
def toI16 (samples : FloatArray) : Array Int16 :=
  samples.foldl (fun a x => a.push (32768 * x).toInt32.toInt16) (Array.emptyWithCapacity samples.size)

end Ggwave
