import Ggwave.Basic
import Ggwave.Protocol
import Ggwave.ReedSolomon
import Ggwave.Resampler
import Ggwave.Tone

/-!
Payload to waveform (`src/encoder.rs` in the Rust crate).
-/

namespace Ggwave

/-- Encodes `payload` with `protocol` at `volume` (0 to 100; C ggwave's default
is 10) and returns mono samples in -1..1 at `sampleRateOut`: the samples C
ggwave produces for `GGWAVE_SAMPLE_FORMAT_F32`, each exactly a `float`.

In fixed-length mode a shorter payload is zero-padded, as in C. -/
def encode (params : Parameters) (protocol : Protocol) (volume : Nat) (payload : ByteArray) :
    Except Error FloatArray := do
  let c ← Config.new params
  if volume > 100 then throw (.invalidVolume volume)
  if payload.size == 0 then throw .emptyPayload
  if payload.size > c.maxLength then throw (.payloadTooLong payload.size c.maxLength)
  let p := protocol.params
  if p.extra == 2 && c.fixedLen.isNone then throw .monoToneNeedsFixedLength

  let dataLength := c.fixedLen.getD payload.size
  let sendVolume := (volume.toFloat / 100).toFloat32
  let mut data := ByteArray.emptyWithCapacity dataLength
  for i in [0:dataLength] do
    data := data.push (payload[i]?.getD 0)
  if c.dss then data := applyDss data

  let mut encoded := ByteArray.empty
  if c.fixedLen.isNone then
    encoded := ReedSolomon.encode ⟨#[dataLength.toUInt8]⟩ (c.encodedDataOffset - 1)
  encoded := encoded ++ ReedSolomon.encode data (ReedSolomon.eccBytesForLength dataLength)

  let bpt := p.bytesPerTx
  let fpt := p.framesPerTx
  let nTxs := (encoded.size + bpt - 1) / bpt
  -- the last chunk reads past the coded bytes; C reads zeroed buffer space there
  encoded := encoded ++ ByteArray.mk (Array.replicate (nTxs * bpt - encoded.size) 0)
  let totalDataFrames := p.extra * nTxs * fpt

  let spf := c.spf
  let nBits := max (16 * bpt) bitsInMarker
  let mut bit1 : Array FloatArray := #[]
  let mut bit0 : Array FloatArray := #[]
  for k in [0:nBits] do
    let freq := c.bitFreq p k
    let phase := (pi * k.toFloat) / (8 * bpt).toFloat
    bit1 := bit1.push (c.tone freq phase)
    bit0 := bit0.push (c.tone (freq + c.hzPerSample.toFloat) phase)

  let nm := c.nMarkerFrames
  let mut resampler := if c.needResampling then some Resampler.new else none
  let factor := c.sampleRate / c.sampleRateOut
  let nFrames := 2 * nm + totalDataFrames
  let mut out := FloatArray.emptyWithCapacity (nFrames * spf)

  for frameId in [0:nFrames] do
    let mut output := zeros spf
    let mut nFreq := bitsInMarker
    if frameId < nm then
      for i in [0:bitsInMarker] do
        let src := if i % 2 == 0 then bit1[i]! else bit0[i]!
        output := addAmplitudeSmooth src output sendVolume frameId nm
    else if frameId < nm + totalDataFrames then
      let d := frameId - nm
      let cycleMod := d % fpt
      let offset := (d / fpt) * bpt
      let mut bits := Array.replicate (2 * 16 * bpt) false
      for j in [0:bpt] do
        if p.extra == 1 then
          let b := encoded[offset + j]!.toNat
          bits := (bits.set! (2 * j * 16 + b % 16) true).set! ((2 * j + 1) * 16 + b / 16) true
        else
          let b := encoded[offset / 2 + j]!.toNat
          let nibble := if offset % 2 == 0 then b % 16 else b / 16
          bits := bits.set! (2 * j * 16 + nibble) true
      nFreq := 0
      for k in [0:bits.size] do
        if bits[k]! then
          nFreq := nFreq + 1
          let src := if k % 2 == 1 then bit0[k / 2]! else bit1[k / 2]!
          output := addAmplitudeSmooth src output sendVolume cycleMod fpt
    else
      let fId := frameId - (nm + totalDataFrames)
      for i in [0:bitsInMarker] do
        let src := if i % 2 == 0 then bit0[i]! else bit1[i]!
        output := addAmplitudeSmooth src output sendVolume fId nm

    let scale : Float32 := 1 / (max nFreq 1).toFloat32
    for i in [0:spf] do
      output := output.set! i (output[i]!.toFloat32 * scale).toFloat
    match resampler with
    | some r =>
      let (r, o) := r.resample factor output out
      resampler := some r
      out := o
    | none => out := pushSlice out output 0 spf
  return out

/-- Converts samples to 16-bit the way C ggwave produces
`GGWAVE_SAMPLE_FORMAT_I16` output (`32768 * x`, truncated). -/
def toI16 (samples : FloatArray) : Array Int16 :=
  samples.foldl (fun a x => a.push (32768 * x).toInt32.toInt16) (Array.emptyWithCapacity samples.size)

end Ggwave
