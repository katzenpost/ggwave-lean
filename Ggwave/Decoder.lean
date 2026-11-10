import Ggwave.Basic
import Ggwave.Fft
import Ggwave.Marker
import Ggwave.Protocol
import Ggwave.ReedSolomon
import Ggwave.Resampler

/-!
Streaming receiver as a pure state machine (`src/decoder.rs` in the Rust
crate): `Decoder.step` takes the state and a block of samples and returns the
next state and any payload completed by that block.
-/

namespace Ggwave

def maxSpectrumHistory : Nat := 4
def maxRecordedFrames : Nat := 2048
def stepsPerFrame : Nat := 16
-- input samples fed to the resampler per call; must exceed its 64-sample width
def resampleChunk : Nat := 1024

open ReedSolomon (eccBytesForLength)

/-- Receiver state. Use each state once (pass it to `Decoder.step` and keep
only the result) so that its buffers are updated in place. -/
structure DecoderState where
  cfg : Config
  protocols : Array ProtocolParams
  fft : Fft
  spectrum : FloatArray
  pending : FloatArray
  resampler : Option Resampler
  resampleInput : FloatArray
  encoded : ByteArray
  -- variable-length state
  receiving : Bool := false
  analyzing : Bool := false
  markerFreqStart : Nat := 0
  recvDurationFrames : Int := 0
  framesLeftToRecord : Int := 0
  framesToRecord : Int := 0
  historyId : Nat := 0
  history : Array FloatArray
  recorded : FloatArray := .empty
  -- fixed-length state
  minFreqStart : Nat
  historyFixed : Array ByteArray
  historyIdFixed : Nat := 0
  deriving Inhabited

def maxFramesPerTx (protocols : Array ProtocolParams) (excludeMt : Bool) : Nat :=
  protocols.foldl (init := 0) fun m p =>
    if excludeMt && p.extra > 1 then m else max m (p.framesPerTx * p.extra)

namespace Decoder

/-- A receiver listening for `protocols` (C's `ggwave_rxToggleProtocol`);
`Protocol.all` listens for every one. -/
def init (params : Parameters) (protocols : List Protocol) : Except Error DecoderState := do
  let cfg ← Config.new params
  let spf := cfg.spf
  -- C keeps protocol order and reads garbage for bins past the frame; skip those instead
  let ps := (Protocol.all.filter protocols.contains).map Protocol.params
    |>.filter (fun pp => pp.freqStart + 32 * pp.bytesPerTx ≤ spf / 2) |>.toArray
  if ps.isEmpty then throw (.invalidParameters "no protocol fits samplesPerFrame")
  let maxLength := cfg.maxLength
  let totalLength := maxLength + eccBytesForLength maxLength
  let historyFixed := match cfg.fixedLen with
    | some _ => Array.replicate (totalLength * maxFramesPerTx ps false) (ByteArray.mk (Array.replicate spf 0))
    | none => #[]
  return {
    cfg, protocols := ps, fft := Fft.new spf, spectrum := zeros spf, pending := .empty
    resampler := if cfg.needResampling then some Resampler.new else none
    resampleInput := .empty
    encoded := ByteArray.mk (Array.replicate (totalLength + cfg.encodedDataOffset) 0)
    history := Array.replicate maxSpectrumHistory (zeros spf)
    minFreqStart := ps.foldl (init := spf) fun m p => min m p.freqStart
    historyFixed }

/-- True between a start marker and the end of its analysis. -/
def isReceiving (s : DecoderState) : Bool := s.receiving

/-- Searches the recorded frames for the payload after an end marker (or a full recording). -/
def analyze (s : DecoderState) : ByteArray × Option ByteArray := Id.run do
  let spf := s.cfg.spf
  let step := spf / stepsPerFrame
  let nm := s.cfg.nMarkerFrames
  let offset := s.cfg.encodedDataOffset
  let mut encoded := s.encoded
  for p in s.protocols do
    if p.extra == 2 || p.freqStart != s.markerFreqStart then continue
    let bpt := p.bytesPerTx
    let fpt := p.framesPerTx
    let baseBin :=
      ((s.cfg.hzPerSample * p.freqStart.toFloat32).toFloat * s.cfg.ihzPerSample.toFloat).round.toUInt64.toNat
    for iiRev in [0:nm * stepsPerFrame] do
      let ii := nm * stepsPerFrame - 1 - iiRev
      let mut knownLength := false
      let mut decodedLength := 0
      for itx in [0:1024] do
        let offsetTx := ii + itx * fpt * stepsPerFrame
        if offsetTx ≥ s.recvDurationFrames * stepsPerFrame || (itx + 1) * bpt ≥ encoded.size then
          break
        let base := offsetTx * step
        let mut fftIn := slice s.recorded base (base + spf)
        for k in [1:fpt] do
          let pos := (offsetTx + k * stepsPerFrame) * step
          for i in [0:spf] do
            fftIn := fftIn.set! i (fftIn[i]! + s.recorded[pos + i]!)
        let spectrum := s.fft.power fftIn

        let mut curByte : UInt8 := 0
        for i in [0:2 * bpt] do
          let bin := baseBin + 16 * i
          let mut kmax : UInt8 := 0
          let mut amax : Float := 0
          for k in [0:16] do
            let v := spectrum[bin + k]!
            if v > amax then
              kmax := k.toUInt8
              amax := v
          if i % 2 == 1 then
            curByte := curByte + (kmax <<< 4)
            encoded := encoded.set! (itx * bpt + i / 2) curByte
            curByte := 0
          else
            curByte := kmax

        if itx * bpt > offset && !knownLength then
          match ReedSolomon.decode (encoded.extract 0 offset) (offset - 1) with
          | some d =>
            if d[0]! > 0 && d[0]!.toNat ≤ maxLengthVariable then
              knownLength := true
              decodedLength := d[0]!.toNat
              let bytes := offset + decodedLength + eccBytesForLength decodedLength
              let frames : Int := 2 * nm + (bytes + bpt - 1) / bpt * fpt
              if s.recvDurationFrames > frames || s.recvDurationFrames < frames - 2 * nm then
                knownLength := false
                break
            else
              break
          | none => break

        let bytes := offset + decodedLength + eccBytesForLength decodedLength
        if knownLength && itx * bpt > bytes + 1 then
          break

      if knownLength then
        let ecc := eccBytesForLength decodedLength
        if let some data := ReedSolomon.decode (encoded.extract offset (offset + decodedLength + ecc)) ecc then
          return (encoded, some (if s.cfg.dss then applyDss data else data))
  return (encoded, none)

/-- One frame in variable-length mode: marker detection, recording and analysis. -/
def decodeVariable (s : DecoderState) (frame : FloatArray) : DecoderState × Option ByteArray := Id.run do
  let spf := s.cfg.spf
  let nm := s.cfg.nMarkerFrames
  let mut s := { s with
    history := s.history.set! s.historyId frame
    historyId := (s.historyId + 1) % maxSpectrumHistory }

  if s.historyId == 0 || s.receiving then
    let mut average := zeros spf
    for h in s.history do
      for i in [0:spf] do
        average := average.set! i (average[i]! + h[i]!)
    let norm := 1 / maxSpectrumHistory.toFloat
    for i in [0:spf] do
      average := average.set! i (average[i]! * norm)
    s := { s with spectrum := s.fft.power average }

  if s.framesLeftToRecord > 0 then
    -- take the 16 MB buffer out of the state so that it is written in place
    let mut recorded := s.recorded
    s := { s with recorded := .empty }
    if recorded.size == 0 then
      recorded := zeros (maxRecordedFrames * spf)
    let pos := (s.framesToRecord - s.framesLeftToRecord).toNat * spf
    for i in [0:spf] do
      recorded := recorded.set! (pos + i) frame[i]!
    s := { s with recorded, framesLeftToRecord := s.framesLeftToRecord - 1 }
    if s.framesLeftToRecord ≤ 0 then
      s := { s with analyzing := true }

  let mut result := none
  if s.analyzing then
    let (encoded, r) := analyze s
    result := r
    s := { s with
      encoded, framesToRecord := if r.isSome then 0 else -1
      receiving := false, analyzing := false, spectrum := zeros spf }

  if !s.receiving then
    if let some p := s.protocols.find? (s.cfg.markerDetected s.spectrum · true) then
      let maxLen := maxLengthVariable + eccBytesForLength maxLengthVariable
      -- minBytesPerTx() in C is always 1
      let frames : Int := 2 * nm + maxFramesPerTx s.protocols true * (maxLen + 1)
      s := { s with
        markerFreqStart := p.freqStart, receiving := true
        recvDurationFrames := frames, framesToRecord := frames, framesLeftToRecord := frames }
  else
    let ended := s.protocols.any (s.cfg.markerDetected s.spectrum · false)
    if ended && s.framesToRecord > 1 then
      s := { s with
        recvDurationFrames := s.recvDurationFrames - (s.framesLeftToRecord - 1)
        framesLeftToRecord := 1 }
  return (s, result)

/-- Index of the largest of `row[start:start+16]`, ties going to the higher bin. -/
def argmax16 (row : ByteArray) (start : Nat) : Nat := Id.run do
  let mut m := row[start]!
  let mut mb := 0
  for b in [1:16] do
    if m ≤ row[start + b]! then
      m := row[start + b]!
      mb := b
  return mb

/-- One frame in fixed-length mode: every frame's tones are kept and the last
transmission's worth is decoded by majority vote. -/
def decodeFixed (s : DecoderState) (frame : FloatArray) : DecoderState × Option ByteArray := Id.run do
  let spf := s.cfg.spf
  let payloadLength := s.cfg.fixedLen.getD 0
  let spectrum := s.fft.power frame
  let mut amax : Float := 0
  for i in [max s.minFreqStart 1:spf / 2] do
    amax := if amax ≥ spectrum[i]! then amax else spectrum[i]!
  let scale := 255 / (if amax == 0 then 1 else amax)
  let mut newRow := ByteArray.emptyWithCapacity spf
  for i in [0:spf] do
    newRow := newRow.push (spectrum[i]! * scale).round.toUInt8
  let historyLen := s.historyFixed.size
  let mut s := { s with
    spectrum
    historyFixed := s.historyFixed.set! s.historyIdFixed newRow
    historyIdFixed := (s.historyIdFixed + 1) % historyLen }

  let totalLength := payloadLength + eccBytesForLength payloadLength
  for p in s.protocols do
    let binStart := p.freqStart
    let binOffset := if p.extra == 1 then 16 else 0
    let bpt := p.bytesPerTx
    let fpt := p.framesPerTx
    let extra := p.extra
    let totalTxs := extra * ((totalLength + bpt - 1) / bpt)
    let historyStart := (s.historyIdFixed + historyLen - totalTxs * fpt) % historyLen
    let mut detectedBins := ByteArray.mk (Array.replicate (2 * totalLength) 0)
    let mut detectedTones : Array Nat := Array.replicate (2 * 16 * bpt) 0
    let mut txNeededTotal := 0
    let mut txDetectedTotal := 0

    for k in [0:totalTxs] do
      if k % extra == 0 then
        detectedTones := Array.replicate (2 * 16 * bpt) 0
      for i in [0:fpt] do
        let row := s.historyFixed[(historyStart + k * fpt + i) % historyLen]!
        for j in [0:bpt] do
          let f0bin := argmax16 row (binStart + 2 * j * 16)
          let f1bin := if p.extra == 1 then argmax16 row (binStart + 2 * j * 16 + binOffset) else f0bin
          if k % extra == 0 then
            detectedTones := detectedTones.modify (2 * j * 16 + f0bin) (· + 1)
          if (k + 1) % extra == 0 then
            detectedTones := detectedTones.modify ((2 * j + 1) * 16 + f1bin) (· + 1)
      if extra > 1 && k % extra == 0 then continue
      for j in [0:bpt] do
        let byte := (k / extra) * bpt + j
        if byte ≥ totalLength then break
        txNeededTotal := txNeededTotal + 2
        for b in [0:16] do
          if detectedTones[2 * j * 16 + b]! > fpt / 2 then
            detectedBins := detectedBins.set! (2 * byte) b.toUInt8
            txDetectedTotal := txDetectedTotal + 1
          if detectedTones[(2 * j + 1) * 16 + b]! > fpt / 2 then
            detectedBins := detectedBins.set! (2 * byte + 1) b.toUInt8
            txDetectedTotal := txDetectedTotal + 1

    if txDetectedTotal.toFloat < 0.75 * txNeededTotal.toFloat then continue
    let mut encoded := s.encoded
    for j in [0:totalLength] do
      encoded := encoded.set! j ((detectedBins[2 * j + 1]! <<< 4) + detectedBins[2 * j]!)
    s := { s with encoded }
    if let some data := ReedSolomon.decode (encoded.extract 0 totalLength) (eccBytesForLength payloadLength) then
      return (s, some (if s.cfg.dss then applyDss data else data))
  return (s, none)

/-- Feeds mono samples (-1..1) at `sampleRateInp`, in blocks of any size.
Returns the next state and the payload completed by these samples, if any
(the last one, if several).

In fixed-length mode, as in C ggwave, the same transmission can be reported
again on the following frames. -/
def step (s : DecoderState) (samples : FloatArray) : DecoderState × Option ByteArray := Id.run do
  let mut pending := s.pending
  let mut s := { s with pending := .empty }
  match s.resampler with
  | some r =>
    let input := pushSlice s.resampleInput samples 0 samples.size
    let factor := s.cfg.sampleRateInp / s.cfg.sampleRate
    let chunks := input.size / resampleChunk
    let mut r := r
    for ci in [0:chunks] do
      -- C resets the resampler about once a minute while idle
      if !s.receiving && Float32.ofInt r.nSamplesTotal > 60 * factor * s.cfg.sampleRate then
        r := r.reset
      let (r', p') := r.resample factor (slice input (ci * resampleChunk) ((ci + 1) * resampleChunk)) pending
      r := r'
      pending := p'
    s := { s with resampler := some r, resampleInput := slice input (chunks * resampleChunk) input.size }
  | none => pending := pushSlice pending samples 0 samples.size

  let spf := s.cfg.spf
  let frames := pending.size / spf
  let mut result := none
  for f in [0:frames] do
    let frame := slice pending (f * spf) ((f + 1) * spf)
    let (s', r) := if s.cfg.fixedLen.isSome then decodeFixed s frame else decodeVariable s frame
    s := s'
    if r.isSome then result := r
  return ({ s with pending := slice pending (frames * spf) pending.size }, result)

end Decoder

end Ggwave
