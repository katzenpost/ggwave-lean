import Tests.Common

/-! Module-level checks, from the Rust crate's unit tests and `tests/api.rs`. -/

open Ggwave

namespace Tests

def reedSolomon : T Unit := do
  -- spot values from reed-solomon/gf.hpp
  check (ReedSolomon.expTable.extract 0 10 == ⟨#[1, 2, 4, 8, 0x10, 0x20, 0x40, 0x80, 0x1d, 0x3a]⟩) "RS exp table"
  check (ReedSolomon.expTable.extract 508 512 == ⟨#[0x47, 0x8e, 0x1, 0x2]⟩) "RS exp table end"
  check (ReedSolomon.logTable.extract 0 8 == ⟨#[0, 0, 1, 0x19, 2, 0x32, 0x1a, 0xc6]⟩) "RS log table"
  check (ReedSolomon.logTable[255]! == 0xaf) "RS log 255"
  -- C RS::ReedSolomon(32, 12).Encode() of this message
  let msg : ByteArray := ⟨(Array.range 32).map fun i => (i * 37 % 256).toUInt8 ^^^ 0x5a⟩
  let enc := ReedSolomon.encode msg 12
  check (enc.extract 32 44 == ⟨#[0x5e, 0xf0, 0xe5, 0x27, 0x07, 0xbb, 0xb8, 0x30, 0xbc, 0xc0, 0x5c, 0x38]⟩)
    "RS parity matches C"
  let ecc := ReedSolomon.eccBytesForLength msg.size
  let enc := ReedSolomon.encode msg ecc
  check (ReedSolomon.decode enc ecc == some msg) "RS decodes clean"
  let mut bad := enc
  for i in [0:ecc / 2] do
    bad := bad.set! (i * 7 % bad.size) (bad[i * 7 % bad.size]! ^^^ 0xa5)
  check (ReedSolomon.decode bad ecc == some msg) "RS corrects ecc/2 errors"
  let mut worse := enc
  for i in [0:ecc] do
    worse := worse.set! (i * 3) (worse[i * 3]! ^^^ 0x11)
  check (ReedSolomon.decode worse ecc != some msg) "RS does not claim to fix ecc errors"

def fft : T Unit := do
  let n := 1024
  let x : FloatArray := ⟨(Array.range n).map fun i => (2 * pi * 40 * i.toFloat / n.toFloat).sin⟩
  let s := (Fft.new n).power x
  let mut peak := 1
  for k in [1:n / 2] do
    if s[k]! > s[peak]! then peak := k
  check (peak == 40) s!"FFT peak at {peak}"
  -- |X_k| = n/2 for a unit sine
  check ((s[40]!.sqrt - n.toFloat / 2).abs < 0.5) "FFT magnitude"

def rejectsBadInput : T Unit := do
  let p : Parameters := {}
  check (encode p .audibleFast 10 .empty matches .error .emptyPayload) "empty payload"
  check (encode p .audibleFast 101 ⟨#[1]⟩ matches .error (.invalidVolume 101)) "volume"
  check (encode p .mtFast 10 ⟨#[1]⟩ matches .error .monoToneNeedsFixedLength) "MT variable"
  check (encode p .audibleFast 10 (ByteArray.mk (Array.replicate 141 0)) matches .error (.payloadTooLong 141 140))
    "too long"
  let bad := fun (p : Parameters) =>
    (encode p .audibleFast 10 ⟨#[1]⟩ matches .error _) && (Decoder.init p Protocol.all matches .error _)
  check (bad { samplesPerFrame := 1000 }) "samplesPerFrame 1000"
  check (bad { sampleRateInp := 500 }) "sampleRateInp 500"
  check (bad { payloadLength := some 65 }) "payloadLength 65"
  check (Decoder.init { samplesPerFrame := 128 } [.ultrasoundFast] matches .error _) "no protocol fits"
  check (Protocol.all.all fun p => Protocol.ofId? p.id == some p) "protocol ids"

def halfRateHalfFrame : T Unit := do
  -- 24 kHz / 512 has the same 46.875 Hz bins and frame length as 48 kHz / 1024
  let p := params none 24000 24000 (rate := 24000) (spf := 512)
  let msg := "voucher".toUTF8
  let some s ← expectOk (encode p .audibleFast 50 msg) "encode 24 kHz" | return
  let some d ← expectOk (Decoder.init p [.audibleFast]) "decoder 24 kHz" | return
  check ((Decoder.step d (pad s 3000 24000)).2 == some msg) "24 kHz / 512 round trip"

-- the README example, word for word
section Readme
def code : ByteArray := ByteArray.mk (Array.replicate 32 0x5a)

-- 131072 mono samples in -1..1 at 48 kHz, about 2.7 s
def tones : Except Error FloatArray := encode {} .audibleFast 25 code

def listen (blocks : List FloatArray) : Except Error (Option ByteArray) := do
  let mut s ← Decoder.init {} [.audibleFast]
  for block in blocks do
    let (next, payload) := Decoder.step s block
    if payload.isSome then return payload
    s := next
  return none
end Readme

def readmeExample : T Unit := do
  let some t ← expectOk tones "README encode" | return
  check (t.size == 131072) s!"README: {t.size} samples"
  -- 20 ms blocks, as an audio input callback would deliver them
  let x := pad t 4800 48000
  let blocks := (List.range ((x.size + 959) / 960)).map fun i => slice x (960 * i) (min x.size (960 * (i + 1)))
  check ((listen blocks).toOption == some (some code)) "README listen"

def unit : T Unit := do
  readmeExample
  reedSolomon
  fft
  rejectsBadInput
  halfRateHalfFrame

end Tests
