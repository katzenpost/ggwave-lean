import GgwaveTests.Common

/-! Module-level checks, from the Rust crate's unit tests and `tests/api.rs`. -/

open Ggwave

namespace GgwaveTests

def fft : T Unit := do
  let n := 1024
  let x := Samples.ofFn n fun i => (2 * pi * 40 * i.toFloat / n.toFloat).sin
  let s := (Fft.new n).power x
  let mut peak := 1
  for k in [1:n / 2] do
    if s.get k > s.get peak then peak := k
  check (peak == 40) s!"FFT peak at {peak}"
  -- |X_k| = n/2 for a unit sine
  check (((s.get 40).sqrt - n.toFloat / 2).abs < 0.5) "FFT magnitude"

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

-- a payload whose length is checked by the type checker: no `Except` to handle
def voucherTones (code : Vector UInt8 32) : FloatArray :=
  encodeWith Config.default .audibleFast 25 code (by decide)

def typedVoucher : T Unit := do
  let s := voucherTones (Vector.replicate 32 0x5a)
  check ((encode {} .audibleFast 25 code).toOption.map (·.toList) == some s.toList) "typed and checked encoders agree"

def unit : T Unit := do
  readmeExample
  typedVoucher
  fft
  rejectsBadInput
  halfRateHalfFrame

end GgwaveTests
