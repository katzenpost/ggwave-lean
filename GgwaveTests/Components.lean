import Lean.Data.Json
import GgwaveTests.Common

/-! Each module against the component vectors C ggwave 060aec7 produced
(`tests/vectors/<component>/`, written by `tools/ggwave-vectors.cpp`). -/

open Ggwave
open Lean (Json toJson)

namespace GgwaveTests

def componentsDir : System.FilePath := "tests" / "vectors"

def loadJson (path : System.FilePath) : IO Json := do
  match Json.parse (← IO.FS.readFile path) with
  | .ok j => return j
  | .error e => throw (.userError s!"{path}: {e}")

def _root_.Lean.Json.arrD (j : Json) (k : String) : Array Json := ((j.getObjValD k).getArr?).toOption.getD #[]
def _root_.Lean.Json.natD (j : Json) (k : String) : Nat := ((j.getObjValD k).getNat?).toOption.getD 0
def _root_.Lean.Json.strD (j : Json) (k : String) : String := ((j.getObjValD k).getStr?).toOption.getD ""
def _root_.Lean.Json.ints (j : Json) : Array Int := ((j.getArr?).toOption.getD #[]).map fun x => (x.getInt?).toOption.getD 0
/-- A hex string or `null`. -/
def _root_.Lean.Json.hexOpt (j : Json) (k : String) : Option ByteArray :=
  match j.getObjValD k with
  | .str s => some (unhex s)
  | _ => none

def fnv1a (b : ByteArray) : UInt64 :=
  b.foldl (init := 0xcbf29ce484222325) fun h x => (h ^^^ x.toUInt64) * 0x100000001b3

def hex64 (x : UInt64) : String :=
  hex ⟨(Array.range 8).reverse.map fun i => (x >>> (8 * i).toUInt64).toUInt8⟩

def reedSolomonVectors : T Unit := do
  let j ← loadJson (componentsDir / "reed_solomon" / "reed_solomon.json")
  check (GF256.expTable == unhex (j.strD "exp")) "GF(2^8) exp table"
  check (GF256.logTable == unhex (j.strD "log")) "GF(2^8) log table"
  let g := fun (n : Int) => (⟨n.toNat.toUInt8⟩ : GF256)
  for r in j.arrD "mul" do
    let v := r.ints
    check ((g v[0]! * g v[1]!) == g v[2]!) s!"GF mul {v}"
  for r in j.arrD "div" do
    let v := r.ints
    check ((g v[0]! / g v[1]!) == g v[2]!) s!"GF div {v}"
  for r in j.arrD "inverse" do
    let v := r.ints
    check (GF256.inverse (g v[0]!) == g v[1]!) s!"GF inverse {v}"
  for r in j.arrD "pow" do
    let v := r.ints
    check (GF256.pow (g v[0]!) v[1]! == g v[2]!) s!"GF pow {v}"
  let mut rejected := 0
  for kind in ["encode", "decode"] do
    for e in j.arrD kind do
      let (k, ecc) := (e.natD "msg_len", e.natD "ecc_len")
      let some c := (if h : k + ecc < 256 then some (⟨k, ecc, h⟩ : ReedSolomon.Code) else none)
        | check false s!"RS code {k}+{ecc}"
      if kind == "encode" then
        let block := ReedSolomon.encode c (ReedSolomon.ofBytes (unhex (e.strD "msg")) 0 c.msgLen)
        check (ReedSolomon.toBytes block == unhex (e.strD "block")) s!"RS encode {k}+{ecc}"
      else
        let got := (ReedSolomon.decode c (ReedSolomon.ofBytes (unhex (e.strD "block")) 0 c.blockLen)).map
          ReedSolomon.toBytes
        if got.isNone then rejected := rejected + 1
        check (got == e.hexOpt "decoded")
          s!"RS decode {k}+{ecc} with {e.natD "errors"} errors: got {got.map hex}"
  IO.println s!"  {(j.arrD "encode").size} encodings, {(j.arrD "decode").size} decodings ({rejected} rejected, as in C)"

def framingVectors : T Unit := do
  let j ← loadJson (componentsDir / "framing" / "framing.json")
  for e in j.arrD "cases" do
    let some protocol := Protocol.ofId? (e.natD "protocol") | check false "framing protocol"
    let fixed := e.natD "fixed_len"
    let data := unhex (e.strD "payload")
    let what := s!"framing {protocol} len {data.size} fixed {fixed}"
    let some cfg ← expectOk (Config.new { payloadLength := if fixed == 0 then none else some fixed }) what
      | continue
    if h : data.size ≤ maxLengthVariable then
      let coded := cfg.frame ⟨data.data, rfl⟩ h
      check (ByteArray.mk coded.toArray == unhex (e.strD "coded")) s!"{what}: coded bytes"
      let p := protocol.params
      let txs := (p.symbols coded).toArray.map fun tx =>
        (tx.mapFinIdx fun g v hg => ((p.toneBit ⟨g, hg⟩ v : Nat) : Int)).toArray
      check (txs == (e.arrD "txs").map (·.ints)) s!"{what}: tones per Tx"
    else check false s!"{what}: length"
  IO.println s!"  {(j.arrD "cases").size} payloads framed and split into tones"

def encoderVectors : T Unit := do
  let j ← loadJson (componentsDir / "encoder" / "encoder.json")
  for e in j.arrD "cases" do
    let some protocol := Protocol.ofId? (e.natD "protocol") | check false "encoder protocol"
    let fixed := e.natD "fixed_len"
    let (rate, rateOut) := ((e.natD "sample_rate").toFloat, (e.natD "sample_rate_out").toFloat)
    let data := unhex (e.strD "payload")
    let what := s!"encoder {protocol} len {data.size} at {rate.toUInt64}/{e.natD "samples_per_frame"} to {rateOut.toUInt64}"
    let some cfg ← expectOk (Config.new (params (if fixed == 0 then none else some fixed) rate rateOut
      (rate := rate) (spf := e.natD "samples_per_frame"))) what | continue
    if h : cfg.Sendable protocol data.size ∧ e.natD "volume" < 101 then
      let payload : Vector UInt8 data.size := ⟨data.data, rfl⟩
      let volume : Volume := ⟨e.natD "volume", h.2⟩
      let want := (e.arrD "frames").map (·.getStr?.toOption.getD "")
      unless want.isEmpty do
        let frames := encodeFrames cfg protocol volume payload h.1
        check (frames.size == want.size) s!"{what}: {frames.size} frames, C has {want.size}"
        let differing := (frames.zip want).toList.findIdx? fun (f, w) => hex64 (fnv1a (f32Bytes f.1)) != w
        check differing.isNone s!"{what}: frame {differing} differs"
      let w := encodeWith cfg protocol volume payload h.1
      check (w.size == e.natD "samples") s!"{what}: {w.size} samples"
      check (hex64 (fnv1a (f32Bytes w)) == e.strD "waveform") s!"{what}: waveform differs"
    else check false s!"{what}: not sendable"
  IO.println s!"  {(j.arrD "cases").size} transmissions, frame by frame"

def decoderVectors : T Unit := do
  let dir := componentsDir / "decoder"
  let j ← loadJson (dir / "decoder.json")
  for e in j.arrD "cases" do
    let name := e.strD "name"
    let some protocol := Protocol.ofId? (e.natD "protocol") | check false "decoder protocol"
    let fixed := e.natD "fixed_len"
    let (rate, spf) := ((e.natD "sample_rate").toFloat, e.natD "samples_per_frame")
    let only := (e.getObjValD "only_protocol") == .bool true
    let input := ofI16 (ofI16Bytes (← IO.FS.readBinFile (dir / e.strD "input")))
    let some d ← expectOk (Decoder.init (params (if fixed == 0 then none else some fixed) rate rate
      (rate := rate) (spf := spf)) (if only then [protocol] else Protocol.all)) name | continue
    let mut d := d
    let trace := e.arrD "trace"
    let mut payloads := 0
    for row in trace do
      let want := ((row.getArr?).toOption.getD #[])
      let f := ((want[0]!).getNat?).toOption.getD 0
      -- the input, then `zero_frames_after` frames of silence
      let block := if (f + 1) * spf ≤ input.size then slice input (f * spf) ((f + 1) * spf) else zeros spf
      let (d', r) := Decoder.step d block
      d := d'
      if r.isSome then payloads := payloads + 1
      let got : Array Json := #[toJson f, toJson (if d.rx.receiving then 1 else 0), toJson d.rx.framesToRecord,
        toJson d.rx.framesLeftToRecord, toJson d.rx.recvDurationFrames, match r with | some b => hex b | none => .null]
      unless got == want do
        check false s!"{name}: frame {f}: Lean {Json.compress (.arr got)}, C {Json.compress (.arr want)}"
        break
    IO.println s!"  {name}: {trace.size} frames, {payloads} payloads reported"

def components : T Unit := do
  reedSolomonVectors
  framingVectors
  encoderVectors
  decoderVectors

end GgwaveTests
