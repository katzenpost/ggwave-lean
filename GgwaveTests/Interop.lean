import GgwaveTests.Common

/-! Live interop with C ggwave 060aec7 and the Rust crate, through test tools
with one command line (`tools/ggwave-c.cpp`, built by `tools/build-c-ggwave.sh`,
and `tools/ggwave-rs-oracle`). Run when `GGWAVE_C` and `GGWAVE_RS` name them. -/

open Ggwave

namespace GgwaveTests

/-- Runs a test tool with `input` on stdin and returns its stdout. -/
def runTool (tool : String) (args : Array String) (input : ByteArray) : IO ByteArray := do
  let child ← IO.Process.spawn { cmd := tool, args, stdin := .piped, stdout := .piped, stderr := .inherit }
  let (stdin, child) ← child.takeStdin
  stdin.write input
  -- closing stdin happens when `stdin` is dropped here; the tools write little until then
  let out ← IO.asTask child.stdout.readBinToEnd .dedicated
  let code ← child.wait
  let out ← IO.ofExcept out.get
  if code != 0 then throw (.userError s!"{tool} {args} exited with {code}")
  return out

/-- A transmission setup: payload mode, operating rate and frame size. -/
structure Mode where
  fixed : Option Nat := none
  rate : Float := 48000
  spf : Nat := 1024
  dss : Bool := false

def Mode.args (m : Mode) (rate : Float) : Array String :=
  #["-r", toString rate.toUInt64, "-R", toString m.rate.toUInt64, "-f", toString m.spf] ++
    (match m.fixed with | some l => #["-l", toString l] | none => #[]) ++
    (if m.dss then #["-d"] else #[])

def toolEncode (tool : String) (m : Mode) (data : ByteArray) (p : Protocol) (volume : Nat)
    (rateOut : Float) : IO FloatArray :=
  ofF32Bytes <$> runTool tool (#["encode", "-p", toString p.id, "-v", toString volume] ++
    m.args rateOut ++ #[hex data]) .empty

/-- Payloads the tool's decoder reports, listening for `only` or for every protocol. -/
def toolDecode (tool : String) (m : Mode) (samples : FloatArray) (rateInp : Float)
    (only : Option Protocol := none) : IO (Array ByteArray) := do
  let p := match only with | some p => #["-p", toString p.id] | none => #[]
  let out ← runTool tool (#["decode"] ++ m.args rateInp ++ p) (f32Bytes samples)
  return ((String.fromUTF8? out).getD "").splitOn "\n" |>.filter (!·.isEmpty) |>.map unhex |>.toArray

def leanEncode (m : Mode) (data : ByteArray) (p : Protocol) (volume : Nat) (rateOut : Float) :
    Except Error FloatArray :=
  encode (params m.fixed m.rate rateOut (rate := m.rate) (spf := m.spf) (dss := m.dss)) p volume data

def leanDecode (m : Mode) (samples : FloatArray) (rateInp : Float)
    (only : List Protocol := Protocol.all) (chunk := 4096) : Except Error (Array ByteArray) := do
  let d ← Decoder.init (params m.fixed rateInp m.rate (rate := m.rate) (spf := m.spf) (dss := m.dss)) only
  return decodeAll d samples chunk

def lengthsVariable : List Nat := [1, 5, 16, 32, 140]
def lengthsFixed : List Nat := [1, 8, 32, 64]

def cases : List (Mode × Protocol × Nat) :=
  (protocols false).flatMap (fun p => lengthsVariable.map fun l => (({} : Mode), p, l)) ++
  (protocols true).flatMap (fun p => lengthsFixed.map fun l => (({ fixed := some l } : Mode), p, l))

-- a microphone-like noise floor, then two noisy trials; on exact digital silence
-- C ggwave misses some DT messages (its marker thresholds compare against zero bins)
def trials : List (Float × UInt64) := [(0.002, 0), (0.1, 1), (0.1, 2)]

-- 1-5 byte payloads carry only 2-4 ECC bytes and C ggwave itself sometimes
-- miscorrects them; longer payloads must always decode
def short : Nat := 5

def describe (m : Mode) (p : Protocol) (len : Nat) : String :=
  s!"{p} len {len} {if m.fixed.isSome then "fixed" else "variable"}"

/-- Records a decode: longer payloads must be found, short ones may be missed rarely. -/
def found (got : Array ByteArray) (data : ByteArray) (misses : IO.Ref Nat) (what : String) : T Unit := do
  unless got.contains data do
    check (data.size ≤ short) s!"{what}: payload not decoded, got {got.toList.map hex}"
    misses.modify (· + 1)

def checkShortMisses (misses : IO.Ref Nat) (what : String) : T Unit := do
  let n ← misses.get
  let shortTrials := (cases.filter (·.2.2 ≤ short)).length * trials.length
  IO.println s!"  {what}: {n} of {shortTrials} short-payload trials missed"
  check (n * 10 ≤ shortTrials) s!"{what}: too many short-payload misses ({n})"

/-- Lean's samples against each tool's, every protocol and length at 48 kHz.
Prints the largest difference per protocol. -/
def encodeMatches (tools : List (String × String)) : T Unit := do
  IO.println "  protocol       max |Lean - C|   max |Lean - Rust|   (f32 samples, all lengths, variable and fixed)"
  for p in Protocol.all do
    let mut line := s!"  {p.name.pushn ' ' (14 - p.name.length)}"
    for (name, tool) in tools do
      let mut worst := 0
      for (m, q, len) in cases do
        if q != p then continue
        let data := payload len len
        let some r ← expectOk (leanEncode m data p 50 48000) (describe m p len) | continue
        let c ← toolEncode tool m data p 50 48000
        check (r.size == c.size) s!"{name} {describe m p len}: {r.size} samples, {name} has {c.size}"
        worst := max worst (maxAbsDiff r c)
        let differing := ((toI16 r).zip (toI16 c)).filter (fun (a, b) => a != b) |>.size
        check (differing ≤ r.size / 1000) s!"{name} {describe m p len}: {differing} i16 samples differ"
      check (worst < 1e-6) s!"{name} {p}: max diff {worst}"
      line := line ++ s!" {(fmt worst).pushn ' ' (16 - (fmt worst).length)}"
    IO.println line

/-- Lean-encoded audio decoded by each tool. -/
def leanEncodeToolDecode (tools : List (String × String)) : T Unit := do
  for (name, tool) in tools do
    let misses ← IO.mkRef 0
    for (m, p, len) in cases do
      let data := payload len 3
      let some r ← expectOk (leanEncode m data p 50 48000) (describe m p len) | continue
      for (noise, seed) in trials do
        let x := addNoise (pad r 12345 48000) noise (seed + len.toUInt64)
        found (← toolDecode tool m x 48000) data misses s!"{name} decodes Lean {describe m p len} noise {noise}"
    checkShortMisses misses s!"Lean encode, {name} decode"

/-- C-encoded audio decoded by Lean; also counts how often Lean returns exactly what C does. -/
def cEncodeLeanDecode (c : String) : T Unit := do
  let misses ← IO.mkRef 0
  let mut same := 0
  let mut total := 0
  for (m, p, len) in cases do
    let data := payload len 9
    let ce ← toolEncode c m data p 50 48000
    for (noise, seed) in trials do
      let x := addNoise (pad ce 4321 48000) noise (seed + len.toUInt64)
      let some got ← expectOk (leanDecode m x 48000) (describe m p len) | continue
      found got data misses s!"Lean decodes C {describe m p len} noise {noise}"
      total := total + 1
      if got == (← toolDecode c m x 48000) then same := same + 1
  IO.println s!"  Lean's decoder returned exactly what C's did in {same} of {total} trials"
  checkShortMisses misses "C encode, Lean decode"

/-- The voucher contract: Audible Fast, variable length, 32 bytes, volume 25,
at 48 kHz / 1024 and 24 kHz / 512, through heavier noise and ragged blocks. -/
def voucher (c : String) (tools : List (String × String)) : T Unit := do
  for m in [({} : Mode), { rate := 24000, spf := 512 }] do
    let rate := m.rate.toUInt64.toNat
    let mut worst := 0
    for seed in [0:8] do
      let data := payload 32 (seed * 29 % 256)
      let some r ← expectOk (leanEncode m data .audibleFast 25 m.rate) "voucher encode" | continue
      let ce ← toolEncode c m data .audibleFast 25 m.rate
      worst := max worst (maxAbsDiff r ce)
      check (r.size == ce.size) s!"voucher at {rate}: {r.size} samples, C has {ce.size}"
      -- half the samples per frame at 24 kHz: scale the noise for the same SNR per bin
      for noise in [0.1, 0.3, 0.5].map (· * (m.rate / 48000).sqrt) do
        let x := addNoise (pad r (rate / 48 + 97 * seed) rate) noise seed.toUInt64
        for (name, tool) in tools do
          let got ← toolDecode tool m x m.rate (only := some .audibleFast)
          check (got == #[data]) s!"voucher at {rate}: {name} decodes Lean, seed {seed} noise {noise}: {got.toList.map hex}"
        let x := addNoise (pad ce (rate / 48 + 97 * seed) rate) noise (seed.toUInt64 + 100)
        let some got ← expectOk (leanDecode m x m.rate [.audibleFast] (chunk := 333 + seed)) "voucher decode" | continue
        check (got == #[data]) s!"voucher at {rate}: Lean decodes C, seed {seed} noise {noise}: {got.toList.map hex}"
    IO.println s!"  voucher at {rate} Hz / {m.spf}: max |Lean - C| = {fmt worst} over 8 payloads"
    check (worst < 1e-6) s!"voucher at {rate}: max diff {worst}"

/-- Other playback and capture rates go through ggwave's resampler on both sides. -/
def resampledRates (c : String) (tools : List (String × String)) : T Unit := do
  let data := payload 32 1
  for rate in [16000.0, 44100.0] do
    for p in [Protocol.audibleFast, .dtFast] do
      let some r ← expectOk (leanEncode {} data p 50 rate) s!"{p} at {rate}" | continue
      let ce ← toolEncode c {} data p 50 rate
      check (r.size == ce.size) s!"{p} at {rate}: {r.size} samples, C has {ce.size}"
      IO.println s!"  {p} at {rate.toUInt64} Hz: max |Lean - C| = {fmt (maxAbsDiff r ce)}"
      check (maxAbsDiff r ce < 1e-5) s!"{p} at {rate}: max diff {maxAbsDiff r ce}"
      let n := rate.toUInt64.toNat
      for (name, tool) in tools do
        let got ← toolDecode tool {} (addNoise (pad r 1000 n) 0.1 5) rate
        check (got.contains data) s!"{name} decodes Lean {p} at {rate}"
      let some got ← expectOk (leanDecode {} (addNoise (pad ce 1000 n) 0.1 6) rate) s!"{p} at {rate}" | continue
      check (got.contains data) s!"Lean decodes C {p} at {rate}: {got.toList.map hex}"

/-- `GGWAVE_OPERATING_MODE_USE_DSS` on both sides. -/
def dssBothWays (c : String) (tools : List (String × String)) : T Unit := do
  for fixed in [none, some 32] do
    let m : Mode := { fixed, dss := true }
    let data := payload 32 77
    let some r ← expectOk (leanEncode m data .audibleFast 50 48000) "DSS encode" | continue
    let ce ← toolEncode c m data .audibleFast 50 48000
    check (r.size == ce.size && maxAbsDiff r ce < 1e-6) s!"DSS encode differs, fixed {fixed}"
    let some got ← expectOk (leanDecode m (addNoise (pad ce 3000 48000) 0.1 1) 48000) "DSS decode" | continue
    check (got.contains data) s!"Lean DSS decode, fixed {fixed}"
    for (name, tool) in tools do
      let got ← toolDecode tool m (addNoise (pad r 3000 48000) 0.1 2) 48000
      check (got.contains data) s!"{name} DSS decode of Lean, fixed {fixed}"

def interop (c rs : String) : T Unit := do
  let tools := [("C", c), ("Rust", rs)]
  let part (name : String) (t : T Unit) : T Unit := do
    let t0 ← IO.monoMsNow
    IO.println s!" {name}"
    t
    IO.println s!" {name}: {(← IO.monoMsNow) - t0} ms"
  part "encoder samples against C and Rust" (encodeMatches tools)
  part "Lean encode, C and Rust decode" (leanEncodeToolDecode tools)
  part "C encode, Lean decode" (cEncodeLeanDecode c)
  part "voucher contract" (voucher c tools)
  part "resampled rates" (resampledRates c tools)
  part "DSS" (dssBothWays c tools)

end GgwaveTests
