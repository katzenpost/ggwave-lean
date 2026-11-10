import Tests.Unit
import Tests.Vectors
import Tests.Interop

/-! `lake test`: unit checks, the C vectors, and live interop with C ggwave and
the Rust crate when `GGWAVE_C` and `GGWAVE_RS` name their test tools. -/

open Tests

def main : IO UInt32 := do
  let failures ← IO.mkRef 0
  let run (name : String) (t : T Unit) : IO Unit := do
    IO.println s!"{name}"
    let t0 ← IO.monoMsNow
    t.run failures
    IO.println s!"{name}: {(← IO.monoMsNow) - t0} ms"
  run "unit" unit
  let vs ← loadVectors
  run "vectors: Lean encoder against C waveforms" (encoderReproducesC vs)
  run "vectors: Lean decoder on C waveforms" (decoderReadsC vs)
  match (← IO.getEnv "GGWAVE_C"), (← IO.getEnv "GGWAVE_RS") with
  | some c, some rs => run "interop with C ggwave and the Rust crate" (interop c rs)
  | _, _ => IO.println "GGWAVE_C / GGWAVE_RS not set; skipping live interop"
  let n ← failures.get
  if n == 0 then
    IO.println "all tests passed"
    return 0
  IO.eprintln s!"{n} checks failed"
  return 1
