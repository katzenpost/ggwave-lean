# ggwave-lean

A port of [ggwave](https://github.com/ggerganov/ggwave), Georgi Gerganov's
data-over-sound modem, to Lean 4, for Lean programs that send short payloads
over audio. It uses multi-frequency FSK, Reed-Solomon error correction and
start/end markers, and its encoded samples are identical to C ggwave's.

The codec is pure: total functions from bytes to samples and back, with no IO,
no FFI and no dependencies beyond Lean core. Microphones and speakers belong to
the caller. Outside Lean, use C ggwave or the Rust port
[ggwave-rs](https://github.com/katzenpost/ggwave-rs). This package was ported
with the help of a large language model, and is checked against C ggwave with
interop tests.

## Quick start

Add it to your `lakefile.toml`:

```toml
[[require]]
name = "ggwave"
git = "https://github.com/katzenpost/ggwave-lean"
rev = "main"
```

Send a 32-byte code with Audible Fast at 48 kHz and volume 25, then listen for
it, feeding the decoder blocks of samples as a microphone would deliver them:

```lean
import Ggwave
open Ggwave

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
```

`Parameters` takes the sample rates and frame size; ggwave's 46.875 Hz bins
come from 48000 Hz with 1024-sample frames or 24000 Hz with 512. `encode`
checks untrusted input; underneath it, `encodeWith` takes a `Vector UInt8 n`
and a proof that `n` fits the mode, so it has no error case.

## Contents

- [`Ggwave/`](Ggwave): the codec, one module per module of ggwave-rs
  (protocols, GF(2^8), Reed-Solomon, framing, FFT, resampler, tones, markers,
  encoder, decoder).
- [`GgwaveTests/`](GgwaveTests): the `lake test` driver.
- [`tests/vectors/`](tests/vectors/README.md): vectors and waveforms from C ggwave.
- [`tools/`](tools): scripts and oracles that build C ggwave and ggwave-rs for
  the interop tests and regenerate the vectors.

## Status

Version 0.1.0, Lean `v4.35.0`. It follows C ggwave at commit
[`060aec7`](https://github.com/ggerganov/ggwave/tree/060aec73dd7123ccac200442f75bdc7369795ffe)
through ggwave-rs: all twelve protocols, variable and fixed-length payloads,
DSS, and resampling. It computes in `Float32` wherever C uses `float`.

## Tests

`lake test` checks each module against vectors from C ggwave, and the whole
codec against waveforms it rendered. With the C library and the Rust crate
built as test tools, it also encodes and decodes in both directions against
them, as CI does:

```sh
cargo build --release --manifest-path tools/ggwave-rs-oracle/Cargo.toml --target-dir target/rs
GGWAVE_C=$(tools/build-c-ggwave.sh) GGWAVE_RS=target/rs/release/ggwave-rs-oracle lake test
```

Issues and pull requests are welcome on GitHub; a change to the codec should
keep `lake test` passing against C ggwave.

## Licence

MIT. The ported code keeps the copyright notices of ggwave (Georgi Gerganov)
and of the Reed-Solomon code it bundles (Mike Lubinets); see `LICENSE`.
