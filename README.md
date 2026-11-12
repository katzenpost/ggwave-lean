# ggwave-lean

A port of [ggwave](https://github.com/ggerganov/ggwave), Georgi Gerganov's
data-over-sound modem, to Lean 4. It carries short payloads over audio with
multi-frequency FSK, Reed-Solomon error correction and start/end markers.

The codec is pure by design: total functions from bytes to samples and back,
with no IO, no FFI and no dependencies beyond Lean core. Microphones and
speakers belong to the caller. It follows C ggwave at commit
[`060aec7`](https://github.com/ggerganov/ggwave/tree/060aec73dd7123ccac200442f75bdc7369795ffe)
through the Rust port [ggwave-rs](https://github.com/katzenpost/ggwave-rs),
whose modules it mirrors under `Ggwave/`: all twelve protocols, variable and
fixed-length payloads, DSS, and resampling. It computes in `Float32` wherever C
uses `float`, so encoded samples are identical to C ggwave's.

This package was ported with the help of a large language model, and is
checked against C ggwave with interop tests.

## Example

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
come from 48000 Hz with 1024-sample frames or 24000 Hz with 512.

`encode` checks untrusted input. Underneath it, `encodeWith` takes a
`Vector UInt8 n` and a proof that `n` fits the mode, so it has no error case:
`encodeWith Config.default .audibleFast 25 code (by decide)` for a 32-byte
`code`. Reed-Solomon blocks, tone choices and frames carry their lengths in
their types in the same way.

## Tests

`lake test` checks each module against vectors from C ggwave, and the whole
codec against waveforms it rendered; `tests/vectors/README.md` describes them.
With the C library and the Rust crate built as test tools, it also encodes and
decodes in both directions against them:

```sh
cargo build --release --manifest-path tools/ggwave-rs-oracle/Cargo.toml --target-dir target/rs
GGWAVE_C=$(tools/build-c-ggwave.sh) GGWAVE_RS=target/rs/release/ggwave-rs-oracle lake test
```

## Licence

MIT. The ported code keeps the copyright notices of ggwave (Georgi Gerganov)
and of the Reed-Solomon code it bundles (Mike Lubinets); see `LICENSE`.
