# Test vectors

Waveforms rendered by C ggwave at commit
[`060aec7`](https://github.com/ggerganov/ggwave/tree/060aec73dd7123ccac200442f75bdc7369795ffe)
(MIT, Georgi Gerganov), as 16-bit little-endian samples, C's
`GGWAVE_SAMPLE_FORMAT_I16` output. `manifest.txt` gives each file's protocol,
payload length mode, output rate, volume, payload, operating rate and frame size.

`tools/gen-vectors.sh` regenerates them with the C tool in `tools/`. The 24
vectors at 48 kHz / 1024 are byte for byte those of
[katzenpost/ggwave-rs](https://github.com/katzenpost/ggwave-rs) `tests/vectors`
(commit `b0435a8`); `p1-var-32-24000-512` adds the voucher case at 24 kHz with
512-sample frames.
