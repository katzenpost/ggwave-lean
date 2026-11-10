#!/bin/sh
# Regenerates Tests/vectors/: C ggwave 060aec7 waveforms (i16 little-endian,
# C's GGWAVE_SAMPLE_FORMAT_I16 output) plus manifest.txt, so `lake test`
# checks against C output without a C toolchain. The first 24 are the vectors
# of katzenpost/ggwave-rs (its tools/gen-vectors.sh), byte for byte; the last
# is the voucher contract at 24 kHz with 512-sample frames.
#
# Audible and DT/MT vectors are rendered at 16 kHz through C's resampler to
# keep the repository small; ultrasound needs 48 kHz.
set -eu

ROOT=$(cd "$(dirname "$0")/.." && pwd)
TOOL=${GGWAVE_C:-$("$ROOT/tools/build-c-ggwave.sh")}
OUT="$ROOT/Tests/vectors"

rm -f "$OUT"/*.i16
mkdir -p "$OUT"
echo "# name protocol_id fixed_len(0=variable) sample_rate_out volume payload_hex sample_rate samples_per_frame" > "$OUT/manifest.txt"

# payload of $1 bytes: byte i is ((i * 37 + $1) & 255) ^ 90
payload() {
    i=0; hex=""
    while [ "$i" -lt "$1" ]; do
        hex="$hex$(printf '%02x' $(( ((i * 37 + $1) & 255) ^ 90 )))"
        i=$((i + 1))
    done
    echo "$hex"
}

vector() { # protocol fixed len rate-out [volume rate spf suffix]
    volume=${5:-50}; rate=${6:-48000}; spf=${7:-1024}
    name="p$1-$( [ "$2" -eq 0 ] && echo var || echo fixed)-$3-$4${8:-}"
    hex=$(payload "$3")
    lflag=""; [ "$2" -ne 0 ] && lflag="-l $2"
    # shellcheck disable=SC2086
    "$TOOL" encode -s -p "$1" -v "$volume" $lflag -r "$4" -R "$rate" -f "$spf" "$hex" > "$OUT/$name.i16"
    echo "$name $1 $2 $4 $volume $hex $rate $spf" >> "$OUT/manifest.txt"
}

# the must-have: AUDIBLE_FAST at 48 kHz, variable and fixed, including 32-byte payloads
for len in 1 5 32; do vector 1 0 "$len" 48000; done
vector 1 32 32 48000

# every other variable-length protocol
for p in 0 2 6 7 8; do vector "$p" 0 16 16000; done
for p in 3 4 5; do vector "$p" 0 16 48000; done

# every protocol in fixed-length mode
for p in 0 1 2 6 7 8 9 10 11; do vector "$p" 8 8 16000; done
for p in 3 4 5; do vector "$p" 8 8 48000; done

# the voucher contract's other rate: 24 kHz, 512-sample frames, volume 25
vector 1 0 32 24000 25 24000 512 -512

du -sh "$OUT"
