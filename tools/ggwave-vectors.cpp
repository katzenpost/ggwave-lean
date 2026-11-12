// Component test vectors from C ggwave at the pinned commit, as JSON on stdout.
// Built by tools/build-c-ggwave.sh; run by tools/gen-vectors.sh. Not part of the library.
//
//   ggwave-vectors reed-solomon            GF(2^8) tables and ops, encode, decode
//   ggwave-vectors framing                 payload -> coded bytes -> tones per Tx
//   ggwave-vectors encoder                 per-frame hashes of encoded waveforms
//   ggwave-vectors decoder <dir>           per-frame receiver traces; writes inputs to <dir>
//
// Payloads follow ggwave-rs tests/common: byte i of a len-byte payload with seed s
// is ((i * 37 + s) & 255) ^ 0x5a. Hex is lowercase; samples are 32-bit floats or
// 16-bit integers, little-endian.

#ifndef PROGMEM
#define PROGMEM
#endif

#include "ggwave/ggwave.h"
#include "reed-solomon/rs.hpp"

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <string>
#include <vector>

typedef std::vector<uint8_t> Bytes;

static Bytes payload(int len, int seed) {
    Bytes b(len);
    for (int i = 0; i < len; ++i) b[i] = (uint8_t) (((i * 37 + seed) & 255) ^ 0x5a);
    return b;
}

static std::string hex(const uint8_t * p, size_t n) {
    static const char * d = "0123456789abcdef";
    std::string s;
    for (size_t i = 0; i < n; ++i) { s += d[p[i] >> 4]; s += d[p[i] & 15]; }
    return s;
}
static std::string hex(const Bytes & b) { return hex(b.data(), b.size()); }

static int eccBytesForLength(int len) { return len < 4 ? 2 : std::max(4, 2 * (len / 5)); }

// A fresh coder over zeroed work memory, so results never depend on an earlier call.
struct Coder {
    std::vector<uint8_t> work;
    RS::ReedSolomon rs;
    Coder(int msg, int ecc) : work(RS::ReedSolomon::getWorkSize_bytes(msg, ecc), 0), rs(msg, ecc, work.data()) {}
};

static Bytes rsEncode(const Bytes & msg, int ecc) {
    Coder c(msg.size(), ecc);
    Bytes out(msg.size() + ecc);
    c.rs.Encode(msg.data(), out.data());
    return out;
}

// splitmix-style generator for corruption positions and values
static uint64_t g_state = 0x9e3779b97f4a7c15ull;
static uint64_t next() {
    uint64_t z = (g_state += 0x9e3779b97f4a7c15ull);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ull;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebull;
    return z ^ (z >> 31);
}

static void reedSolomon() {
    printf("{\n\"generator\": \"tools/ggwave-vectors reed-solomon\",\n");
    printf("\"ggwave\": \"060aec73dd7123ccac200442f75bdc7369795ffe\",\n");
    printf("\"exp\": \"%s\",\n", hex(RS::gf::exp, 512).c_str());
    printf("\"log\": \"%s\",\n", hex(RS::gf::log, 256).c_str());
    const uint8_t xs[] = {0, 1, 2, 3, 0x1d, 0x53, 0x8e, 0xca, 0xff};
    printf("\"mul\": [");
    bool first = true;
    for (uint8_t x : xs) for (uint8_t y : xs) {
        printf("%s[%d, %d, %d]", first ? "" : ", ", x, y, RS::gf::mul(x, y)); first = false;
    }
    printf("],\n\"div\": [");
    first = true;
    for (uint8_t x : xs) for (uint8_t y : xs) {
        if (y == 0) continue;
        printf("%s[%d, %d, %d]", first ? "" : ", ", x, y, RS::gf::div(x, y)); first = false;
    }
    printf("],\n\"inverse\": [");
    first = true;
    for (uint8_t x : xs) {
        if (x == 0) continue;
        printf("%s[%d, %d]", first ? "" : ", ", x, RS::gf::inverse(x)); first = false;
    }
    printf("],\n\"pow\": [");
    first = true;
    const int powers[] = {-300, -255, -3, -1, 0, 1, 2, 7, 254, 255, 256, 600};
    for (uint8_t x : xs) for (int p : powers) {
        if (x == 0) continue;
        printf("%s[%d, %d, %d]", first ? "" : ", ", x, p, RS::gf::pow(x, p)); first = false;
    }
    // every code ggwave uses: the length header, and each payload length
    printf("],\n\"encode\": [\n");
    printf("{\"msg_len\": 1, \"ecc_len\": 2, \"msg\": \"20\", \"block\": \"%s\"}", hex(rsEncode(Bytes{0x20}, 2)).c_str());
    for (int len = 1; len <= 140; ++len) {
        const int ecc = eccBytesForLength(len);
        const Bytes msg = payload(len, len);
        printf(",\n{\"msg_len\": %d, \"ecc_len\": %d, \"msg\": \"%s\", \"block\": \"%s\"}",
               len, ecc, hex(msg).c_str(), hex(rsEncode(msg, ecc)).c_str());
    }
    // decode: t errors at unknown positions, from none to all the parity can hold and past it
    printf("\n],\n\"decode\": [\n");
    first = true;
    const int lens[] = {1, 3, 4, 5, 16, 32, 64, 140};
    for (int len : lens) {
        const int ecc = eccBytesForLength(len);
        const Bytes msg = payload(len, 7 * len);
        const Bytes block = rsEncode(msg, ecc);
        const int n = (int) block.size();
        std::vector<int> counts = {0, 1, ecc / 2, ecc / 2 + 1, ecc, ecc + 3};
        for (size_t ci = 0; ci < counts.size(); ++ci) {
            if (ci > 0 && counts[ci] == counts[ci - 1]) continue;
            const int t = std::min(counts[ci], n);
            for (int trial = 0; trial < 3; ++trial) {
                Bytes bad = block;
                std::vector<int> pos;
                while ((int) pos.size() < t) {
                    const int p = (int) (next() % n);
                    bool seen = false;
                    for (int q : pos) seen |= q == p;
                    if (!seen) pos.push_back(p);
                }
                for (int p : pos) bad[p] ^= (uint8_t) (1 + next() % 255);
                Coder c(len, ecc);
                Bytes out(len);
                const int r = c.rs.Decode(bad.data(), out.data());
                printf("%s{\"msg_len\": %d, \"ecc_len\": %d, \"msg\": \"%s\", \"errors\": %d, \"block\": \"%s\", \"decoded\": %s}",
                       first ? "" : ",\n", len, ecc, hex(msg).c_str(), t, hex(bad).c_str(),
                       r == 0 ? ("\"" + hex(out) + "\"").c_str() : "null");
                first = false;
                if (t == 0) break;
            }
        }
    }
    printf("\n]\n}\n");
}

static GGWave::Parameters params(int fixedLen, float rate, int spf, int mode) {
    auto p = GGWave::getDefaultParameters();
    p.payloadLength = fixedLen;
    p.sampleRate = p.sampleRateInp = p.sampleRateOut = rate;
    p.samplesPerFrame = spf;
    p.sampleFormatInp = p.sampleFormatOut = GGWAVE_SAMPLE_FORMAT_F32;
    p.operatingMode = mode;
    return p;
}

static void framing() {
    printf("{\n\"generator\": \"tools/ggwave-vectors framing\",\n");
    printf("\"ggwave\": \"060aec73dd7123ccac200442f75bdc7369795ffe\",\n\"cases\": [\n");
    bool first = true;
    for (int fixed = 0; fixed <= 1; ++fixed) {
        const int lensVar[] = {1, 5, 32, 140}, lensFixed[] = {1, 8, 32, 64};
        for (int id = 0; id < 12; ++id) {
            const GGWave::Protocol & pr = GGWave::Protocols::kDefault()[id];
            if (!fixed && pr.extra == 2) continue;
            for (int li = 0; li < 4; ++li) {
                const int len = fixed ? lensFixed[li] : lensVar[li];
                const Bytes data = payload(len, len);
                // ggwave's framing: RS(1, 2) of the length in variable mode, then RS(len, ecc) of the data
                Bytes coded;
                if (!fixed) coded = rsEncode(Bytes{(uint8_t) len}, 2);
                const Bytes block = rsEncode(data, eccBytesForLength(len));
                coded.insert(coded.end(), block.begin(), block.end());

                // A full transmit, not GGWAVE_OPERATING_MODE_TX_ONLY_TONES: when the coded bytes do not
                // fill the last Tx, C reads past them into the next buffer, which is zero here (as in
                // the waveform) but holds the payload length in tones-only mode.
                GGWave gg(params(fixed ? len : -1, 48000, 1024, GGWAVE_OPERATING_MODE_TX));
                gg.init(len, (const char *) data.data(), (GGWave::TxProtocolId) id, 50);
                gg.encode();
                const GGWave::Tones tones = gg.txTones();

                // walk C's tone list as encode() writes it: one group per framesPerTx frames,
                // separated by -1 unless the protocol sounds one tone per Tx
                const int nm = fixed ? 0 : 16;
                const int nTones = pr.nTones();
                const int dataFrames = pr.extra * ((int) ((coded.size() + pr.bytesPerTx - 1) / pr.bytesPerTx)) * pr.framesPerTx;
                const int sep = nTones > 1 ? 1 : 0;
                int at = 0;
                std::string txs;
                for (int frameId = 0; frameId < 2 * nm + dataFrames; frameId += pr.framesPerTx) {
                    const bool marker = frameId < nm || frameId >= nm + dataFrames;
                    const int count = marker ? 16 : nTones;
                    if (!marker) {
                        txs += txs.empty() ? "[" : ", [";
                        for (int k = 0; k < count; ++k) txs += (k ? ", " : "") + std::to_string((int) tones[at + k]);
                        txs += "]";
                    }
                    at += count + sep;
                }
                if (at != tones.size()) { fprintf(stderr, "tone list walk mismatch\n"); exit(1); }
                printf("%s{\"protocol\": %d, \"fixed_len\": %d, \"payload\": \"%s\", \"coded\": \"%s\", \"txs\": [%s]}",
                       first ? "" : ",\n", id, fixed ? len : 0, hex(data).c_str(), hex(coded).c_str(), txs.c_str());
                first = false;
            }
        }
    }
    printf("\n]\n}\n");
}

static uint64_t fnv1a(const void * p, size_t n) {
    uint64_t h = 0xcbf29ce484222325ull;
    for (size_t i = 0; i < n; ++i) { h ^= ((const uint8_t *) p)[i]; h *= 0x100000001b3ull; }
    return h;
}

static std::vector<float> encodeF32(int id, int fixedLen, float rate, int spf, float rateOut, int volume, const Bytes & data) {
    auto p = params(fixedLen, rate, spf, GGWAVE_OPERATING_MODE_TX);
    p.sampleRateOut = rateOut;
    GGWave gg(p);
    gg.init((int) data.size(), (const char *) data.data(), (GGWave::TxProtocolId) id, volume);
    const uint32_t n = gg.encode();
    std::vector<float> s(n / sizeof(float));
    memcpy(s.data(), gg.txWaveform(), n);
    return s;
}

static void encoder() {
    printf("{\n\"generator\": \"tools/ggwave-vectors encoder\",\n");
    printf("\"ggwave\": \"060aec73dd7123ccac200442f75bdc7369795ffe\",\n");
    printf("\"hash\": \"FNV-1a 64 over the float32 little-endian bytes\",\n\"cases\": [\n");
    struct Case { int id, fixed, len, volume; float rate; int spf; float rateOut; };
    const Case cases[] = {
        {1, 0, 32, 25, 48000, 1024, 48000}, {1, 0, 32, 25, 24000, 512, 24000},
        {0, 0, 5, 50, 48000, 1024, 48000}, {4, 0, 16, 50, 48000, 1024, 48000},
        {6, 0, 16, 50, 48000, 1024, 48000}, {9, 8, 8, 50, 48000, 1024, 48000},
        {2, 8, 3, 10, 48000, 256, 48000}, {1, 0, 32, 50, 48000, 1024, 16000},
    };
    bool first = true;
    for (const Case & c : cases) {
        const Bytes data = payload(c.len, c.len);
        const std::vector<float> s = encodeF32(c.id, c.fixed ? c.fixed : -1, c.rate, c.spf, c.rateOut, c.volume, data);
        std::string frames;
        if (c.rateOut == c.rate) {
            for (size_t f = 0; f * c.spf < s.size(); ++f) {
                char buf[24];
                snprintf(buf, sizeof buf, "%s\"%016llx\"", f ? ", " : "", (unsigned long long) fnv1a(&s[f * c.spf], c.spf * sizeof(float)));
                frames += buf;
            }
        }
        printf("%s{\"protocol\": %d, \"fixed_len\": %d, \"volume\": %d, \"sample_rate\": %g, \"samples_per_frame\": %d, "
               "\"sample_rate_out\": %g, \"payload\": \"%s\", \"samples\": %zu, \"waveform\": \"%016llx\", \"frames\": [%s]}",
               first ? "" : ",\n", c.id, c.fixed, c.volume, c.rate, c.spf, c.rateOut, hex(data).c_str(), s.size(),
               (unsigned long long) fnv1a(s.data(), s.size() * sizeof(float)), frames.c_str());
        first = false;
    }
    printf("\n]\n}\n");
}

static void decoder(const char * dir) {
    printf("{\n\"generator\": \"tools/ggwave-vectors decoder\",\n");
    printf("\"ggwave\": \"060aec73dd7123ccac200442f75bdc7369795ffe\",\n");
    printf("\"trace\": [\"frame\", \"receiving\", \"frames_to_record\", \"frames_left_to_record\", \"recv_duration_frames\", \"payload\"],\n");
    printf("\"cases\": [\n");
    struct Case { const char * name; int id, fixed, len, volume; float rate; int spf; int onlyRx; float noise; int keepFrames; int tailFrames; };
    const Case cases[] = {
        // the voucher at 24 kHz / 512, listening for Audible Fast only
        {"voucher-clean", 1, 0, 32, 25, 24000, 512, 1, 0.0f, -1, 40},
        {"voucher-noisy", 1, 0, 32, 25, 24000, 512, 1, 0.25f, -1, 40},
        // cut off after 70 frames: no end marker, so the receiver records until it gives up
        {"voucher-truncated", 1, 0, 32, 25, 24000, 512, 1, 0.0f, 70, 1250},
        // every protocol enabled, fixed length: the payload is reported on several frames
        {"fixed-dt-fast", 7, 8, 8, 50, 48000, 1024, 0, 0.05f, -1, 40},
    };
    bool first = true;
    for (const Case & c : cases) {
        const Bytes data = payload(c.len, c.len);
        const std::vector<float> s = encodeF32(c.id, c.fixed ? c.fixed : -1, c.rate, c.spf, c.rate, c.volume, data);
        // input: 7 frames of silence, the waveform (or its first keepFrames frames), noise, as i16
        const size_t lead = 7 * c.spf;
        const size_t body = c.keepFrames < 0 ? s.size() : std::min(s.size(), (size_t) c.keepFrames * c.spf);
        std::vector<int16_t> in(lead + body + 8 * c.spf, 0);
        uint64_t x = 0x2545f4914f6cdd1dull;
        for (size_t i = 0; i < in.size(); ++i) {
            float v = (i >= lead && i < lead + body) ? s[i - lead] : 0.0f;
            if (c.noise > 0) {
                x ^= x << 13; x ^= x >> 7; x ^= x << 17;
                v += ((float) (x >> 40) / 16777216.0f - 0.5f) * c.noise;
            }
            v = v < -1 ? -1 : v > 1 ? 1 : v;
            in[i] = (int16_t) (32767.0f * v);
        }
        const std::string file = std::string(c.name) + ".i16";
        FILE * f = fopen((std::string(dir) + "/" + file).c_str(), "wb");
        fwrite(in.data(), sizeof(int16_t), in.size(), f);
        fclose(f);

        auto p = params(c.fixed ? c.fixed : -1, c.rate, c.spf, GGWAVE_OPERATING_MODE_RX);
        p.sampleFormatInp = GGWAVE_SAMPLE_FORMAT_I16;
        GGWave::Protocols::rx().enableAll();
        if (c.onlyRx) GGWave::Protocols::rx().only((GGWave::ProtocolId) c.id);
        GGWave gg(p);
        const size_t frames = in.size() / c.spf + c.tailFrames;
        std::vector<int16_t> zero(c.spf, 0);
        printf("%s{\"name\": \"%s\", \"input\": \"%s\", \"zero_frames_after\": %d, \"protocol\": %d, \"only_protocol\": %s, "
               "\"fixed_len\": %d, \"volume\": %d, \"noise\": %g, \"sample_rate\": %g, \"samples_per_frame\": %d, \"payload\": \"%s\",\n\"trace\": [",
               first ? "" : ",\n", c.name, file.c_str(), c.tailFrames, c.id, c.onlyRx ? "true" : "false", c.fixed, c.volume,
               c.noise, c.rate, c.spf, hex(data).c_str());
        first = false;
        GGWave::TxRxData out;
        for (size_t fi = 0; fi < frames; ++fi) {
            const int16_t * frame = fi * c.spf < in.size() ? &in[fi * c.spf] : zero.data();
            gg.decode(frame, c.spf * sizeof(int16_t));
            const int n = gg.rxTakeData(out);
            printf("%s[%zu, %d, %d, %d, %d, %s]", fi ? ",\n" : "\n", fi, gg.rxReceiving() ? 1 : 0, gg.rxFramesToRecord(),
                   gg.rxFramesLeftToRecord(), gg.rxDurationFrames(),
                   n > 0 ? ("\"" + hex((const uint8_t *) out.data(), n) + "\"").c_str() : "null");
        }
        printf("]}");
    }
    printf("\n]\n}\n");
}

int main(int argc, char ** argv) {
    GGWave::setLogFile(nullptr);
    const std::string cmd = argc > 1 ? argv[1] : "";
    if (cmd == "reed-solomon") reedSolomon();
    else if (cmd == "framing") framing();
    else if (cmd == "encoder") encoder();
    else if (cmd == "decoder" && argc > 2) decoder(argv[2]);
    else {
        fprintf(stderr, "usage: ggwave-vectors reed-solomon|framing|encoder|decoder <dir>\n");
        return 2;
    }
    return 0;
}
