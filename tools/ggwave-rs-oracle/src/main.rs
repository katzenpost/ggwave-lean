//! Rust ggwave over raw little-endian samples on stdin/stdout, with the
//! options of tools/ggwave-c.cpp (see there).

use ggwave::{to_i16, Decoder, Encoder, Parameters, Protocol};
use std::io::{Read, Write};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let cmd = args.get(1).map(String::as_str).unwrap_or("");
    let (mut protocol, mut only_rx, mut volume, mut fixed) = (1u8, false, 50u8, None);
    let (mut rate, mut op_rate, mut spf, mut dss, mut i16s) = (None, 48000.0f32, 1024usize, false, false);
    let mut hex = String::new();
    let mut i = 2;
    while i < args.len() {
        let next = |i: usize| args[i + 1].clone();
        match args[i].as_str() {
            "-p" => { protocol = next(i).parse().unwrap(); only_rx = true; i += 1 }
            "-v" => { volume = next(i).parse().unwrap(); i += 1 }
            "-l" => { fixed = Some(next(i).parse().unwrap()); i += 1 }
            "-r" => { rate = Some(next(i).parse().unwrap()); i += 1 }
            "-R" => { op_rate = next(i).parse().unwrap(); i += 1 }
            "-f" => { spf = next(i).parse().unwrap(); i += 1 }
            "-d" => dss = true,
            "-s" => i16s = true,
            a => hex = a.to_string(),
        }
        i += 1;
    }
    let rate = rate.unwrap_or(op_rate);
    let params = Parameters {
        payload_length: fixed,
        sample_rate_inp: if cmd == "decode" { rate } else { op_rate },
        sample_rate_out: if cmd == "encode" { rate } else { op_rate },
        sample_rate: op_rate,
        samples_per_frame: spf,
        dss,
        ..Parameters::default()
    };
    let p = Protocol::from_id(protocol).expect("protocol id");
    let mut stdout = std::io::stdout().lock();
    match cmd {
        "encode" => {
            let data: Vec<u8> = (0..hex.len() / 2)
                .map(|i| u8::from_str_radix(&hex[2 * i..2 * i + 2], 16).expect("hex"))
                .collect();
            let s = Encoder::new(params).unwrap().encode(&data, p, volume).unwrap();
            let bytes: Vec<u8> = if i16s {
                to_i16(&s).iter().flat_map(|x| x.to_le_bytes()).collect()
            } else {
                s.iter().flat_map(|x| x.to_le_bytes()).collect()
            };
            stdout.write_all(&bytes).unwrap();
        }
        "decode" => {
            let mut d = if only_rx {
                Decoder::with_protocols(params, &[p]).unwrap()
            } else {
                Decoder::new(params).unwrap()
            };
            let mut raw = Vec::new();
            std::io::stdin().read_to_end(&mut raw).unwrap();
            let samples: Vec<f32> = if i16s {
                raw.chunks_exact(2).map(|b| i16::from_le_bytes([b[0], b[1]]) as f32 / 32768.0).collect()
            } else {
                raw.chunks_exact(4).map(|b| f32::from_le_bytes([b[0], b[1], b[2], b[3]])).collect()
            };
            for block in samples.chunks(4 * spf) {
                if let Some(got) = d.push(block) {
                    let h: String = got.iter().map(|b| format!("{b:02x}")).collect();
                    writeln!(stdout, "{h}").unwrap();
                }
            }
        }
        _ => {
            eprintln!("usage: ggwave-rs-oracle encode|decode [options]");
            std::process::exit(2);
        }
    }
}
