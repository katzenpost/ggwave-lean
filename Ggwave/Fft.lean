import Ggwave.Basic

/-!
Power spectrum of a real frame, laid out the way C ggwave's decoder sees it
(`src/fft.rs` in the Rust crate). An iterative radix-2 FFT in `Float` (f64);
C and Rust work in f32, which only moves the decoder's decisions on audio
right at a threshold.
-/

namespace Ggwave

/-- Twiddle factors and bit-reversal permutation for an `n`-point FFT. -/
structure Fft where
  n : Nat
  cos : FloatArray
  sin : FloatArray
  rev : Array Nat
  deriving Inhabited

def pi : Float := 3.141592653589793

/-- `i` with its low `bits` bits reversed. -/
def reverseBits (i bits : Nat) : Nat := Id.run do
  let mut r := 0
  for b in [0:bits] do
    r := (r <<< 1) ||| ((i >>> b) &&& 1)
  return r

/-- `n` must be a power of two. -/
def Fft.new (n : Nat) : Fft := Id.run do
  let bits := n.log2
  let mut cos := FloatArray.emptyWithCapacity (n / 2)
  let mut sin := FloatArray.emptyWithCapacity (n / 2)
  for k in [0:n / 2] do
    let a := -2 * pi * k.toFloat / n.toFloat
    cos := cos.push a.cos
    sin := sin.push a.sin
  return { n, cos, sin, rev := (Array.range n).map (reverseBits · bits) }

/-- |X_k|^2 at index `k` for `0 < k < n/2`, the DC and Nyquist powers summed at
index 0 (Ooura packing), and zero above n/2: what ggwave.cpp computes from its
`rdft` output. `input` must have `f.n` samples. -/
def Fft.power (f : Fft) (input : FloatArray) : FloatArray := Id.run do
  let n := f.n
  let mut re := zeros n
  for i in [0:n] do
    re := re.set! f.rev[i]! input[i]!
  let mut im := zeros n
  for s in [0:n.log2] do
    let len := 2 <<< s
    let half := len / 2
    let step := n / len
    for blk in [0:n / len] do
      let start := blk * len
      for k in [0:half] do
        let wr := f.cos[k * step]!
        let wi := f.sin[k * step]!
        let a := start + k
        let b := a + half
        let (rb, ib) := (re[b]!, im[b]!)
        let tr := rb * wr - ib * wi
        let ti := rb * wi + ib * wr
        let (ra, ia) := (re[a]!, im[a]!)
        re := (re.set! b (ra - tr)).set! a (ra + tr)
        im := (im.set! b (ia - ti)).set! a (ia + ti)
  let mut spectrum := FloatArray.emptyWithCapacity n
  spectrum := spectrum.push (re[0]! * re[0]! + re[n / 2]! * re[n / 2]!)
  for k in [1:n / 2] do
    spectrum := spectrum.push (re[k]! * re[k]! + im[k]! * im[k]!)
  for _ in [n / 2:n] do
    spectrum := spectrum.push 0
  return spectrum

end Ggwave
