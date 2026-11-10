/-!
Reed-Solomon over GF(2^8) (primitive polynomial 0x11d), ported from the
`reed-solomon` code bundled with C ggwave (Mike Lubinets, MIT), by way of
`src/reed_solomon.rs` in the Rust crate.

Polynomials are stored highest degree first, as in the C code. Decoding
follows the C algorithm step by step, including that it trusts the
Berlekamp-Massey result without re-checking syndromes, so it accepts and
rejects exactly the same inputs as C ggwave.
-/

namespace Ggwave.ReedSolomon

/-- `(exp, log)` tables; `exp` is doubled to 512 entries so products need no reduction. -/
def tables : ByteArray × ByteArray := Id.run do
  let mut exp := ByteArray.mk (Array.replicate 512 0)
  let mut log := ByteArray.mk (Array.replicate 256 0)
  let mut x : Nat := 1
  for i in [0:255] do
    exp := exp.set! i x.toUInt8
    log := log.set! x i.toUInt8
    x := x <<< 1
    if x &&& 0x100 != 0 then
      x := x ^^^ 0x11d
  for i in [255:512] do
    exp := exp.set! i exp[i - 255]!
  return (exp, log)

def expTable : ByteArray := tables.1
def logTable : ByteArray := tables.2

@[inline] def exp (i : Nat) : UInt8 := expTable[i]!
@[inline] def log (x : UInt8) : Nat := logTable[x.toNat]!.toNat

def mul (x y : UInt8) : UInt8 :=
  if x == 0 || y == 0 then 0 else exp (log x + log y)

-- C asserts y != 0; with assertions off it uses log[0] = 0, which we mirror.
def div (x y : UInt8) : UInt8 :=
  if x == 0 then 0 else exp ((log x + 255 - log y) % 255)

def pow (x : UInt8) (power : Int) : UInt8 :=
  exp ((log x * power) % 255).toNat

def inverse (x : UInt8) : UInt8 := exp (255 - log x)

def polyScale (p : ByteArray) (x : UInt8) : ByteArray := Id.run do
  let mut r := p
  for i in [0:p.size] do
    r := r.set! i (mul p[i]! x)
  return r

def polyAdd (p q : ByteArray) : ByteArray := Id.run do
  let n := max p.size q.size
  let mut r := ByteArray.mk (Array.replicate n 0)
  for i in [0:p.size] do
    r := r.set! (i + n - p.size) p[i]!
  for i in [0:q.size] do
    let j := i + n - q.size
    r := r.set! j (r[j]! ^^^ q[i]!)
  return r

def polyMul (p q : ByteArray) : ByteArray := Id.run do
  let mut r := ByteArray.mk (Array.replicate (p.size + q.size - 1) 0)
  for j in [0:q.size] do
    for i in [0:p.size] do
      r := r.set! (i + j) (r[i + j]! ^^^ mul p[i]! q[j]!)
  return r

def polyEval (p : ByteArray) (x : UInt8) : UInt8 := Id.run do
  let mut y := p[0]!
  for i in [1:p.size] do
    y := mul y x ^^^ p[i]!
  return y

/-- Number of ECC bytes ggwave appends to a payload of `len` bytes. -/
def eccBytesForLength (len : Nat) : Nat :=
  if len < 4 then 2 else max (2 * (len / 5)) 4

def generator (eccLen : Nat) : ByteArray := Id.run do
  let mut gen := ByteArray.mk #[1]
  for i in [0:eccLen] do
    gen := polyMul gen ⟨#[1, pow 2 i]⟩
  return gen

/-- Systematic encoding: `msg` followed by `eccLen` parity bytes. -/
def encode (msg : ByteArray) (eccLen : Nat) : ByteArray := Id.run do
  let gen := generator eccLen
  let mut out := msg ++ ByteArray.mk (Array.replicate eccLen 0)
  for i in [0:msg.size] do
    let coef := out[i]!
    if coef != 0 then
      for j in [1:gen.size] do
        out := out.set! (i + j) (out[i + j]! ^^^ mul gen[j]! coef)
  -- the division above overwrote the message part; only the remainder is parity
  return msg.copySlice 0 out 0 msg.size

-- Berlekamp-Massey, as in RS::ReedSolomon::FindErrorLocator without erasures.
def findErrorLocator (synd : ByteArray) (eccLen : Nat) : Option ByteArray := Id.run do
  let mut errLoc := ByteArray.mk #[1]
  let mut oldLoc := ByteArray.mk #[1]
  for k in [0:eccLen] do
    let mut delta := synd[k]!
    for j in [1:errLoc.size] do
      delta := delta ^^^ mul errLoc[errLoc.size - j - 1]! synd[k - j]!
    oldLoc := oldLoc.push 0
    if delta != 0 then
      if oldLoc.size > errLoc.size then
        let newLoc := polyScale oldLoc delta
        oldLoc := polyScale errLoc (inverse delta)
        errLoc := newLoc
      errLoc := polyAdd errLoc (polyScale oldLoc delta)
  let shift := (errLoc.data.toList.takeWhile (· == 0)).length
  if shift == errLoc.size then return none
  let errs := errLoc.size - shift - 1
  if errs * 2 > eccLen then return none
  return some (errLoc.extract shift errLoc.size)

def reverse (p : ByteArray) : ByteArray := ⟨p.data.reverse⟩

def correctErrata (synd : ByteArray) (errPos : Array Nat) (msgIn : ByteArray) : ByteArray := Id.run do
  let coefPos := errPos.map (msgIn.size - 1 - ·)
  let mut errataLoc := ByteArray.mk #[1]
  for p in coefPos do
    errataLoc := polyMul errataLoc ⟨#[pow 2 p, 1]⟩
  -- Error evaluator: (reversed syndromes * errata locator) mod x^(nerr + 1).
  let product := polyMul (reverse synd) errataLoc
  let reEval := product.extract (product.size - errataLoc.size) product.size
  let x := coefPos.map fun (p : Nat) => pow 2 (-(255 - (p : Int)))
  let mut e := ByteArray.mk (Array.replicate msgIn.size 0)
  for i in [0:x.size] do
    let xi := x[i]!
    let xiInv := inverse xi
    let mut errLocPrime : UInt8 := 1
    for j in [0:x.size] do
      if j != i then
        errLocPrime := mul errLocPrime (1 ^^^ mul xiInv x[j]!)
    let y := mul (pow xi 1) (polyEval reEval xiInv)
    e := e.set! errPos[i]! (div y errLocPrime)
  return polyAdd msgIn e

/-- Decodes `src` (message followed by `eccLen` parity bytes) and returns the
corrected message, or `none` when C ggwave would report failure. -/
def decode (src : ByteArray) (eccLen : Nat) : Option ByteArray := do
  let msgLen := src.size - eccLen
  -- synd[0] = 0, synd[i] = msg(alpha^(i-1))
  let mut synd := ByteArray.mk #[0]
  for i in [1:eccLen + 1] do
    synd := synd.push (polyEval src (pow 2 (i - 1 : Nat)))
  if synd.data.all (· == 0) then
    return src.extract 0 msgLen
  -- No erasures: the Forney syndromes are the syndromes without the leading 0.
  let eloc ← findErrorLocator (synd.extract 1 synd.size) eccLen
  let reloc := reverse eloc
  let n := src.size
  let errs := reloc.size - 1
  let mut errPos : Array Nat := #[]
  for i in [0:n] do
    if polyEval reloc (pow 2 i) == 0 then
      errPos := errPos.push (n - 1 - i)
  guard (errPos.size == errs && !errPos.isEmpty)
  return (correctErrata synd errPos src).extract 0 msgLen

end Ggwave.ReedSolomon
