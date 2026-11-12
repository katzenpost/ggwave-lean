import Ggwave.GF256

/-!
Reed-Solomon over GF(2^8), ported from the `reed-solomon` code bundled with C
ggwave (Mike Lubinets, MIT), by way of `src/reed_solomon.rs` in the Rust crate.

A `Code` fixes the message and parity lengths, and blocks are vectors of that
length. Polynomials are stored highest degree first, as in the C code.
Decoding follows the C algorithm step by step, including that it trusts the
Berlekamp-Massey result without re-checking syndromes, so it accepts and
rejects exactly the same blocks as C ggwave.

Only errors at unknown positions are corrected. ggwave never passes erasures,
and the C library's erasure path fails on erasures alone, so it is not ported.
-/

namespace Ggwave.ReedSolomon

open GF256 (pow inverse)

/-- A polynomial, highest degree first. -/
abbrev Poly := Array GF256

def polyScale (p : Poly) (x : GF256) : Poly := p.map (· * x)

def polyAdd (p q : Poly) : Poly := Id.run do
  let n := max p.size q.size
  let mut r : Poly := Array.replicate n 0
  for i in [0:p.size] do
    r := r.set! (i + n - p.size) p[i]!
  for i in [0:q.size] do
    let j := i + n - q.size
    r := r.set! j (r[j]! + q[i]!)
  return r

def polyMul (p q : Poly) : Poly := Id.run do
  let mut r : Poly := Array.replicate (p.size + q.size - 1) 0
  for j in [0:q.size] do
    for i in [0:p.size] do
      r := r.set! (i + j) (r[i + j]! + p[i]! * q[j]!)
  return r

def polyEval (p : Poly) (x : GF256) : GF256 := Id.run do
  let mut y := p[0]!
  for i in [1:p.size] do
    y := y * x + p[i]!
  return y

/-- Number of ECC bytes ggwave appends to a payload of `len` bytes. -/
def eccBytesForLength (len : Nat) : Nat :=
  if len < 4 then 2 else max (2 * (len / 5)) 4

/-- A Reed-Solomon code: blocks of `msgLen` data symbols followed by `eccLen`
parity symbols, at most 255 symbols in all. -/
structure Code where
  msgLen : Nat
  eccLen : Nat
  blockLen_lt : msgLen + eccLen < 256

abbrev Code.blockLen (c : Code) : Nat := c.msgLen + c.eccLen
/-- The data part of a block. -/
abbrev Message (c : Code) := Vector GF256 c.msgLen
/-- A whole block: message then parity. -/
abbrev Block (c : Code) := Vector GF256 c.blockLen

theorem payload_blockLen_lt (len : Nat) (h : len ≤ 140) : len + eccBytesForLength len < 256 :=
  have all : ∀ n < 141, n + eccBytesForLength n < 256 := by decide
  all len (by omega)

/-- The code ggwave protects a `len`-byte payload with. -/
def Code.payload (len : Nat) (h : len ≤ 140) : Code :=
  ⟨len, eccBytesForLength len, payload_blockLen_lt len h⟩

/-- The code of the one-byte length header sent before variable-length payloads. -/
def Code.lengthHeader : Code := ⟨1, 2, by decide⟩

def generator (eccLen : Nat) : Poly := Id.run do
  let mut gen : Poly := #[1]
  for i in [0:eccLen] do
    gen := polyMul gen #[1, pow 2 i]
  return gen

/-- Systematic encoding: the message followed by its parity. -/
def encode (c : Code) (msg : Message c) : Block c := Id.run do
  let gen := generator c.eccLen
  let mut out := msg.toArray ++ Array.replicate c.eccLen 0
  for i in [0:c.msgLen] do
    let coef := out[i]!
    if coef != 0 then
      for j in [1:gen.size] do
        out := out.set! (i + j) (out[i + j]! + gen[j]! * coef)
  -- the division above overwrote the message part; only the remainder is parity
  return msg ++ Vector.ofFn fun (i : Fin c.eccLen) => out[c.msgLen + i]!

-- Berlekamp-Massey, as in RS::ReedSolomon::FindErrorLocator without erasures.
def findErrorLocator (synd : Poly) (eccLen : Nat) : Option Poly := Id.run do
  let mut errLoc : Poly := #[1]
  let mut oldLoc : Poly := #[1]
  for k in [0:eccLen] do
    let mut delta := synd[k]!
    for j in [1:errLoc.size] do
      delta := delta + errLoc[errLoc.size - j - 1]! * synd[k - j]!
    oldLoc := oldLoc.push 0
    if delta != 0 then
      if oldLoc.size > errLoc.size then
        let newLoc := polyScale oldLoc delta
        oldLoc := polyScale errLoc (inverse delta)
        errLoc := newLoc
      errLoc := polyAdd errLoc (polyScale oldLoc delta)
  let shift := (errLoc.toList.takeWhile (· == 0)).length
  if shift == errLoc.size then return none
  let errs := errLoc.size - shift - 1
  if errs * 2 > eccLen then return none
  return some (errLoc.extract shift errLoc.size)

def correctErrata (synd : Poly) (errPos : Array Nat) (msgIn : Poly) : Poly := Id.run do
  let coefPos := errPos.map (msgIn.size - 1 - ·)
  let mut errataLoc : Poly := #[1]
  for p in coefPos do
    errataLoc := polyMul errataLoc #[pow 2 p, 1]
  -- Error evaluator: (reversed syndromes * errata locator) mod x^(nerr + 1).
  let product := polyMul synd.reverse errataLoc
  let reEval := product.extract (product.size - errataLoc.size) product.size
  let x := coefPos.map fun (p : Nat) => pow 2 (-(255 - (p : Int)))
  let mut e : Poly := Array.replicate msgIn.size 0
  for i in [0:x.size] do
    let xi := x[i]!
    let xiInv := inverse xi
    let mut errLocPrime : GF256 := 1
    for j in [0:x.size] do
      if j != i then
        errLocPrime := errLocPrime * (1 - xiInv * x[j]!)
    let y := pow xi 1 * polyEval reEval xiInv
    e := e.set! errPos[i]! (y / errLocPrime)
  return polyAdd msgIn e

/-- The corrected message, or `none` when C ggwave would report failure. -/
def decode (c : Code) (block : Block c) : Option (Message c) := do
  let src := block.toArray
  -- synd[0] = 0, synd[i] = msg(alpha^(i-1))
  let synd : Poly := #[0] ++ (Array.range c.eccLen).map fun (i : Nat) => polyEval src (pow 2 i)
  let corrected ← if synd.all (· == 0) then pure src else do
    -- No erasures: the Forney syndromes are the syndromes without the leading 0.
    let reloc := (← findErrorLocator (synd.extract 1 synd.size) c.eccLen).reverse
    let n := src.size
    let errPos := (Array.range n).filter (fun (i : Nat) => polyEval reloc (pow 2 i) == 0) |>.map (n - 1 - ·)
    guard (errPos.size == reloc.size - 1 && !errPos.isEmpty)
    pure (correctErrata synd errPos src)
  return Vector.ofFn fun i => corrected[i.val]!

/-! Byte views, for callers holding `ByteArray`s. -/

/-- `n` bytes of `b` from `start` as field elements. -/
def ofBytes (b : ByteArray) (start n : Nat) : Vector GF256 n := Vector.ofFn fun i => ⟨b[start + i]!⟩

def toBytes (v : Vector GF256 n) : ByteArray := ⟨v.toArray.map (·.val)⟩

end Ggwave.ReedSolomon
