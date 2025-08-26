structure Cpx where
  re : Float
  im : Float

def add (a b : Cpx) : Cpx := {
  re := a.re + b.re,
  im := a.im + b.im
}

def mul (a b : Cpx) : Cpx := {
  re := a.re*b.re - a.im*b.im,
  im := a.re*b.im + a.im*b.re
}

def norm2 (z : Cpx) := z.re*z.re + z.im*z.im

partial def mandelIter (c : Cpx) (maxIt : Nat := 80) : Nat :=
  let rec go (z : Cpx) (n : Nat) :=
    if n = maxIt then n
    else if norm2 z > 4.0 then n
    else go (add (mul z z) c) (n+1)
  go {re := 0.0, im := 0.0} 0

def mandelAscii (w h : Nat) (xmin xmax ymin ymax : Float) : String :=
  let ramp := "#@%*=+:-. "
  let rlen := ramp.length
  let ch (i j : Nat) :=
    let x := xmin + (xmax - xmin) * Float.ofNat i / Float.ofNat (w-1)
    let y := ymax - (ymax - ymin) * Float.ofNat j / Float.ofNat (h-1)
    let it := mandelIter {re := x, im := y} 80
    let idx := (it * (rlen - 1)) / 80
    ramp.get! (String.Pos.mk idx)
  let rows :=
    List.range h |>.map (fun j =>
      String.mk (List.range w |>.map (fun i => ch i j)))
  String.intercalate "\n" rows
