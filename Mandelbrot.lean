import Bountiful.Term
import Bountiful.String

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

def mandelAscii (w h : Nat) (xmin xmax ymin ymax : Float) (maxIter : Nat) : String :=
  let ramp := "#@%*=+:-. "
  let rlen := ramp.length
  let ch (i j : Nat) :=
    let x := xmin + (xmax - xmin) * Float.ofNat i / Float.ofNat (w-1)
    let y := ymax - (ymax - ymin) * Float.ofNat j / Float.ofNat (h-1)
    let it := mandelIter {re := x, im := y} maxIter
    let idx := (it * (rlen - 1)) / maxIter
    ramp.get! (String.Pos.mk idx)
  let rows :=
    List.range h |>.map (fun j =>
      String.mk (List.range w |>.map (fun i => ch i j)))
  String.intercalate "\n" rows

def printHelp : IO Unit := do
  IO.println "🌊 Mandelbrot Set Renderer"
  IO.println ""
  IO.println "USAGE:"
  IO.println "    lake exe mandelbrot [OPTIONS]"
  IO.println "    lake exe mandelbrot --help"
  IO.println ""
  IO.println "OPTIONS:"
  IO.println "    --help          Show this help message and exit"
  IO.println "    --width WIDTH   Output width in characters (default: auto-detect terminal width)"
  IO.println "    --height HEIGHT Output height in characters (default: auto-detect terminal height - 2)"
  IO.println "    --xmin XMIN     Left boundary of x range (default: -2.0)"
  IO.println "    --xmax XMAX     Right boundary of x range (default: 1.0)"
  IO.println "    --ymin YMIN     Bottom boundary of y range (default: -1.0)"
  IO.println "    --ymax YMAX     Top boundary of y range (default: 1.0)"
  IO.println "    --iter ITER     Maximum iterations per point (default: 80)"
  IO.println ""
  IO.println "DESCRIPTION:"
  IO.println "    Renders the Mandelbrot set as ASCII art. The Mandelbrot set is a"
  IO.println "    fractal defined by the set of complex numbers c for which the"
  IO.println "    function f(z) = z² + c does not diverge when iterated from z = 0."
  IO.println ""
  IO.println "    The output uses different characters to represent the number of"
  IO.println "    iterations before divergence, creating a visual representation"
  IO.println "    of the set's structure."
  IO.println ""
  IO.println "    Legend (from most to least iterations):"
  IO.println "      # @ % * = + : - . (space)"
  IO.println ""
  IO.println "EXAMPLES:"
  IO.println "    lake exe mandelbrot                           # Default view, auto-size"
  IO.println "    lake exe mandelbrot --width 80 --height 24   # Custom dimensions"
  IO.println "    lake exe mandelbrot --xmin -0.5 --xmax 0.5  # Zoom to center"
  IO.println "    lake exe mandelbrot --iter 200               # Higher quality"

structure Config where
  width : Option Nat
  height : Option Nat
  xmin : Option Float
  xmax : Option Float
  ymin : Option Float
  ymax : Option Float
  iter : Option Nat

def parseArgs (args : List String) : Config :=
  let rec parseLoop (remaining : List String) (config : Config) : Config :=
    match remaining with
    | [] => config
    | "--width" :: w :: rest =>
      parseLoop rest { config with width := w.toNat? }
    | "--height" :: h :: rest =>
      parseLoop rest { config with height := h.toNat? }
    | "--xmin" :: x :: rest =>
      parseLoop rest { config with xmin := x.toFloat? }
    | "--xmax" :: x :: rest =>
      parseLoop rest { config with xmax := x.toFloat? }
    | "--ymin" :: y :: rest =>
      parseLoop rest { config with ymin := y.toFloat? }
    | "--ymax" :: y :: rest =>
      parseLoop rest { config with ymax := y.toFloat? }
    | "--iter" :: i :: rest =>
      parseLoop rest { config with iter := i.toNat? }
    | _ :: rest =>
      parseLoop rest config

  parseLoop args {
    width := none,
    height := none,
    xmin := none,
    xmax := none,
    ymin := none,
    ymax := none,
    iter := none
  }

def main (args : List String) : IO Unit := do
  if args.contains "--help" then
    printHelp
    return

  let config := parseArgs args

  let (termHeight, termWidth) ← getTerminalSize
  let gridWidth := config.width.getD (max 10 termWidth)
  let gridHeight := config.height.getD (max 5 (termHeight - 2))

  let xmin := config.xmin.getD (-2.0)
  let xmax := config.xmax.getD 1.0
  let ymin := config.ymin.getD (-1.0)
  let ymax := config.ymax.getD 1.0
  let maxIter := config.iter.getD 80

  IO.println "🌊 Rendering Mandelbrot set..."
  IO.println s!"Dimensions: {gridWidth}×{gridHeight}"
  IO.println s!"X range: [{xmin}, {xmax}]"
  IO.println s!"Y range: [{ymin}, {ymax}]"
  IO.println s!"Max iterations: {maxIter}"
  IO.println ""

  let result := mandelAscii gridWidth gridHeight xmin xmax ymin ymax maxIter
  IO.println result
