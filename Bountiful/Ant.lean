import Std.Data.HashMap
import Bountiful.Term

abbrev Pos := Int × Int
inductive Dir | N | E | S | W deriving DecidableEq, Repr

instance : ToString Dir where
  toString
  | .N => "North"
  | .E => "East"
  | .S => "South"
  | .W => "West"
abbrev Cell := Bool          -- true = black, false/absent = white
abbrev Grid := Std.HashMap Pos Cell

structure Ant where
  pos : Pos
  dir : Dir
  deriving Repr

structure World where
  grid : Grid
  ant  : Ant
  deriving Repr

def turnRight : Dir → Dir
| .N => .E | .E => .S | .S => .W | .W => .N

def turnLeft : Dir → Dir
| .N => .W | .W => .S | .S => .E | .E => .N

def forward : Pos → Dir → Pos
| (x, y), .N => (x, y - 1)
| (x, y), .S => (x, y + 1)
| (x, y), .E => (x + 1, y)
| (x, y), .W => (x - 1, y)

def cellAt (g : Grid) (p : Pos) : Cell :=
  g.getD p false -- default white

def flipCell (c : Cell) : Cell := not c

def step (w : World) : World :=
  let p := w.ant.pos
  let c := cellAt w.grid p
  let dir' := if c then turnLeft w.ant.dir else turnRight w.ant.dir
  let grid' := w.grid.insert p (flipCell c)
  let pos' := forward p dir'
  { grid := grid', ant := { pos := pos', dir := dir' } }

-- Initialize a world with ant at origin facing north
def initWorld : World :=
  { grid := Std.HashMap.emptyWithCapacity 100, ant := { pos := (0, 0), dir := .N } }

-- Run n steps of the simulation
def runSteps (w : World) : Nat → World
  | 0 => w
  | n + 1 => runSteps (step w) n

-- Get bounds of the grid for display purposes
def getBounds (g : Grid) : Option (Int × Int × Int × Int) :=
  if g.isEmpty then none
  else
    let positions := g.toList.map (·.1)
    let xs := positions.map (·.1)
    let ys := positions.map (·.2)
    match xs, ys with
    | [], _ => none
    | _, [] => none
    | x::xs', y::ys' =>
      let minX := xs'.foldl min x
      let maxX := xs'.foldl max x
      let minY := ys'.foldl min y
      let maxY := ys'.foldl max y
      some (minX, minY, maxX, maxY)

-- Convert direction to character for display
def dirToChar : Dir → Char
  | .N => '^'
  | .E => '>'
  | .S => 'v'
  | .W => '<'

-- Count black cells in grid
def countBlackCells (g : Grid) : Nat :=
  g.toList.filter (fun (_, cell) => cell = true) |>.length

-- Simple text representation of world state
def worldInfo (w : World) : String :=
  s!"Ant at {w.ant.pos} facing {w.ant.dir}, Black cells: {countBlackCells w.grid}"

-- Build a single row of the grid
def buildRow (w : World) (y : Int) (width : Nat) : String :=
  let halfW : Int := width / 2
  let rec buildChars (x : Int) (remaining : Nat) (acc : String) : String :=
    if remaining = 0 then acc
    else
      let pos : Pos := (x, y)
      let char :=
        if pos = w.ant.pos then
          dirToChar w.ant.dir
        else if cellAt w.grid pos then
          '█'
        else
          '·'
      buildChars (x + 1) (remaining - 1) (acc ++ char.toString)
  buildChars (-halfW) width ""

-- Build the entire grid
def buildGrid (w : World) (width height : Nat) : String :=
  let halfH : Int := height / 2
  let rec buildRows (y : Int) (remaining : Nat) (acc : String) : String :=
    if remaining = 0 then acc
    else
      let row := buildRow w y width
      let newAcc := if acc.isEmpty then row else acc ++ "\n" ++ row
      buildRows (y + 1) (remaining - 1) newAcc
  buildRows (-halfH) height ""

-- Configurable grid display that actually uses the requested dimensions
def renderGrid (w : World) (width height : Nat) : String :=
  -- Cap dimensions at reasonable values to prevent performance issues
  let gridW := max 5 (min width 200)  -- Between 5 and 200 wide
  let gridH := max 5 (min height 60)   -- Between 5 and 60 tall
  buildGrid w gridW gridH

-- Clear screen ANSI escape sequence
def clearScreen : String := "\x1b[2J\x1b[H"

-- Finite animation loop
def animateFinite (w : World) (totalSteps : Nat) (width height : Nat) (currentStep : Nat) (delay : UInt32) : IO Unit := do
  IO.print clearScreen
  IO.println s!"🐜 Langton's Ant Simulation - Step {currentStep}/{totalSteps}"
  IO.println "========================================"
  IO.println (worldInfo w)
  IO.println ""
  IO.println (renderGrid w width height)
  IO.println ""

  if currentStep >= totalSteps then
    IO.println "✅ Simulation complete!"
    return
  else
    IO.sleep delay
    animateFinite (step w) totalSteps width height (currentStep + 1) delay
  termination_by totalSteps - currentStep + 1

-- Infinite animation loop
partial def animateInfinite (w : World) (width height : Nat) (currentStep : Nat) (delay : UInt32) : IO Unit := do
  IO.print clearScreen
  IO.println s!"🐜 Langton's Ant Simulation - Step {currentStep} (∞)"
  IO.println "========================================"
  IO.println (worldInfo w)
  IO.println ""
  IO.println (renderGrid w width height)
  IO.println ""

  IO.sleep delay
  animateInfinite (step w) width height (currentStep + 1) delay

-- Main animation dispatcher
def animate (w : World) (totalSteps : Option Nat) (width height : Nat) (currentStep : Nat := 1) (delay : UInt32) : IO Unit := do
  match totalSteps with
  | some steps => animateFinite w steps width height currentStep delay
  | none => animateInfinite w width height currentStep delay

def printHelp : IO Unit := do
  IO.println "🐜 Langton's Ant Simulation"
  IO.println ""
  IO.println "USAGE:"
  IO.println "    lake exe ant [STEPS] [OPTIONS]"
  IO.println "    lake exe ant [OPTIONS]"
  IO.println "    lake exe ant --help"
  IO.println ""
  IO.println "ARGUMENTS:"
  IO.println "    STEPS           Number of simulation steps to run (backward compatibility)"
  IO.println ""
  IO.println "OPTIONS:"
  IO.println "    --help          Show this help message and exit"
  IO.println "    --steps STEPS   Number of simulation steps to run (default: run forever)"
  IO.println "    --width WIDTH   Grid width in characters (default: auto-detect terminal width - 1)"
  IO.println "    --height HEIGHT Grid height in characters (default: auto-detect terminal height - 6)"
  IO.println ""
  IO.println "DESCRIPTION:"
  IO.println "    Simulates Langton's Ant, a cellular automaton that follows simple rules:"
  IO.println "    • On a white cell: turn right, flip cell to black, move forward"
  IO.println "    • On a black cell: turn left, flip cell to white, move forward"
  IO.println ""
  IO.println "    By default, the simulation runs forever until you press Ctrl+C. Use --steps"
  IO.println "    to limit the number of steps."
  IO.println ""
  IO.println "    The grid automatically sizes to fit your terminal. The default leaves space"
  IO.println "    for the header (4 lines) plus margins (2 lines) for height, and 1 column"
  IO.println "    margin for width."
  IO.println ""
  IO.println "    Legend:"
  IO.println "      ^ > v <  Ant facing North/East/South/West"
  IO.println "      █        Black cell"
  IO.println "      ·        White cell"
  IO.println ""
  IO.println "EXAMPLES:"
  IO.println "    lake exe ant                        # Run forever, auto-size to terminal"
  IO.println "    lake exe ant --steps 20             # Run 20 steps, auto-size"
  IO.println "    lake exe ant 100                    # Run 100 steps (backward compatibility)"
  IO.println "    lake exe ant --steps 50 --width 30  # 50 steps, custom width"
  IO.println "    lake exe ant --width 25 --height 15 # Run forever, custom dimensions"

-- Parse command line arguments
structure Config where
  steps : Option Nat  -- None means run forever
  width : Option Nat
  height : Option Nat
  delay : Option UInt32

def String.toUInt32? (s : String) : Option UInt32 :=
  s.toNat?.map UInt32.ofNat

def parseArgs (args : List String) : Config :=
  let rec parseLoop (remaining : List String) (config : Config) : Config :=
    match remaining with
    | [] => config
    | "--steps" :: s :: rest =>
      parseLoop rest { config with steps := s.toNat? }
    | "--width" :: w :: rest =>
      parseLoop rest { config with width := w.toNat? }
    | "--height" :: h :: rest =>
      parseLoop rest { config with height := h.toNat? }
    | "--delay" :: d :: rest =>
      parseLoop rest { config with delay := d.toUInt32? }
    | s :: rest =>
      if s.startsWith "--" then
        parseLoop rest config  -- Skip unknown flags
      else
        -- For backward compatibility, treat bare numbers as steps
        parseLoop rest { config with steps := s.toNat? }

  parseLoop args { steps := none, width := none, height := none, delay := some 10 }

def main (args : List String) : IO Unit := do
  -- Check for help flag
  if args.contains "--help" then
    printHelp
    return

  let config := parseArgs args

  -- Get terminal size and compute grid dimensions
  let (termHeight, termWidth) ← getTerminalSize  -- Note: Term.getTerminalSize returns (rows, cols)
  let gridWidth := config.width.getD (max 5 (termWidth - 1))
  let gridHeight := config.height.getD (max 5 (termHeight - 6))  -- Leave space for header (4 lines) + margins (2 lines)

  IO.println "🐜 Starting Langton's Ant simulation..."
  match config.steps with
  | some steps =>
    IO.println s!"Running {steps} steps on {gridWidth}×{gridHeight} grid..."
    if steps > 10 then
      IO.println "Press Ctrl+C to stop early"
  | none =>
    IO.println s!"Running forever on {gridWidth}×{gridHeight} grid..."
    IO.println "Press Ctrl+C to stop"

  IO.sleep 1000
  let delay := match config.delay with
  | some d => d
  | none => 1
  animate initWorld config.steps gridWidth gridHeight 0 delay
