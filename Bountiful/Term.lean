import Std

open IO

def getTerminalSize : IO (Nat × Nat) := do
  let out ← IO.Process.output {
    cmd  := "stty"
    args := #["-f", "/dev/tty", "size"]
  }
  if out.exitCode == 0 then
    match out.stdout.trim.splitOn " " with
    | [rows, cols] =>
      match (rows.toNat?, cols.toNat?) with
      | (some r, some c) => return (r, c)
      | _ => return (25, 80)
    | _ => return (25, 80)
  else
    return (25, 80)
