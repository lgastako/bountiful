def digitsToNat (s : String) : Option Nat := Id.run do
  let mut acc : Nat := 0
  for c in s.data do
    if '0' ≤ c ∧ c ≤ '9' then
      acc := acc * 10 + (c.toNat - '0'.toNat)
    else
      return none
  some acc

def parseFloatSimple (s : String) : Option Float := do
  let s := s.trim
  if s.isEmpty then none else
  let neg :=
    if s.startsWith "-" then true
    else false
  let body :=
    if s.startsWith "-" || s.startsWith "+" then s.drop 1 else s
  let parts := body.splitOn "."
  match parts with
  | [intStr] =>             -- no decimal point
      let m ← digitsToNat intStr
      some (Float.ofScientific m neg 0)
  | [intStr, fracStr] =>    -- with decimal point
      let i ← digitsToNat intStr
      let f ← digitsToNat fracStr
      let k := fracStr.length
      let mant := i * Nat.pow 10 k + f
      some (Float.ofScientific mant neg k)
  | _ => none               -- multiple dots -> reject


namespace String

def toFloat? := parseFloatSimple

end String
