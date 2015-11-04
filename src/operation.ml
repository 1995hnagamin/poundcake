type operand =
  Push of int
| Pop
| Dup
| Roll
| Add
| Mul
| Sub
| Div
| Mod
| Greater
| Not
| Ifte
| Jez of int
| Jmp of int
| Call of int
| Return of int
| Inn
| Inc
| Outn
| Outc
| Nop
| Halt
| Malloc
| Loadh
| Storeh
| Loads of int
| Stores of int

