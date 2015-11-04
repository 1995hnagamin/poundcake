open Operation
open Machine

let pop2 machine =
  let (y, stk) = Stak.pop machine.stk in
  let (x, stk) = Stak.pop stk in
  (x, y, stk)

let exec_stk stk machine =
  incr_pc (update_stk s m)

let exec m = function
  Push n ->
    let s = Stak.push n m.stk in
    exec_stk s m
| Pop ->
    let (_, s) = Stak.pop m.stk in
    exec_stk s m
| Dup ->
    let s = Stak.push (Stak.top s) m.stk in
    exec_stk s m
| Roll ->
    let (depth, count, s) = pop2 m in
    let s = Stak.roll count depth s in
    exec_stk s m
| Add ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (x + y) s in
    exec_stk s m
| Mul ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (x * y) s in
    exec_stk s m
| Sub ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (x - y) s in
    exec_stk s m
| Div ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (x / y) s in
    exec_stk s m
| Mod ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (x mod y) s in
    exec_stk s m
| Greater ->
    let (x, y, s) = pop2 m in
    let s = Stak.push (if x > y then 1 else 0) s in
    exec_stk s m
| Not ->
    let (x, s) = Stak.pop m.stk in
    let s = Stak.push (if x = 0 then 1 else 0) s in
    exec_stk s m
| Ifte ->
    let (p, s) = Stak.pop m.stk in
    let (x, y, s) = pop2 s in
    let s = Stak.push (if p <> 0 then x else y) s in
    exec_stk s m
| Jez diff ->
    let (x, s) = Stak.pop m.stk in
    let pc = if x = 0 then m.pc + diff else m.pc in
    update_pc pc m
| Jmp diff ->
    update_pc (m.pc + diff) m
| Call diff ->
    let s = Stak.push m.pc s in
    update_pc (m.pc + diff) m
| Return diff ->
    let (addr, s) = Stak.pop m.stk in
    let addr = addr + diff in
    update_pc (m.pc + addr) m
| Inn ->
    let n = read_int () in
    update_stk (Stak.push n s) m
| Inc ->
    let c = input_char(stdin) in
