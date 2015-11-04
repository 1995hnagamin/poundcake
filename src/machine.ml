type t = {
  stk: Stak.t;
  hep: Heap.t;
  stm: StaticM.t;
  pc: int
}

let update_stk s machine = {machine with stk = s}
let update_hep h machine = {machine with hep = h}
let update_stm m machine = {machine with stm = m}
let update_pc  c machine = {machine with pc  = c}
let incr_pc machine = update_pc (machine.pc + 1) machine
