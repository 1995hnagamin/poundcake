type t = StaticM.memory

let lookup = StaticM.lookup
let update = StaticM.update
let extend = StaticM.extend

let alloc env =
  let (id, _) = List.hd env in
  let newid = id + 1 in
  (newid, extend newid 0 env)
