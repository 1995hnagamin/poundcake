type memory = (int * int) list
type t = memory
exception Not_bound

let rec lookup id = function
  [] -> raise Not_bound
| (x, v)::env -> if id = x then v else lookup id env

let rec update id value = function
  [] -> raise Not_bound
| (x, v)::env ->
    if id = x then (x, value)::env else (x, v)::(update id value env)

let extend id value env = (id, value)::env

let alloc env =
  let (id, _) = List.hd env in
  let newid = id + 1 in
  (newid, extend newid 0 env)

