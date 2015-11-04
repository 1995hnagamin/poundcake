type t = int list

let rec partition n lst = match n with
  0 -> ([], lst)
| n ->
    let (left, right) = partition (n - 1) (List.tl lst) in
    ((List.hd)::left, right)

let push value stk = value::stk

let pop stk = (List.hd stk, List.tl stk)

let top stk = List.hd stk

let rec roll count depth stk = match count with
  0 -> stk
| n ->
    let (top, bottom) = partition depth stk in
    let stk = (List.tl top) @ [List.hd top] @ bottom in
    roll (n - 1) depth stk
