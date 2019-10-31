(** Reverses a string. eg, "kayak" becomes "kayak" *)
let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

(** string_fold f a "c1c2 ... cn" is f (... (f (f a c1) c2) ...) cn. *)
let string_fold (f:'a -> char -> 'a) acc str =
  let n = String.length str - 1 in
  let rec aux acc i =
    if i > n then acc
    else aux (f acc str.[i]) (i+1)
  in aux acc 0
