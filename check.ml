exception BadFormat of string

let bad_prefix c =
  let msg = "unrecognized prefix for integer litterals:" in
  Format.asprintf "%s %c" msg c

let compare_floats s1 s2 =
  if String.length s1 = String.length s2 then s1 = s2
  else
    (* s1 is smaller than s2 *)
    let aux s1 s2 =
      let size_s1 = String.length s1 and size_s2 = String.length s2 in
      let begining = String.sub s2 0 size_s1 in
      let ending = String.sub s2 size_s1 (size_s2 - size_s1) in
      (begining = s1) &&
      (try String.iter (fun c -> if c <> '0' then raise Exit) ending;
        true
      with Exit -> false)
    in
    if String.length s1 >  String.length s2 then aux s2 s1 else aux s1 s2

(* checks if an integer litteral composed by a char prefix and a
   string radix is exactly represented by an integer constant.
   ex:
   - check_int None "32" 32 = true
   - check_int (Some 'x') "20" 32 = true
   - check_int (Some 'b') "11" 3 = true
   - check_int None "4611686018427387903" 4611686018427387903 = true
   - check_int None "4611686018427387904" -4611686018427387904 = false *)
let check_int str charopt (i:int) =
  match charopt with
  | None -> Format.asprintf "%i" i = str
  | Some ('b' | 'B') -> failwith "niy"
  | Some ('o' | 'O') -> Format.asprintf "%o" i = str
  | Some ('x' | 'X') -> Format.asprintf "%x" i = str
  | Some x   -> raise (BadFormat (bad_prefix x))

(* checks if a decimal litteral composed by a char prefix and a string
   radix is exactly represented by a float constant.
   ex:
   - check_float None "32." 32. = true
   - check_float (Some 'x') "20." 32. = true
   - check_float (Some 'o') "11." 9. = true *)
let check_float str charopt (f:float) =
  match charopt with
  | None ->
     compare_floats (Format.asprintf "%.20f" f) str
  | Some (('x'|'X') as c) ->
     compare_floats (Format.asprintf "%h" f) (Format.asprintf "%c%s" c str)
  | Some x   ->
    Format.asprintf "unrecognized prefix for float litterals: %c" x
    |> failwith

(* Separates the prefix and radix from an integer litteral *)
let format_of_int_str (i:string) =
  if String.length i <= 2 then None,i
  else
    match i.[0],i.[1] with
    | '0',(('b'|'B'|'x'|'X'|'o'|'O') as c) ->
      Some c, (String.sub i 2 ((String.length i) -2))
    | _ -> None,i

(* Separates the prefix and radix from a decimal litteral *)
let format_of_float_str (f:string) =
  if String.length f <= 2 then None,f
  else
    match f.[0],f.[1] with
    | '0',('x'|'X' as c) ->
      Some c, (String.sub f 2 ((String.length f) -2))
    | _ -> None,f

(* checks if decimal litteral can be encoded exactly as a float *)
let exact_parse_float s =
  let f = float_of_string s in
  let pre,rad = format_of_float_str s in
  check_float rad pre f

(* prints the string represent of the float built by float_of_string s *)
let print_repr_float s =
  float_of_string s |> Format.asprintf "%.32f"
