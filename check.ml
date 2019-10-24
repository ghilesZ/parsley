(* raised when a string does not match the format defined by OCaml's lexer *)
exception BadFormat of string

(* error msg utility *)
let bad_prefix c =
  let msg = "unrecognized prefix for integer litterals:" in
  Format.asprintf "%s %c" msg c

(* removes begining zeros of string *)
let trail_begining_zeros s =
  let cpt = ref 0 in
  try
    String.iter (fun c -> if c = '0' then incr cpt else raise Exit) s;
    (* only zeros in the representation *)
    "0"
  with Exit -> String.sub s !cpt (String.length s - !cpt)

(* trails leading zeros and removes '_' char *)
let normalize str =
  let remove__ s =
    let cpt = ref 0 in
    String.iter (function '_' -> incr cpt | _ -> ()) s;
    let s' = Bytes.make (String.length s - !cpt) ' ' in
    let cpt2 = ref 0 in
    String.iteri (fun i c -> if c = '_' then incr cpt2
                             else Bytes.set s' (i- !cpt2) c) s;
    Bytes.to_string s'
  in
  str |> remove__ |> trail_begining_zeros

(* Compares two string representation of an integer in the same format
   (eg both in decimal format or both in octal) and return true iff
   they are semantically equivalent. *)
let compare_ints s1 s2 =
  let s1 = normalize s1 in
  let s2 = normalize s2 in
  let size_1 = String.length s1 in
  let size_2 = String.length s2 in
  size_1 = size_2 && s1 = s2

(* Compares two string representation of a float in the same format
   (eg both in decimal format or both in hexa) and return true iff
   they are semantically equivalent.
   Two floats are semantically equivalent if:
   - TODO
   - TODO
*)
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
