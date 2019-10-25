(** the different kinds of litteral corresponding the (Pconst_int) ast node *)
type integer_kind =
  | Native (* 1n *)
  | Int32  (* 1l *)
  | Int64  (* 1L *)
  | Int    (* 1  *)

(** the different kind of notation for floats *)
type float_notation =
  | Regular      (* 100.5 *)
  | Scientific   (* 1.005e2 *)

(** the different kinds of base for litteral numbers *)
(* WARNING: floats can only be in Hexa or Decimal *)
type base_kind =
  | Hexadecimal    (* 11 *)
  | Decimal        (* 17 *)
  | Octal          (* 0o21 *)
  | Binary         (* 0b10001 *)

(** raised when a string does not match the format defined by OCaml's
   lexer for floats and integer litterals *)
exception BadFormat of string

(** error msg utility *)
let bad_prefix s =
  let msg = "unrecognized prefix for litterals:" in
  Format.asprintf "%s %s" msg s

(** error msg utility *)
let bad_suffix s =
  let msg = "unrecognized suffix for litterals:" in
  Format.asprintf "%s %c" msg s

(** reverses a string. eg, "kayak" becomes "kayak" *)
let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

(** removes begining zeros of string *)
let trail_begining_zeros s =
  let cpt = ref 0 in
  try
    String.iter (fun c -> if c = '0' then incr cpt else raise Exit) s;
    ""
  with Exit -> String.sub s !cpt (String.length s - !cpt)

(** removes ending zeros of string *)
let trail_ending_zeros s =
  s |> string_rev |> trail_begining_zeros |> string_rev

(** removes all occurences of the character '_' of a string *)
let remove__ s =
    let nb_occ = ref 0 in
    String.iter (function '_' -> incr nb_occ | _ -> ()) s;
    let s' = Bytes.make (String.length s - !nb_occ) ' ' in
    let nb_cur = ref 0 in
    String.iteri (fun i c -> if c = '_' then incr nb_cur
                            else Bytes.set s' (i- !nb_cur) c) s;
    Bytes.to_string s'

(** trails leading zeros and removes '_' char *)
let normalize_int str =
  str |> remove__ |> trail_begining_zeros

(** Compares two string representations of an integer in the same format
   (eg both in decimal format or both in octal) and return true iff
   they are semantically equivalent. *)
let compare_ints s1 s2 =
  let s1 = normalize_int s1 in
  let s2 = normalize_int s2 in
  let size_1 = String.length s1 in
  let size_2 = String.length s2 in
  (* not using (&&) to explicit the evaluation order *)
  if size_1 = size_2 then s1 = s2 else false

(** Compares two string representation of a float in the same format
   (eg both in decimal format or both in hexa) and return true iff
   they are semantically equivalent.
   Two floats are semantically equivalent if:
   - their integer part are semantically equivalent
   - their decimal parts are semantically equivalent *)
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

(** categorizes a non-empty string into an integer_kind. Returns the
   pair formed by the string where the suffixes (n,l,L) where removed
   if present, and the corresponding integer_kind *)
let categorize_repr int_str =
  let before_last_idx = String.length int_str -1 in
  match int_str.[before_last_idx] with
  | 'n' -> (String.sub int_str 0 before_last_idx), Native
  | 'l' -> (String.sub int_str 0 before_last_idx), Int32
  | 'L' -> (String.sub int_str 0 before_last_idx), Int64
  | '0'..'9' | '_' -> int_str, Int
  | x -> raise (BadFormat (bad_suffix x))

(** categorizes a non-empty string into a base. Returns the pair
   formed by the string where the prefixes (0o,0x,0b) where removed if
   present, and the corresponding base *)
let categorize_base str =
  let size = String.length str in
  if size >  2 then
    match str.[0],str.[1] with
    | '0','x' -> String.sub str 2 (size-2),Hexadecimal
    | '0','o' -> String.sub str 2 (size-2),Octal
    | '0','b' -> String.sub str 2 (size-2),Binary
    | ('0'..'9' | '_'),('0'..'9' | '_') -> str,Decimal
    | _ -> raise (BadFormat (bad_prefix str))
  else str,Decimal

(* prints the string represent of the float built by float_of_string s *)
let print_repr_float s =
  float_of_string s |> Format.asprintf "%.32f"
