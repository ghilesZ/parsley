open Utils

(* the different kinds of base for litteral numbers *)
(* WARNING: floats can only be in Hexa or Decimal *)
type base_kind =
  | Hexadecimal    (* 11 *)
  | Decimal        (* 17 *)
  | Octal          (* 0o21 *)
  | Binary         (* 0b10001 *)

(* raised when a string does not match the format defined by OCaml's
   lexer for floats and integer litterals *)
exception BadFormat of string

(* error msg utility *)
let bad_prefix s =
  let msg = "unrecognized prefix for litterals:" in
  Format.asprintf "%s %s" msg s

(* Removes all occurences of the character '_'  *)
let remove__ s =
  let nb_occ = ref 0 in
  String.iter (function '_' -> incr nb_occ | _ -> ()) s;
  let s' = Bytes.make (String.length s - !nb_occ) ' ' in
  let nb_cur = ref 0 in
  String.iteri (fun i c -> if c = '_' then incr nb_cur
                           else Bytes.set s' (i- !nb_cur) c) s;
  Bytes.to_string s'

(* categorizes a non-empty string into a base. Returns the pair
   formed by the string where the prefixes (0o,0x,0b) where removed if
   present, and the corresponding base *)
let categorize_base str =
  let size = String.length str in
  if size > 2 then
    match str.[0],str.[1] with
    | '0',('x'|'X') -> String.sub str 2 (size-2),Hexadecimal
    | '0',('o'|'O') -> String.sub str 2 (size-2),Octal
    | '0',('b'|'B') -> String.sub str 2 (size-2),Binary
    | ('0'..'9' | '_'),('0'..'9' | '_' | '.') -> str,Decimal
    | _ -> raise (BadFormat (bad_prefix str))
  else str,Decimal

let i_of_char = function
  | '0'..'9' as x -> int_of_char x - 48
  | 'a'..'f' as x -> int_of_char x - 87
  | 'A'..'F' as x -> int_of_char x - 55
  | c -> failwith (Format.asprintf "%c is not a valid numerical character" c)

let q_of_char x = i_of_char x |> Q.of_int

let int_of_base = function
  | Hexadecimal -> 16
  | Decimal -> 10
  | Octal -> 8
  | Binary -> 2

(* Computes exactly the rational corresponding to the litteral in the
   given base. Works for both integers and floats, for all bases *)
let parse_base b lit =
  let q_base = Q.of_int b in
  let size_lit = String.length lit in
  let integer_part,decimal_part =
    try
      let dot =  String.index lit '.' in
      (String.sub lit 0 dot),(String.sub lit (dot+1) (size_lit-dot-1))
    with Not_found -> lit,""
  in
  let i,_ = string_fold (fun (acc,i) c ->
                (Q.add (Q.mul (q_of_char c) i) acc),(Q.mul i q_base)
              ) (Q.zero,Q.one) (string_rev integer_part) in
  let d,_ = string_fold (fun (acc,i) c ->
                (Q.add (Q.div (q_of_char c) i) acc),(Q.mul i q_base)
              ) (Q.zero,q_base) decimal_part in
  Q.add i d

(* Computes exactly the rational corresponding to the litteral in the
   given base. Works for both integers and floats, for all bases, and
   for both scientific and regular notation *)
let parse_mant_exp b lit =
  let pred_c = match b with
  | Hexadecimal -> (function 'P' | 'p' -> true | _ -> false)
  | Decimal -> (function 'E' | 'e' -> true | _ -> false)
  | _ -> (fun _ -> false)
  in
  match split_on lit pred_c with
  | [mant;exp] ->
     let i_base = int_of_base b in
     let mant = parse_base i_base mant in
     let exp = parse_base i_base exp in
     let exp_int = Q.to_int exp in
     let exp_z = Z.pow (Z.of_int i_base) exp_int in
     Q.mul mant (Q.of_bigint exp_z)
  | [regular] -> parse_base (int_of_base b) regular
  | _ -> assert false

let positive_rat_of_string litteral =
  let str,base = categorize_base litteral in
  let normalized_litteral = remove__ str in
  parse_mant_exp base normalized_litteral

let rat_of_string lit =
  if lit.[0] = '-' then
    Q.neg (positive_rat_of_string (String.sub lit 1 (String.length lit -1)))
  else (positive_rat_of_string lit)

(* builds an 'of_string' that convert a string representation of a
   value of a numeric type, to its corresponding value, while
   indicating if a loss of precision occured during the conversion *)
let build_os num_of_string q_of_num x =
  let i = num_of_string x in
  let r = rat_of_string x in
  match i with
  | None -> Error i
  | Some i ->
     let ir = q_of_num i in
     if ir = r then Ok i
     else Error (Some i)

(* generic building of exact conversion *)
let exact a_of_b rat_of_a rat_of_b =
  fun x ->
  let a = a_of_b x in
  let ra = rat_of_a a in
  let rb = rat_of_b x in
  if rb = ra then Ok a else Error a

(* (\* of_string conversions *\)
 * let exact_int_of_string    = build_os int_of_string_opt Q.of_int
 * let exact_int32_of_string  = build_os Int32.of_string_opt Q.of_int32
 * let exact_int64_of_string  = build_os Int64.of_string_opt Q.of_int64
 * let exact_native_of_string = build_os Nativeint.of_string_opt Q.of_nativeint *)
let exact_float_of_string  = build_os float_of_string_opt Q.of_float

(* cross-integers conversions *)
let exact_int_of_32 = exact Int32.to_int Z.of_int Z.of_int32
let exact_32_of_int = exact Int32.of_int Z.of_int32 Z.of_int

let exact_int_of_64 = exact Int64.to_int Z.of_int Z.of_int64
let exact_64_of_int = exact Int64.of_int Z.of_int64 Z.of_int

let exact_int_of_nativeint = exact Nativeint.to_int Z.of_int Z.of_nativeint
let exact_nativeint_of_int = exact Nativeint.of_int Z.of_nativeint Z.of_int

let exact_32_of_64 = exact Int64.to_int32 Z.of_int32 Z.of_int64
let exact_64_of_32 = exact Int64.of_int32 Z.of_int64 Z.of_int32

let exact_native_of_64 = exact Int64.to_nativeint Z.of_nativeint Z.of_int64
let exact_64_of_native = exact Int64.of_nativeint Z.of_int64 Z.of_nativeint

let exact_native_of_32 = exact Nativeint.of_int32 Z.of_nativeint Z.of_int32
let exact_32_of_native = exact Nativeint.to_int32 Z.of_int32 Z.of_nativeint

(* integers-float conversions *)
let exact_float_of_int = exact float_of_int Q.of_float Q.of_int
let exact_int_of_float = exact int_of_float Q.of_int Q.of_float

let exact_32_of_float = exact Int32.of_float Q.of_int32 Q.of_float

let exact_float_of_64 = exact Int64.to_float Q.of_float Q.of_int64
let exact_64_of_float = exact Int64.of_float Q.of_int64 Q.of_float

let exact_float_of_native = exact Nativeint.to_float Q.of_float Q.of_nativeint
let exact_native_of_float = exact Nativeint.of_float Q.of_nativeint Q.of_float

(* exactish string_of_float : assuming a float will never have a
   non-nul digit after the 1000th digit after the dot *)
let exact_string_of_float f =
  (* overkill precision than trail ending zeros *)
  Format.asprintf "%.1000f" f |> trail_ending_zeros
