(** Parsley library.

 This module provides the basic conversion operations over the
   built-in numeric types of OCaml (integers, Int32.t, Int64.t,
   Nativeint.t and floats) while indicating if the conversion was
   exact or approximated. It does it using the {{:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Result.html }
   Result } module of the standard library. *)

(** exact string_of_float: computes all the non-nul decimals of a float *)
val exact_string_of_float: float -> string

(**************************)
(** of_string conversions *)
(**************************)

val exact_int_of_string   : string -> (int, int option) Result.t
val exact_int32_of_string : string -> (Int32.t, Int32.t option) Result.t
val exact_int64_of_string : string -> (Int64.t, Int64.t option) Result.t
val exact_native_of_string: string -> (Nativeint.t, Nativeint.t option) Result.t

(** Convert the given string to a float, following OCaml's lexical
   conventions. It return 'Error(None)' if the given string is not a
   valid representation of a float, or 'Error(Some f)' if 'f' is not
   an approximated representation of the given string. It return 'Ok
   f' if f is an exact representation of the given string. *)
val exact_float_of_string : string -> (float, float option) Result.t

(*******************************)
(** integers-float conversions *)
(*******************************)


val exact_float_of_int: int -> (float,float) Result.t
val exact_int_of_float: float -> (int, int) Result.t

val exact_float_of_32: Int32.t -> (float, float) Result.t
val exact_32_of_float: float -> (Int32.t, Int32.t) Result.t

val exact_float_of_64: Int64.t -> (float, float) Result.t
val exact_64_of_float: float -> (Int64.t, Int64.t) Result.t

val exact_float_of_native: Nativeint.t -> (float, float) Result.t
val exact_native_of_float: float -> (Nativeint.t, Nativeint.t) Result.t


(*******************************)
(** cross-integers conversions *)
(*******************************)


val exact_int_of_32: Int32.t -> (int, int) Result.t
val exact_32_of_int: int -> (Int32.t, Int32.t) Result.t

val exact_int_of_64: Int64.t -> (int, int) Result.t
val exact_64_of_int: int -> (Int64.t, Int64.t) Result.t

val exact_int_of_nativeint: Nativeint.t -> (int, int) Result.t
val exact_nativeint_of_int: int -> (Nativeint.t, Nativeint.t) Result.t

val exact_32_of_64: Int64.t -> (Int32.t, Int32.t) Result.t
val exact_64_of_32: Int32.t -> (Int64.t, Int64.t) Result.t

val exact_native_of_64: Int64.t -> (Nativeint.t, Nativeint.t) Result.t
val exact_64_of_native: Nativeint.t -> (Int64.t, Int64.t) Result.t

val exact_native_of_32: Int32.t -> (Nativeint.t, Nativeint.t) Result.t
val exact_32_of_native: Nativeint.t -> (Int32.t, Int32.t) Result.t
