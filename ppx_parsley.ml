open Ast_mapper
open Parsetree

(* shortcut *)
let fas = Format.asprintf

(* error msg utilities *)
let error_msg lit typ_str =
  fas "The litteral %s can not be exactly encoded as %s" lit typ_str

(* report building utility *)
let build_report msg1 msg2 loc =
  let open Location in
  let msg fmt = Format.fprintf fmt "%s %s" msg1 msg2 in
  {kind=Report_warning ("Parsley.warning");
   main={txt=msg;loc};
   sub=[];}

(* builds the report warning corresponding to the loss of precision *)
(* that occured during the parsing of a floatting value *)
let build_report_float f f' loc =
  let msg1 = error_msg f "a float" in
  let msg2 =
    let open Parsley in
    match f' with
    | Some v -> fas "The value %s was used instead" (exact_string_of_float v)
    | None -> fas "An unknown value will be used instead"
  in  build_report msg1 msg2 loc

(* builds the report warning corresponding to the loss of precision *)
(* that occured during the parsing of an integer value *)
let build_report_int i i' loc =
  let msg1 = error_msg i "an int" in
  let msg2 =
    let open Parsley in
    match i' with
    | Some v -> fas "The value %i was used instead" v
    | None ->
       (* int_of_string fails on max_int+1 but OCaml's lexer accepts it *)
       try fas "The value %i was used instead" (Misc.Int_literal_converter.int i)
       with Failure _ -> fas "An unknown value will be used instead"
  in  build_report msg1 msg2 loc

let build_report_32 i i' loc =
  let msg1 = error_msg i "an int32" in
  let msg2 =
    let open Parsley in
    match i' with
    | Some v -> fas "The value %li was used instead" v
    | None -> fas "An unknown value will be used instead"
  in  build_report msg1 msg2 loc

let build_report_64 i i' loc =
  let msg1 = error_msg i "an int64" in
  let msg2 =
    let open Parsley in
    match i' with
    | Some v -> fas "The value %Li was used instead" v
    | None -> fas "An unknown value will be used instead"
  in  build_report msg1 msg2 loc

let build_report_native  i i' loc =
  let msg1 = error_msg i "a native int" in
  let msg2 =
    let open Parsley in
    match i' with
    | Some v -> fas "The value %ni was used instead" v
    | None -> fas "An unknown value will be used instead"
  in  build_report msg1 msg2 loc

let expr_mapper mapper _ =
  let exprf default_expr mapper = function
    | {pexp_desc = (Pexp_constant (Pconst_float(f,None))); pexp_loc;_} as x ->
       (match Parsley.exact_float_of_string f with
       | Ok f' -> x
       | Error f' ->
          let report = build_report_float f f' pexp_loc in
          Format.printf "%a" Location.print_report  report;
          x)
    | {pexp_desc=Pexp_constant(Pconst_integer(i,None)); pexp_loc;_} as x ->
       (match Parsley.exact_int_of_string i with
       | Ok i' -> x
       | Error i' ->
          let report = build_report_int i i' pexp_loc in
          Format.printf "%a" Location.print_report  report;
          x)
    | {pexp_desc=Pexp_constant(Pconst_integer(i,Some('l'))); pexp_loc;_} as x ->
       (match Parsley.exact_int32_of_string i with
       | Ok i' -> x
       | Error i' ->
          let report = build_report_32 i i' pexp_loc in
          Format.printf "%a" Location.print_report  report;
          x)
    | {pexp_desc=Pexp_constant(Pconst_integer(i,Some('L'))); pexp_loc;_} as x ->
       (match Parsley.exact_int64_of_string i with
       | Ok i' -> x
       | Error i' ->
          let report = build_report_64 i i' pexp_loc in
          Format.printf "%a" Location.print_report  report;
          x)
    | {pexp_desc=Pexp_constant(Pconst_integer(i,Some('n'))); pexp_loc;_} as x ->
       (match Parsley.exact_native_of_string i with
       | Ok i' -> x
       | Error i' ->
          let report = build_report_native i i' pexp_loc in
          Format.printf "%a" Location.print_report  report;
          x)
    |  x -> default_expr mapper x
  in
  {mapper with expr = (exprf mapper.expr)}

(* mapper composition *)
let build_mapper mappers : string list -> Ast_mapper.mapper =
  fun x ->
  List.fold_left (fun acc newmapper ->
       (newmapper acc x)
     ) default_mapper mappers

(* entry point *)
let () =
  build_mapper [expr_mapper] |> register "parsley"
