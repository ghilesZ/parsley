open Ast_mapper
open Parsetree

(* msg error utility *)
let litteral_error_msg lit typ =
  Format.asprintf "The litteral %s can not be exactly encoded as %s." lit typ

(* report building utility *)
let build_report str_ast result typ to_string loc =
  let open Location in
  let msg fmt =
    let msg1 = litteral_error_msg str_ast typ in
    let msg2 =
      match result with
      | Some v -> Format.asprintf "The value %s was used instead" (to_string v)
      | None -> Format.asprintf "An unknown value will be used instead"
    in Format.fprintf fmt "%s %s" msg1 msg2
  in
  {kind=Report_warning ("Parsley.warning");
   main={txt=msg;loc};
   sub=[];}

(* builds the report warning corresponding to the loss of precision *)
(* that occured during the parsing of a floatting value *)
let build_report_float str_ast str_repr loc =
  build_report str_ast str_repr "a float" (Parsley.exact_string_of_float) loc

(* builds the report warning corresponding to the loss of precision *)
(* that occured during the parsing of an integer value *)
let build_report_int str_ast str_repr loc =
  build_report str_ast str_repr "an int" string_of_int loc

let build_report_32 str_ast str_repr loc =
  build_report str_ast str_repr "an int32" (Format.asprintf "%li") loc

let build_report_64 str_ast str_repr loc =
  build_report str_ast str_repr "an int64" (Format.asprintf "%Li") loc

let build_report_native str_ast str_repr loc =
  build_report str_ast str_repr "a native int" (Format.asprintf "%ni") loc

(* Checks that floatting point are encoded exactly within a float *)
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
