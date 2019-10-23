open Ast_mapper
open Parsetree

let extend_mapper_with_expr mapper exprf =
  {mapper with expr = exprf}


let build_report_float str_ast str_repr loc =
  let open Location in
  let msg fmt =
    let msg1=Format.asprintf "The litteral %s can not be exactly encoded as a float." str_ast in
    let msg2=Format.asprintf "The float %s was used instead" str_repr in
    Format.fprintf fmt "%s %s" msg1 msg2
  in
  {kind=Report_warning ("Parsley.warning");
   main={txt=msg;loc};
   sub=[];
  }

let expr_mapper mapper _ =
  let exprf default_expr mapper = function
    | {pexp_desc = (Pexp_constant (Pconst_float(s,None))); pexp_loc;_} as x ->
       if not (Check.exact_parse_float s) then begin
           let repr = Check.print_repr_float s in
           let report = build_report_float s repr  pexp_loc in
           Format.printf "%a" Location.print_report  report
         end;
       x
    |  x -> default_expr mapper x
  in
  extend_mapper_with_expr mapper (exprf mapper.expr)

(* mapper composition *)
let build_mapper mappers : string list -> Ast_mapper.mapper =
  fun x ->
  List.fold_left (fun acc newmapper ->
       (newmapper acc x)
     ) default_mapper mappers

let () =
  build_mapper [expr_mapper] |> register "parsley"
