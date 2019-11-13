(* testing utilities *)
let ex = function
  | Ok _ -> true
  | Error _ -> false

let nex x = not (ex x)

(* implication and equivalence, useful for testing *)
let ( => ) a b = if a then b else true
let ( <=> ) a b = a => b && b => a

(*useful constants *)
let two_32 = Int32.max_int |> Int64.of_int32
let mtwo_32 = Int32.min_int |> Int64.of_int32
let two_64f = Int64.max_int |> Int64.to_float
let mtwo_64f = Int64.min_int |> Int64.to_float

let range_32 = two_32, mtwo_32
let range_64_f = two_64f, mtwo_64f

let in_range x (a,b) = a<=x && x<=b
let out_of_range x r = not (in_range x r)

let float_of_int =
  QCheck.Test.make ~count:1000 ~name:"float_of_int conversions"
    QCheck.int (fun i -> nex (Parsley.exact_float_of_int i) =>
                           (i > 9007199254740992 || i < -9007199254740993))

let int_of_float =
  QCheck.Test.make ~count:1000 ~name:"int_of_float conversions"
    QCheck.float (fun f ->
      let i = Parsley.exact_int_of_float f in
      (ex i <=> (floor f = f)) &&
        (nex i <=> (floor f <> f || out_of_range f range_64_f)))

let int_32_of_64 =
  QCheck.Test.make ~count:1000 ~name:"int64 to int32 conversions"
    QCheck.int64 (fun i -> nex (Parsley.exact_32_of_64 i) <=>
                             (out_of_range i range_32))

let int_64_of_32 =
  QCheck.Test.make ~count:1000 ~name:"int32 to int64 conversions"
    QCheck.int32 (fun i -> ex (Parsley.exact_64_of_32 i))

let int_native_of_64 =
  QCheck.Test.make ~count:1000 ~name:"int64 to nativeint conversions"
    QCheck.int64 (fun i -> nex (Parsley.exact_native_of_64 i) =>
                             (Nativeint.size=32 && (out_of_range i range_32)))

let int_64_of_native =
  QCheck.Test.make ~count:1000 ~name:"nativeint to int64 conversions"
    QCheck.int64 (fun i -> ex (Parsley.exact_native_of_64 i))

let () =
  Format.printf "Starting tests using QCheck.\n";
  QCheck.Test.check_exn float_of_int;
  QCheck.Test.check_exn int_of_float;
  QCheck.Test.check_exn int_32_of_64;
  QCheck.Test.check_exn int_64_of_32;
  QCheck.Test.check_exn int_64_of_native;
  QCheck.Test.check_exn int_native_of_64;
  Format.printf "All tests sucessfully ran.\n"
