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

let float_int =
  QCheck.Test.make ~count:1000 ~name:"float_of_int conversions"
    QCheck.int (fun i -> nex (Parsley.exact_float_of_int i) =>
                           (i > 9007199254740992 || i < -9007199254740993))

let int_32_of_64 =
  QCheck.Test.make ~count:1000 ~name:"int64 to int32 conversions"
    QCheck.int64 (fun i -> nex (Parsley.exact_32_of_64 i) <=>
                             (i > two_32 || i < mtwo_32))

let int_64_of_32 =
  QCheck.Test.make ~count:1000 ~name:"int32 to int64 conversions"
    QCheck.int32 (fun i -> ex (Parsley.exact_64_of_32 i))

let int_native_of_64 =
  QCheck.Test.make ~count:1000 ~name:"int64 to nativeint conversions"
    QCheck.int64 (fun i -> nex (Parsley.exact_native_of_64 i) =>
                             (Nativeint.size=32 && (i > two_32 || i < mtwo_32)))

let int_64_of_native =
  QCheck.Test.make ~count:1000 ~name:"nativeint to int64 conversions"
    QCheck.int64 (fun i -> ex (Parsley.exact_native_of_64 i))

let () =
  QCheck.Test.check_exn float_int;
  QCheck.Test.check_exn int_32_of_64;
  QCheck.Test.check_exn int_64_of_32;
  QCheck.Test.check_exn int_64_of_native;
  QCheck.Test.check_exn int_native_of_64;
