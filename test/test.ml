(* testing utilities *)
let ex = function
  | Ok _ -> true
  | Error _ -> false

let nex x = not (ex x)

(* wrapper that check that no trivially true implication is made *)
let check_implies ~count ~name gen left right =
  let right_hand = ref false in
  QCheck.Test.make ~count ~name
    gen (fun i -> if left i then (right_hand:= true; right i) else true)
  |> QCheck.Test.check_exn;
  if not !right_hand then
    Format.printf "Warning: the implication in \027[36m%s\027[0m is \
                   true but maybe its left-hand side is always false\n" name

(* useful constants using Int module *)
let two_32L = Int32.max_int |> Int64.of_int32
let mtwo_32L = Int32.min_int |> Int64.of_int32
let two_64f = Int64.max_int |> Int64.to_float
let mtwo_64f = Int64.min_int |> Int64.to_float
let two_53 = 9007199254740992
let mtwo_53 = -9007199254740992
let two_53L = 9007199254740992 |> Int64.of_int
let mtwo_53L = -9007199254740992 |> Int64.of_int
let two_53f = 9007199254740992.
let mtwo_53f = -9007199254740992.

let range_32L = mtwo_32L,two_32L
let range_53 = mtwo_53, two_53
let range_53L = mtwo_53L, two_53L
let range_53f = mtwo_53f, two_53f
let range_64_f = mtwo_64f, two_64f

let in_range x (a,b) = a<=x && x<=b
let out_of_range x r = not (in_range x r)

let default () =
  check_implies ~count:1000 ~name:"float_of_int conversions"
    QCheck.int
    (fun i -> nex (Parsley.exact_float_of_int i))
    (fun i -> out_of_range i range_53);
  QCheck.Test.make ~count:1000 ~name:"int_of_float conversions"
    QCheck.float (fun f ->
      let i = Parsley.exact_int_of_float f in
      if ex i then (floor f = f)
      else (floor f <> f || out_of_range f range_64_f))
  |> QCheck.Test.check_exn;
  QCheck.Test.make ~count:1000 ~name:"int64 to int32 conversions"
    QCheck.int64 (fun i ->
      nex (Parsley.exact_32_of_64 i) = (out_of_range i range_32L))
  |> QCheck.Test.check_exn;
  check_implies ~count:1000 ~name:"int64 to float conversions"
    QCheck.int64
    (fun i -> nex (Parsley.exact_float_of_64 i))
    (fun i-> (out_of_range i range_53L))

(* these tests can only fail on 32bit machines *)
let only_32_bits () =
  check_implies ~count:1000 ~name:"int64 to nativeint conversions"
    QCheck.int64 (fun i -> nex (Parsley.exact_native_of_64 i))
    (fun i -> (out_of_range i range_32L))

(* these tests should trivially pass *)
let tautologies () =
  QCheck.Test.make ~count:1000 ~name:"int32 to int64 conversions"
    QCheck.int32 (fun i -> ex (Parsley.exact_64_of_32 i))
  |> QCheck.Test.check_exn;
  QCheck.Test.make ~count:1000 ~name:"nativeint to int64 conversions"
    QCheck.int64 (fun i -> ex (Parsley.exact_native_of_64 i))
  |> QCheck.Test.check_exn

let () =
  Format.printf "Starting the test using QCheck.\n";
  default();
  if Nativeint.size = 32 then only_32_bits()
  else Format.printf "64-bit architecture detected, skipping the 32-bits tests\n";
  tautologies();
  Format.printf "All tests sucessfully ran.\n"
