open Ubjson_parser
open Angstrom
open OUnit2

let test_easy = "[U\003d\001\002\003\004TFZNC3]"

let test_arr_fixed_type = "[$U\000\001\002\003]"

let test_arr_fixed_length = "[#i\006U\000U\001U\002U\003U\004U\005"

let test_arr_fixed_type_and_length = "[$U#i\006\000\001\002\003\004\005"

let test_obj_fixed_type = "{$Ui\003lat\003i\003lon\005i\003alt\009}"

let test_obj_fixed_length = "{#i\003i\003latU\003i\003lonU\005i\003altU\009"

let test_obj_fixed_type_and_length = "{$U#i\003i\003lat\003i\003lon\005i\003alt\009"

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


let check_ok s =
  match parse_string ~consume:All ubjson s with
  | Ok _    -> true
  | Error _ -> false


let assert_true v = assert_equal true v

let test_suite =
  "test suite ubjson_parser"
  >::: [ ("Complete Example"
         >:: fun _ ->
         assert_true
           (check_ok
              "{i\005loginSi\007octocati\002idSi\006Mediumi\004plan{i\004nameSi\006Medium}U\003raw[U\003d\001\002\003\004TFZNC3]}")
         )
       ; ("Large Raw Array, length+type specified"
         >:: fun _ ->
         let fn = read_whole_file "../../../test/test.slp" in
         check_ok fn |> assert_true)
       ; ("Easy Array, all types" >:: fun _ -> check_ok test_easy |> assert_true)
       ; ("Fixed size object" >:: fun _ -> check_ok test_obj_fixed_length |> assert_true)
       ; ("Fixed type object" >:: fun _ -> check_ok test_obj_fixed_type |> assert_true)
       ; ("Fixed type and size object"
         >:: fun _ -> check_ok test_obj_fixed_type_and_length |> assert_true)
       ; ("Fixed size array" >:: fun _ -> check_ok test_arr_fixed_length |> assert_true)
       ; ("Fixed type array" >:: fun _ -> check_ok test_arr_fixed_type |> assert_true)
       ; ("Fixed type and size array"
         >:: fun _ -> check_ok test_arr_fixed_type_and_length |> assert_true)
       ]


let () =
  print_endline "---------- Unit Tests ----------";
  OUnit2.run_test_tt_main test_suite
