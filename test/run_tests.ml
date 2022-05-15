open Ubjson_parser
open Angstrom

let test_easy = "[U\003d\001\002\003\004TFZNC3]"
let test_str = "{i\005loginSi\007octocati\002idSi\006Mediumi\004plan{i\004nameSi\006Medium}U\003raw[U\003d\001\002\003\004TFZNC3]}"

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


let _ =
  let fn = (read_whole_file "/home/liamp/Slippi/Game_20220108T175949.slp") in
  match (parse_string ~consume:All ubjson "fn") with
  | Ok c -> c
  | Error msg -> print_endline msg; `Null
