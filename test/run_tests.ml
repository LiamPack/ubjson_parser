open Ubjson_parser
open Angstrom

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
