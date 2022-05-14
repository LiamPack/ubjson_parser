open Angstrom


(* https://ubjson.org/ *)

(* https://ubjson.org/type-reference/ *)
type ubjson =
  (* value types *)
  [ `Null
  | `NoOp
  | `True
  | `False
  | `Int8 of int
  | `UInt8 of int
  | `Int16 of int
  | `Int32 of int32
  | `Int64 of int64
  | `Float32 of float
  | `Float64 of float
  | `HighPrecisionNumber of float
  | `Char of char
  | `String of string
  (* https://ubjson.org/type-reference/container-types/ *)
  | `Object of (string * ubjson) list
  | `Array  of ubjson list ];;


(* Number parsers *)
module N = struct
  let _uint8 : ubjson t =
    let open Bytes in
    char 'U' *> take 1 >>= fun x -> return (`UInt8 (get_uint8 (of_string x) 0))

  let _int8 : ubjson t =
    let open Bytes in
    char 'i' *> take 1 >>= fun x -> return (`Int8 (get_int8 (of_string x) 0))

  let _int16 : ubjson t =
    let open Bytes in
    char 'I' *> take 2 >>= fun x -> return (`Int16 (get_int16_be (of_string x) 0))

  let _int32 : ubjson t =
    let open Bytes in
    char 'l' *> take 4 >>= fun x -> return (`Int32 (get_int32_be (of_string x) 0))

  let _int64 : ubjson t =
    let open Bytes in
    char 'L' *> take 8 >>= fun x -> return (`Int64 (get_int64_be (of_string x) 0))

  let _float32 : ubjson t  =
    let open Bytes in
    char 'd' *> take 4 >>= fun x -> return (`Float32 (get_int32_be (of_string x) 0 |> Int32.float_of_bits))

  let _float64 : ubjson t =
    let open Bytes in
    char 'D' *> take 8 >>= fun x -> return (`Float64 (get_int64_be (of_string x) 0 |> Int64.float_of_bits))

  let _number : char -> ubjson t = function
    | 'U' -> _uint8
    | 'i' -> _int8
    | 'I' -> _int16
    | 'l' -> _int32
    | 'L' -> _int64
    | 'd' -> _float32
    | 'D' -> _float64
    | x -> fail ("_number Failed on char " ^ (String.make 1 x))

end

let _false : ubjson t = char 'F' *> return `False
let _true  : ubjson t = char 'T'  *> return `True
let _null  : ubjson t = char 'Z'  *> return `Null
let _no_op  : ubjson t = char 'N'  *> return `NoOp

let _char : ubjson t =
  let open Bytes in
  char 'C' *> take 1 >>= fun x -> return (`Char (String.get (to_string (of_string x)) 0))

let _id_string : string t =
  peek_char_fail >>= fun x -> N._number x
  >>= function | `UInt8 y -> take y
               | `Int8 y -> take y
               | `Int16 y -> take y
               | `Int32 y -> take (Int32.to_int y)
               | `Int64 y -> take (Int64.to_int y)
               | _ -> fail "HELP"

let _string : ubjson t =
  let open Bytes in
  char 'S' *> _id_string >>= fun s -> return (`String s)


let ubjson =
  let pair x y = (x,y) in
  fix (fun ubjson ->
      let mem = lift2 pair _id_string ubjson in
      let peek_optimized_container =
        (function | '$' -> take 2
                  | '#' -> char '#' *> peek_char_fail >>=
                             fun x -> N._number x *> take 0
                  | _ -> take 0) in
      let peek_dollar_sign =
        (peek_char_fail
         >>=  peek_optimized_container
         >>= fun _ -> peek_char_fail
         >>= peek_optimized_container) in
      let obj = char '{'
                *> peek_dollar_sign
                *> many mem <* char '}' >>= fun ms -> (List.iter (fun x -> match x with |s,_ -> print_endline s) ms); return (`Object ms) in
      let arr = char '['
                *> peek_dollar_sign
                *> many ubjson <* char ']' >>= fun ms -> return (`Array ms) in
      peek_char_fail
      >>= function
      | 'F' -> _false | 'T' -> _true | 'Z' -> _null | 'N' -> _no_op
      | 'U' -> N._uint8
      | 'i' -> N._int8
      | 'I' -> N._int16
      | 'l' -> N._int32
      | 'L' -> N._int64
      | 'd' -> N._float32
      | 'D' -> N._float64
      | 'C' -> _char
      | 'S' -> _string
      | '{' -> obj
      | '[' -> arr
      | c -> fail ("End of Grammar: "^(String.make 1 c))
    ) <?> "ubjson"


let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s


let test_str = "{i\005loginSi\007octocati\002idSi\006Mediumi\004plan{i\004nameSi\006Medium}U\003raw[$U#l\000\002\005\003U\003]}"


let x =
  let fn = (read_whole_file "../test/raw.slp") in
  match (parse_string ~consume:All ubjson fn) with
  | Ok c -> c
  | Error msg -> print_endline msg; `Null
