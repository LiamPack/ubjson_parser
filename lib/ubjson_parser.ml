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

let take1 = take 1 >>= fun x -> return (String.get x 0)
let peek1 = peek_char_fail >>= fun x -> return x

(* Number parsers *)
module N = struct
  let parse_number i f =
    take i >>= fun x -> if (String.equal x "]" || String.equal x "}") then fail "parse_number" else return (f x)

  let _uint8 =
    parse_number  1 (fun x -> `UInt8 (Bytes.get_uint8 (Bytes.of_string x) 0))

  let _int8 : ubjson t =
    parse_number  1 (fun x ->  `Int8 (Bytes.get_int8 (Bytes.of_string x) 0))

  let _int16 : ubjson t =
    parse_number  2 (fun x ->  `Int16 (Bytes.get_int16_be (Bytes.of_string x) 0))

  let _int32 : ubjson t =
    parse_number  4 (fun x -> `Int32 (Bytes.get_int32_be (Bytes.of_string x) 0))

  let _int64 : ubjson t =
    parse_number  8 (fun x -> `Int64 (Bytes.get_int64_be (Bytes.of_string x) 0))

  let _float32 : ubjson t  =
    parse_number  4 (fun x -> `Float32 (Bytes.get_int32_be (Bytes.of_string x) 0 |> Int32.float_of_bits))

  let _float64 : ubjson t  =
    parse_number  8 (fun x -> `Float32 (Bytes.get_int64_be (Bytes.of_string x) 0 |> Int64.float_of_bits))

  let _to_value x =
    match x with
    | `UInt8 y -> y
    | `Int8 y -> y
    | `Int16 y -> y
    | `Int32 y -> (Int32.to_int y)
    | `Int64 y -> (Int64.to_int y)
    | _ -> assert false

  let _number c =
    char c *>
      (match c with
       | 'U' -> _uint8
       | 'i' -> _int8
       | 'I' -> _int16
       | 'l' -> _int32
       | 'L' -> _int64
       | 'd' -> _float32
       | 'D' -> _float64
       | x -> fail ("_number Failed on char " ^ (String.make 1 x)))

end

let _false : ubjson t = return `False
let _true  : ubjson t = return `True
let _null  : ubjson t = return `Null
let _no_op  : ubjson t = return `NoOp

let _char : ubjson t =
  take1 >>= fun x -> return (`Char x)

let _id_string : string t =
  peek_char_fail >>= fun x -> N._number x
  >>= function | `UInt8 y -> take y
               | `Int8 y -> take y
               | `Int16 y -> take y
               | `Int32 y -> take (Int32.to_int y)
               | `Int64 y -> take (Int64.to_int y)
               | _ -> fail "Identifier String"

let _string : ubjson t =
  _id_string >>= fun s -> return (`String s)

let _parse_char : char -> ubjson t =
  function c ->
            match c with
            | 'U' -> N._uint8
            | 'i' -> N._int8
            | 'I' -> N._int16
            | 'l' -> N._int32
            | 'L' -> N._int64
            | 'd' -> N._float32
            | 'D' -> N._float64
            | 'F' ->  _false
            | 'T' ->  _true
            | 'Z' ->  _null
            | 'N' ->  _no_op
            | 'C' ->  _char
            | 'S' ->  _string
            | c -> fail ("End of Grammar: "^(String.make 1 c))


(* TODO: not tail-recursive, also ugly *)
let ubjson =
  let pair x y = (x,y) in
  fix (fun ubjson ->
      let mem = both _id_string ubjson in
      let mem_fixed_type t = lift2 pair _id_string t in
      let maybe_type =
        char '$' *> take1 in
      let maybe_length =
        char '#' *> peek1 >>= N._number in
      (* TODO: replace the "or" (<|>) parser branches with a `both (option maybe_type)
         (option maybe_length) >>= fun x -> match x ... *)
      let obj =
        ((lift2 pair maybe_type maybe_length
          >>= fun x -> let c,l = x in
                       count (N._to_value l) (mem_fixed_type (_parse_char c))) <|>
           (maybe_length
            >>= fun l ->
            count (N._to_value l) mem) <|>
           (maybe_type >>= fun c -> many (mem_fixed_type (_parse_char c)) <* char '}') <|>
           (many mem <* char '}'))
        >>= fun ms -> return (`Object ms) in
      let arr =
        ((lift2 pair maybe_type maybe_length
          >>= fun x -> let c,l = x in
                       count (N._to_value l) (_parse_char c)) <|>
           (maybe_length
            >>= fun l ->
            count (N._to_value l) ubjson) <|>
           (maybe_type >>= fun c -> many (_parse_char c) <* char ']') <|>
           (many ubjson <* char ']'))
        >>= fun ms -> return (`Array ms) in
      peek_char_fail
      >>= function c ->   (advance 1 *> (_parse_char c)) <|>
                            (char '{' *> obj     )<|>
                            (char '[' *> arr     )<|>
                            fail ("End of Grammar: "^(String.make 1 c))) <?> "ubjson"

let _ =
  match (parse_string ~consume:All ubjson "{i\005loginSi\007octocati\002idSi\006Mediumi\004plan{i\004nameSi\006Medium}U\003raw[U\003d\001\002\003\004TFZNC3]}") with
  | Ok c -> c
  | Error msg -> print_endline msg; `Null
