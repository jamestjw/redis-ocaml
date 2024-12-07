open Core

type component =
  | Char of char
  | Star
  | Optional
  | Not of char list
  | Range of char * char
  | Set of char list

let glob_to_re inp =
  let open Result.Let_syntax in
  let parse_sq_bracket inp =
    let rec take_until_close_bracket acc inp =
      match inp with
      | [] -> Error "unmatched '['"
      | ']' :: rest -> Ok (List.rev acc, rest)
      | (('*' | '?' | '[' | '^' | '-') as c) :: _ ->
        Error (Printf.sprintf "'%c' is not allowed in pattern with '^'" c)
      | '\'' :: c :: rest | c :: rest -> take_until_close_bracket (c :: acc) rest
    in
    match inp with
    | '[' :: '^' :: rest ->
      let%bind to_exclude, rest = take_until_close_bracket [] rest in
      Ok (Not to_exclude, rest)
    | '[' :: c1 :: '-' :: c2 :: rest -> Ok (Range (c1, c2), rest)
    | '[' :: rest ->
      let%bind set, rest = take_until_close_bracket [] rest in
      Ok (Set set, rest)
    | _ -> Error "invalid square bracket pattern"
  in
  let rec parse acc inp =
    match inp with
    | [] -> Ok (List.rev acc)
    | '*' :: rest -> parse (Star :: acc) rest
    | '?' :: rest -> parse (Optional :: acc) rest
    | '[' :: _ ->
      let%bind comp, rest = parse_sq_bracket inp in
      parse (comp :: acc) rest
    | ']' :: _ -> Error "unmatched ']' in glob pattern"
    | c :: rest -> parse (Char c :: acc) rest
  in
  let component_to_re_str = function
    | Char c ->
      (* TODO: I need to escape it myself if it is a special regex character*)
      String.of_char c
    | Star -> ".*"
    | Optional -> ".?"
    | Not cs -> Printf.sprintf "[^%s]" (String.of_list cs)
    | Range (left, right) -> Printf.sprintf "[%c-%c]" left right
    | Set cs -> Printf.sprintf "[%s]" (String.of_list cs)
  in
  let%bind components = parse [] (String.to_list inp) in
  let res =
    Printf.sprintf "^%s$" (List.map ~f:component_to_re_str components |> String.concat)
    |> Str.regexp
  in
  Ok res
;;

let glob_to_matcher inp =
  let open Result.Let_syntax in
  let%bind regexp = glob_to_re inp in
  return (fun s -> Str.string_match regexp s 0)
;;
