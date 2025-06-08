let () =
  let show_ast = ref false in
  let show_types = ref false in
  let filename = ref "" in
  let args = Array.to_list Sys.argv |> List.tl in
  List.iter (function
    | "--ast" -> show_ast := true
    | "--types" -> show_types := true
    | s when !filename = "" && not (String.starts_with ~prefix:"--" s) -> filename := s
    | _ -> ()
  ) args;
  if !filename = "" then (
    print_endline "Usage: lmc [--ast] [--types] <file.lmc>";
    exit 1
  );
  let input =
    let ic = open_in !filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s
  in
  try
    let exprs = Parser.parse_and_infer ~show_types:!show_types input in
    if !show_ast then Ast.print_ast exprs;
    Eval.eval_program exprs
  with
    | Parser.ParseError (msg, line, col) ->
      Printf.printf "Parse error at line %d, col %d: %s\n" line col msg