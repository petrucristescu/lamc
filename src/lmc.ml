let () =
  let filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else (print_endline "Usage: lmc <file.lmc>"; exit 1)
  in
  let input =
    let ic = open_in filename in
    let len = in_channel_length ic in
    let s = really_input_string ic len in
    close_in ic;
    s
  in
  try
    let expr = Parser.parse input in
    print_endline ("Parsed expression: " ^ Parser.string_of_exprs expr)
  with
    | Parser.ParseError (msg, line, col) ->
      Printf.printf "Parse error at line %d, col %d: %s\n" line col msg