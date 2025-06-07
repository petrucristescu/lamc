let () =
  print_endline "Enter expression:";
  let input = read_line () in
  try
    let expr = Parser.parse input in
    print_endline "Parsed successfully!";
    print_endline ("Parsed expression: " ^ expr)
  with
  | Parser.ParseError msg ->
    Printf.printf "Parse error: %s\n" msg
