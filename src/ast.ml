let print_ast exprs =
  print_endline (Parser.string_of_exprs exprs)