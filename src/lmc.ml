let () =
  let show_ast = ref false in
  let show_result = ref false in
  let filename = ref "" in
  let args = Array.to_list Sys.argv |> List.tl in
  List.iter (function
    | "--ast" -> show_ast := true
    | "--result" -> show_result := true
    | s when !filename = "" && not (String.starts_with ~prefix:"--" s) -> filename := s
    | _ -> ()
  ) args;
  if !filename = "" then (
    print_endline "Usage: lmc [--ast] [--result] <file.lmc>";
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
    let exprs = Parser.parse input in
    if !show_ast then Ast.print_ast exprs;
    if !show_result then (
      let rec eval_all env = function
        | [] -> ()
        | Parser.Let (name, _, value) :: rest ->
            let v = Eval.eval env value in
            Printf.printf "Let %s = %s\n"
              name
              (match v with
               | Eval.IntVal n -> string_of_int n
               | Eval.StrVal s -> s
               | Eval.FunVal _ -> "<function>");
            eval_all ((name, v) :: env) rest
        | Parser.FunDef (name, args, body) :: rest ->
            let v = Eval.FunVal (args, body, env) in
            Printf.printf "Fun %s = <function>\n" name;
            eval_all ((name, v) :: env) rest
        | expr :: rest ->
            let v = Eval.eval env expr in
            Printf.printf "Result: %s\n"
              (match v with
               | Eval.IntVal n -> string_of_int n
               | Eval.StrVal s -> s
               | Eval.FunVal _ -> "<function>");
            eval_all env rest
      in
      eval_all [] exprs
    )
    else if not !show_ast && not !show_result then
      Eval.eval_toplevel [] exprs
  with
    | Parser.ParseError (msg, line, col) ->
      Printf.printf "Parse error at line %d, col %d: %s\n" line col msg