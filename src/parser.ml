exception ParseError of string

let parse input =
  if String.trim input = "" then
    raise (ParseError "Input cannot be empty")
  else
    input  (* dummy parsed expression *)
