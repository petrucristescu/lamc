let infer_str s =
  Infer.reset_counter ();
  let exprs = Parser.parse s in
  let env = Infer.add_operators_to_env Types.StringMap.empty in
  match exprs with
  | [e] -> let _, t = Infer.infer env e in t
  | _ -> failwith "expected single expression"

let typ_pp fmt t = Format.pp_print_string fmt (Ast.string_of_typ t)
let typ_testable = Alcotest.testable typ_pp ( = )

let check msg expected s = Alcotest.(check typ_testable) msg expected (infer_str s)

let test_int () = check "int" Types.TInt "42"
let test_long () = check "long" Types.TLong "42L"
let test_float () = check "float" Types.TFloat "3.14"
let test_string () = check "string" Types.TString "\"hello\""
let test_bool () = check "bool" Types.TBool "true"
let test_add () = check "add" Types.TInt "2 + 3"
let test_div () = check "div" Types.TFloat "6 / 2"
let test_eq () = check "eq" Types.TBool "eq 1 2"
let test_assert () = check "assert" Types.TBool "assert true"

let test_type_error () =
  try ignore (infer_str "2 + \"hello\""); Alcotest.fail "expected TypeError"
  with Infer.TypeError _ -> ()

let () =
  Alcotest.run "Infer" [
    "literals", [
      Alcotest.test_case "int" `Quick test_int;
      Alcotest.test_case "long" `Quick test_long;
      Alcotest.test_case "float" `Quick test_float;
      Alcotest.test_case "string" `Quick test_string;
      Alcotest.test_case "bool" `Quick test_bool;
    ];
    "expressions", [
      Alcotest.test_case "add" `Quick test_add;
      Alcotest.test_case "div" `Quick test_div;
      Alcotest.test_case "eq" `Quick test_eq;
      Alcotest.test_case "assert" `Quick test_assert;
    ];
    "errors", [
      Alcotest.test_case "type error" `Quick test_type_error;
    ];
  ]
