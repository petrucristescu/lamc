module StringMap = Eval.StringMap

let eval_str s =
  let exprs = Parser.parse s in
  match exprs with
  | [e] -> Eval.eval StringMap.empty e
  | _ -> failwith "expected single expression"

let check_int msg expected s =
  match eval_str s with
  | Eval.VInt n -> Alcotest.(check int) msg expected n
  | _ -> Alcotest.fail ("expected VInt for: " ^ s)

let check_long msg expected s =
  match eval_str s with
  | Eval.VLong n -> Alcotest.(check int64) msg expected n
  | _ -> Alcotest.fail ("expected VLong for: " ^ s)

let check_float msg expected s =
  match eval_str s with
  | Eval.VFloat f -> Alcotest.(check (float 0.001)) msg expected f
  | _ -> Alcotest.fail ("expected VFloat for: " ^ s)

let check_bool msg expected s =
  match eval_str s with
  | Eval.VBool b -> Alcotest.(check bool) msg expected b
  | _ -> Alcotest.fail ("expected VBool for: " ^ s)

let test_int () = check_int "int" 42 "42"
let test_add () = check_int "add" 5 "2 + 3"
let test_sub () = check_int "sub" 2 "5 - 3"
let test_mul () = check_int "mul" 6 "2 * 3"
let test_div () = check_float "div" 2.5 "5 / 2"
let test_add_long () = check_long "long" 11L "4L + 7L"
let test_add_mixed () = check_long "mixed" 11L "4 + 7L"
let test_prec () = check_int "prec" 10 "2 * 3 + 4"
let test_left_assoc () = check_int "assoc" 1 "5 - 3 - 1"
let test_eq_true () = check_bool "eq" true "eq 1 1"
let test_eq_false () = check_bool "eq" false "eq 1 2"
let test_lambda () = check_int "lam" 42 "(|>x. x) 42"

let test_div_zero () =
  try ignore (eval_str "5 / 0"); Alcotest.fail "expected error"
  with Eval.RuntimeError _ -> ()

let test_assert_pass () =
  Eval.eval_program (Parser.parse "~(\n  assert (eq 1 1)\n)")

let test_assert_fail () =
  try Eval.eval_program (Parser.parse "~(\n  assert (eq 1 2)\n)");
    Alcotest.fail "expected AssertionFailure"
  with Eval.AssertionFailure _ -> ()

let () =
  Alcotest.run "Eval" [
    "arithmetic", [
      Alcotest.test_case "int" `Quick test_int;
      Alcotest.test_case "add" `Quick test_add;
      Alcotest.test_case "sub" `Quick test_sub;
      Alcotest.test_case "mul" `Quick test_mul;
      Alcotest.test_case "div" `Quick test_div;
      Alcotest.test_case "long" `Quick test_add_long;
      Alcotest.test_case "mixed" `Quick test_add_mixed;
      Alcotest.test_case "precedence" `Quick test_prec;
      Alcotest.test_case "left assoc" `Quick test_left_assoc;
    ];
    "equality", [
      Alcotest.test_case "eq true" `Quick test_eq_true;
      Alcotest.test_case "eq false" `Quick test_eq_false;
    ];
    "lambda", [
      Alcotest.test_case "identity" `Quick test_lambda;
    ];
    "errors", [
      Alcotest.test_case "div by zero" `Quick test_div_zero;
    ];
    "assert", [
      Alcotest.test_case "pass" `Quick test_assert_pass;
      Alcotest.test_case "fail" `Quick test_assert_fail;
    ];
  ]
