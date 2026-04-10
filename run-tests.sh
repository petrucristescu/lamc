#!/bin/bash
# Run tests locally using Docker (no OCaml needed on host)
# Usage:
#   ./run-tests.sh              # run all tests
#   ./run-tests.sh 12_lambda    # run a single test by name

TEST_NAME="$1"

# MSYS_NO_PATHCONV prevents Git Bash on Windows from mangling /app paths
if [ -z "$TEST_NAME" ]; then
  MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd):/app" lamc-test
else
  MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd):/app" lamc-test bash -c "
    test_name=\"\$1\" &&
    eval \$(opam env) && dune build src/lmc.exe && mkdir -p test_results &&
    test_file=\"src/test/\${test_name}.lmc\" &&
    expected=\"src/test/\${test_name}.lmc.out\" &&
    output_file=\"test_results/\${test_name}.out\" &&
    if [ ! -f \"\$test_file\" ]; then echo \"Test not found: \$test_file\"; exit 1; fi &&
    _build/default/src/lmc.exe \"\$test_file\" > \"\$output_file\" 2>&1 &&
    echo '--- Output ---' &&
    cat \"\$output_file\" &&
    echo '--- Result ---' &&
    if diff -u \"\$output_file\" \"\$expected\"; then
      echo \"PASS: \$test_name\"
    else
      echo \"FAIL: \$test_name\"; exit 1
    fi
  " _ "$TEST_NAME"
fi