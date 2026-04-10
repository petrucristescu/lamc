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
    eval \$(opam env) && dune build src/lmc.exe && mkdir -p test_results &&
    test_file=\"src/test/${TEST_NAME}.lmc\" &&
    expected=\"src/test/${TEST_NAME}.lmc.out\" &&
    if [ ! -f \"\$test_file\" ]; then echo \"Test not found: \$test_file\"; exit 1; fi &&
    _build/default/src/lmc.exe \"\$test_file\" > \"test_results/${TEST_NAME}.out\" 2>&1 &&
    echo '--- Output ---' &&
    cat \"test_results/${TEST_NAME}.out\" &&
    echo '--- Result ---' &&
    if diff -u \"test_results/${TEST_NAME}.out\" \"\$expected\"; then
      echo \"PASS: ${TEST_NAME}\"
    else
      echo \"FAIL: ${TEST_NAME}\"; exit 1
    fi
  "
fi