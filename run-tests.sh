#!/bin/bash
# Run all tests locally using Docker (no OCaml needed on host)
# Usage: ./run-tests.sh

# MSYS_NO_PATHCONV prevents Git Bash on Windows from mangling /app paths
MSYS_NO_PATHCONV=1 docker run --rm -v "$(pwd):/app" lamc-test