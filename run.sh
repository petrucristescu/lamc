#!/bin/bash
# Run a Churing file locally using Docker (no OCaml needed on host)
# Usage: ./run.sh example.ch

FILE="$1"

if [ -z "$FILE" ]; then
  echo "Usage: ./run.sh <file.ch>"
  exit 1
fi

if [ ! -f "$FILE" ]; then
  echo "File not found: $FILE"
  exit 1
fi

MSYS_NO_PATHCONV=1 docker run --rm --user root -e OPAMROOTISOK=1 -v "$(pwd):/app" churing-test bash -c "
  eval \$(opam env) && dune build src/churing.exe &&
  _build/default/src/churing.exe \"\$1\"
" _ "$FILE"
