#!/bin/bash
# Run an LMC file locally using Docker (no OCaml needed on host)
# Usage: ./run.sh example.lmc

FILE="$1"

if [ -z "$FILE" ]; then
  echo "Usage: ./run.sh <file.lmc>"
  exit 1
fi

if [ ! -f "$FILE" ]; then
  echo "File not found: $FILE"
  exit 1
fi

MSYS_NO_PATHCONV=1 docker run --rm --user root -e OPAMROOTISOK=1 -v "$(pwd):/app" lamc-test bash -c "
  eval \$(opam env) && dune build src/lmc.exe &&
  _build/default/src/lmc.exe \"\$1\"
" _ "$FILE"
