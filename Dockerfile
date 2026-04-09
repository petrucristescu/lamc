FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Install dependencies (matching GitHub Actions setup)
RUN apt-get update && \
    apt-get install -y bubblewrap wget gcc make patch unzip m4 git ca-certificates bzip2 && \
    rm -rf /var/lib/apt/lists/*

# Install opam 2.1.5 (same version as CI)
RUN wget -q https://github.com/ocaml/opam/releases/download/2.1.5/opam-2.1.5-x86_64-linux -O /usr/local/bin/opam && \
    chmod +x /usr/local/bin/opam

# Initialize opam with OCaml 5.1.1 (same as CI)
RUN opam init --disable-sandboxing --no-setup --compiler=ocaml-base-compiler.5.1.1 -y && \
    opam install dune -y

WORKDIR /app

# Copy source
COPY . .

# Build and test in one step
CMD ["bash", "-c", "eval $(opam env) && dune build src/lmc.exe && mkdir -p test_results && failed=0 && for test_file in src/test/*.lmc; do test_name=$(basename \"$test_file\" .lmc); expected_output=\"src/test/${test_name}.lmc.out\"; _build/default/src/lmc.exe \"$test_file\" > \"test_results/${test_name}.out\" 2>&1; if ! diff -u \"test_results/${test_name}.out\" \"$expected_output\"; then echo \"FAIL: $test_name\"; failed=1; else echo \"PASS: $test_name\"; fi; done && if [ $failed -eq 1 ]; then echo 'Some tests failed!'; exit 1; else echo 'All tests passed!'; fi"]