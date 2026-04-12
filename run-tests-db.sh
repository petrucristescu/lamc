#!/bin/bash
# Run database integration tests
# Spins up MySQL via Docker Compose, runs Churing DB tests, then tears down.
#
# Usage:
#   ./run-tests-db.sh              # run all DB tests
#   ./run-tests-db.sh 33_mysql     # run a single DB test

set -e

TEST_NAME="$1"

echo "=== Starting MySQL ==="
docker-compose up -d mysql

echo "=== Waiting for MySQL to be ready ==="
for i in $(seq 1 30); do
    if docker-compose exec -T mysql mysqladmin ping -h 127.0.0.1 -pchuringtest --silent 2>/dev/null; then
        echo "MySQL is ready!"
        break
    fi
    if [ "$i" -eq 30 ]; then
        echo "MySQL failed to start in time"
        docker-compose down
        exit 1
    fi
    sleep 1
done

echo "=== Rebuilding Churing Docker image ==="
docker build -t churing-test . -q

echo "=== Running DB tests ==="
failed=0

run_one_test() {
    local test_name="$1"
    local test_file="src/test/${test_name}.ch"
    if [ ! -f "$test_file" ]; then
        echo "Test not found: $test_file"
        return 1
    fi

    # Run test container on the same network as MySQL
    MSYS_NO_PATHCONV=1 docker run --rm \
        --user root \
        --network churing_default \
        -e OPAMROOTISOK=1 \
        -e MYSQL_HOST=mysql \
        -e MYSQL_PORT=3306 \
        -e MYSQL_USER=root \
        -e MYSQL_PASSWORD=churingtest \
        -e MYSQL_DATABASE=churingdb \
        -v "$(pwd):/app" \
        churing-test bash -c "
            eval \$(opam env) && dune build src/churing.exe &&
            _build/default/src/churing.exe '$test_file'
        " 2>&1
    return $?
}

if [ -n "$TEST_NAME" ]; then
    echo "--- Running: $TEST_NAME ---"
    if run_one_test "$TEST_NAME"; then
        echo "PASS: $TEST_NAME"
    else
        echo "FAIL: $TEST_NAME"
        failed=1
    fi
else
    for test_file in src/test/*_mysql*.ch; do
        test_name=$(basename "$test_file" .ch)
        echo "--- Running: $test_name ---"
        if run_one_test "$test_name"; then
            echo "PASS: $test_name"
        else
            echo "FAIL: $test_name"
            failed=1
        fi
    done
fi

echo "=== Tearing down MySQL ==="
docker-compose down

if [ "$failed" -eq 1 ]; then
    echo "Some DB tests failed!"
    exit 1
else
    echo "All DB tests passed!"
fi
