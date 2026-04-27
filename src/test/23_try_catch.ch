# Test try/catch error handling (Option A — primitive)

# Catch division by zero
@r1 (try (10 / 0) (|>e. "caught"))
assert (eq r1 "caught")

# Catch runtime error
@r2 (try (head []) (|>e. "empty"))
assert (eq r2 "empty")

# No error — returns value normally
@r3 (try (10 + 5) (|>e. 0))
assert (eq r3 15)

# Error message is passed to handler
@r4 (try (head []) (|>msg. msg))
assert (startsWith r4 "head")

# Catch assertion failure
@r5 (try (assert false) (|>e. "assert caught"))
assert (eq r5 "assert caught")

# Nested try
@r6 (try
    (try (1 / 0) (|>_. head []))
    (|>e. "inner also failed"))
assert (eq r6 "inner also failed")

true
