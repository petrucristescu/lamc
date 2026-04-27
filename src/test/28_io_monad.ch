# Test Church-encoded IO monad (Option B)

@test_path "/tmp/churing_test_28.txt"

# pureIO wraps a value
assert (eq (runIO (pureIO 42)) 42)

# writeFileIO + readFileIO via bindIO
@program (bindIO (writeFileIO test_path "monadic hello")
    (|>_. readFileIO test_path))
assert (eq (runIO program) "monadic hello")

# mapIO transforms result
@upper_program (mapIO uppercase (readFileIO test_path))
assert (eq (runIO upper_program) "MONADIC HELLO")

# Chain multiple actions
@chain (bindIO (writeFileIO test_path "step1")
    (|>_. bindIO (appendFileIO test_path " step2")
        (|>_. readFileIO test_path)))
assert (eq (runIO chain) "step1 step2")

# Cleanup
assert (deleteFile test_path)

true
