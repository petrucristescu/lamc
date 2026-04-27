# Test tail call optimization
# Tail-recursive functions should not overflow the stack with large inputs

~sum_tail n,acc (
    (eq n 0 (|>_. acc) (|>_. sum_tail (n - 1) (acc + n))) 0
)

~fact_tail n,acc (
    (eq n 0 (|>_. acc) (|>_. fact_tail (n - 1) (acc * n))) 0
)

~countdown n (
    (eq n 0 (|>_. 0) (|>_. countdown (n - 1))) 0
)

assert (eq (sum_tail 10 0) 55)
assert (eq (fact_tail 5 1) 120)
assert (eq (fact_tail 10 1) 3628800)

# Deep recursion - would stack overflow without TCO
assert (eq (countdown 100000) 0)
assert (eq (sum_tail 100000 0) 5000050000L)
