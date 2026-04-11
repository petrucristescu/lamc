# String Library
# Native primitives: length, concat, substring, uppercase, lowercase,
#   trim, charAt, indexOf, startsWith, endsWith, replace, toString

# Check if string is empty
~isEmpty s (eq (length s) 0)

# Check if string contains a substring
~contains s,sub (not (eq (indexOf s sub) (0 - 1)))

# Repeat a string n times
~repeat s,n (
    eq n 0
        (|>_. "")
        (|>_. concat s (repeat s (n - 1)))
    0
)

# Left pad string to length
~padLeft target,pad,s (
    eq (length s) target
        (|>_. s)
        (|>_. eq (length s) target
            (|>_. s)
            (|>_. padLeft target pad (concat pad s))
        0)
    0
)
