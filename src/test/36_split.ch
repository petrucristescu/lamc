# Test split primitive (#57)

@parts (split "a,b,c" ",")
assert (eq (len parts) 3)
assert (eq (nth parts 0) "a")
assert (eq (nth parts 1) "b")
assert (eq (nth parts 2) "c")

# Split with no delimiter found
@single (split "hello" ",")
assert (eq (len single) 1)
assert (eq (nth single 0) "hello")

# Split with spaces
@words (split "1 2 3" " ")
assert (eq (len words) 3)
assert (eq (nth words 0) "1")
assert (eq (nth words 2) "3")
