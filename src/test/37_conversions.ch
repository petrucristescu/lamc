# Test toFloat and toInt primitives (#58)

# toFloat from string
assert (eq (toFloat "3.14") 3.14)
assert (eq (toFloat "0") 0.0)
assert (eq (toFloat "42") 42.0)

# toFloat from int
assert (eq (toFloat 5) 5.0)

# toInt from string
assert (eq (toInt "42") 42)
assert (eq (toInt "0") 0)

# toInt from float (truncates)
assert (eq (toInt 3.7) 3)

# Parsing pixel values (NN use case)
@pixel_str "255"
@pixel (toFloat pixel_str)
assert (eq pixel 255.0)
@normalized (pixel / 255.0)
assert (lt (abs (normalized - 1.0)) 0.0001)

true
