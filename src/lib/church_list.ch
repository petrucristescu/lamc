# Church-encoded lists (pure lambda calculus)
# A list is its own right fold: list = λc. λn. body
# nil = λc. λn. n
# cons x xs = λc. λn. c x (xs c n)

# Church nil: ignores the combining function, returns the base
~church_nil (|>c. |>n. n)

# Church cons: prepend element to a Church list
~church_cons x,xs (|>c. |>n. c x (xs c n))

# Church fold (just apply the list — it IS its fold)
~church_fold f,init,l (l f init)

# Church map: transform each element
~church_map f,l (|>c. |>n. l (|>x. |>acc. c (f x) acc) n)

# Church head: extract the first element
~church_head l (l (|>x. |>_. x) 0)

# Church sum: sum all elements
~church_sum l (l (|>x. |>acc. x + acc) 0)

# Church length: count elements
~church_length l (l (|>_. |>acc. acc + 1) 0)
