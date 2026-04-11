# Dict Library
# Native primitives: get, set, has, keys, values, merge, remove, entries, fromEntries

# Church-encoded dict as association list (Option B)
# A church_dict is a list of [key, value] pairs

# Lookup in association list
~assocGet alist,key (
    matchList alist
        (|>_. nil)
        (|>pair. |>rest.
            matchBool (eq (head pair) key)
                (nth pair 1)
                (assocGet rest key)))

# Set in association list (replace or append)
~assocSet alist,key,val (
    cons [key, val] (filter (|>pair. not (eq (head pair) key)) alist))

# Check if key exists
~assocHas alist,key (
    any (|>pair. eq (head pair) key) alist)

# Get all keys
~assocKeys alist (map (|>pair. head pair) alist)

# Get all values
~assocValues alist (map (|>pair. nth pair 1) alist)

# Map over values
~mapDict f,d (set d "placeholder" 0, fromEntries (map (|>pair. [head pair, f (nth pair 1)]) (entries d)))
