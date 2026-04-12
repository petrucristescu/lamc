# Dict Library
# Native primitives: get, set, has, keys, values, merge, remove, entries, fromEntries

# Church-encoded dict as association list (Option B)
~assocGet alist,key (matchList alist (|>_. nil) (|>pair. |>rest. matchBool (eq (head pair) key) (nth pair 1) (assocGet rest key)))
~assocSet alist,key,val (cons [key, val] (filter (|>pair. not (eq (head pair) key)) alist))
~assocHas alist,key (any (|>pair. eq (head pair) key) alist)
~assocKeys alist (map (|>pair. head pair) alist)
~assocValues alist (map (|>pair. nth pair 1) alist)
