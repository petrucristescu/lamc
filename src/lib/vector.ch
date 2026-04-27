# Vector operations for neural network
# Vectors are lists of floats

# Element-wise binary operation
~vecZipWith f,a,b (matchList a (|>_. []) (|>ha. |>ta. matchList b (|>_. []) (|>hb. |>tb. cons (f ha hb) (vecZipWith f ta tb))))

# Element-wise addition
~vecAdd a,b (vecZipWith (|>x. |>y. x + y) a b)

# Element-wise subtraction
~vecSub a,b (vecZipWith (|>x. |>y. x - y) a b)

# Element-wise multiplication
~vecMul a,b (vecZipWith (|>x. |>y. x * y) a b)

# Multiply all elements by scalar
~vecScale s,v (map (|>x. x * s) v)

# Dot product
~vecDot a,b (foldl (|>acc. |>x. acc + x) 0.0 (vecMul a b))

# Sum all elements
~vecSum v (foldl (|>acc. |>x. acc + x) 0.0 v)

# Apply function to each element
~vecMap f,v (map f v)

# Random vector of size n with values in [-scale, scale]
~vecRandom n,scale (match n
    | 0 -> []
    | _ -> cons ((random 0) * 2.0 * scale - scale) (vecRandom (n - 1) scale))

# Zero vector of size n
~vecZeros n (match n
    | 0 -> []
    | _ -> cons 0.0 (vecZeros (n - 1)))

# Constant vector of size n
~vecConst n,val (match n
    | 0 -> []
    | _ -> cons val (vecConst (n - 1) val))

# Index of maximum element
~argmax v (
    ~go i,maxI,maxV,rest (matchList rest (|>_. maxI) (|>h. |>t. matchBool (gt h maxV) (go (i + 1) i h t) (go (i + 1) maxI maxV t)))
    go 1 0 (head v) (tail v)
)
