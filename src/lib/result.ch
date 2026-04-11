# Church-encoded Result type
# Ok value  = |>onOk. |>onErr. onOk value
# Err msg   = |>onOk. |>onErr. onErr msg

# Constructors
~ok v (|>onOk. |>onErr. onOk v)
~err msg (|>onOk. |>onErr. onErr msg)

# Eliminator: matchResult result onOk onErr
~matchResult r,onOk,onErr (r onOk onErr)

# Map over Ok value
~mapResult f,r (r (|>v. ok (f v)) (|>e. err e))

# Chain (flatMap/bind)
~bindResult f,r (r f (|>e. err e))

# Get value or default
~unwrapOr default,r (r (|>v. v) (|>_. default))

# Check if Ok
~isOk r (r (|>_. true) (|>_. false))

# Check if Err
~isErr r (r (|>_. false) (|>_. true))
