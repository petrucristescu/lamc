# I/O Library
# Native primitives: readFile, writeFile, appendFile, fileExists,
#   deleteFile, readLines, writeLines

# Church-encoded IO monad
# An IO action is a lambda that performs the effect when applied to a dummy arg:
#   IO a = |>_. a  (thunk)

# Wrap a pure value as an IO action
~pureIO v (|>_. v)

# Bind: run first action, pass result to function that returns next action
~bindIO action,f (|>_. (f (action 0)) 0)

# Map: transform the result of an IO action
~mapIO f,action (|>_. f (action 0))

# Sequence: run two actions, return result of second
~seqIO a,b (|>_. a 0, b 0)

# Lift native I/O into IO monad
~readFileIO path (|>_. readFile path)
~writeFileIO path,content (|>_. writeFile path content)
~appendFileIO path,content (|>_. appendFile path content)
~deleteFileIO path (|>_. deleteFile path)
~readLinesIO path (|>_. readLines path)
~writeLinesIO path,lines (|>_. writeLines path lines)

# Run an IO action (extract the value)
~runIO action (action 0)
