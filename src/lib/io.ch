# I/O Library
# Native primitives: readFile, writeFile, appendFile, fileExists,
#   deleteFile, readLines, writeLines

# Church-encoded IO monad
~pureIO v (|>_. v)
~bindIO action,f (|>_. (f (action 0)) 0)
~mapIO f,action (|>_. f (action 0))
~runIO action (action 0)

# Lift native I/O into IO monad
~readFileIO path (|>_. readFile path)
~writeFileIO path,content (|>_. writeFile path content)
~appendFileIO path,content (|>_. appendFile path content)
~deleteFileIO path (|>_. deleteFile path)
~readLinesIO path (|>_. readLines path)
~writeLinesIO path,lines (|>_. writeLines path lines)
