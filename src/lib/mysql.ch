# MySQL Library
# Native primitives: mysqlConnect, mysqlQuery, mysqlExec, mysqlClose

# Church-encoded IO wrappers
~mysqlQueryIO conn,sql (|>_. mysqlQuery conn sql)
~mysqlExecIO conn,sql (|>_. mysqlExec conn sql)

# Find one row (first result or nil)
~mysqlFindOne conn,sql (
    @rows (mysqlQuery conn sql)
    matchList rows (|>_. nil) (|>h. |>_. h))

# Build simple WHERE query
~mysqlFind conn,table,where_col,where_val (
    mysqlQuery conn (str ["SELECT * FROM ", table, " WHERE ", where_col, " = '", toString where_val, "'"]))
