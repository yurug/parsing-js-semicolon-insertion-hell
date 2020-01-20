type goal = statement

and statement =
| DoWhile of statement * expression
| Assign of identifier * expression
| Block of statement list
| Break of identifier
| For of expression * expression * expression * statement
| Nop

and identifier = string

and expression = Var of identifier
