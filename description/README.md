# Definitions
## values, types, type\_predicates, type\_theorems
A program is a list of value/type definitions.
Value definitions must include the type of the value except
when the value is equal to another value or literal
(aka: doesn't have lamda abstractions or operators).
# Other Syntax
## application variations, import
For both values and types the expressions below are equivalent:
x, y *==> f
x==>f<==y
f(x,y)
T1, T2 *==> Or
T1==>Or<==T2
Or(T1, T2)
# Initial Module
