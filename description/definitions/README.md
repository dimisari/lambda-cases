# Types
## or\_type, tuple\_type
We have 2 types of types: or\_types and tuple\_types. Their behavior is as
their names suggest. Namely: 
- or\_types: Many values of different form can be of the same or\_type. For
  example the list type is an or\_type. It's either empty or non-empty.
- tuple\_types: Values of these types contain many different (sub)values (aka
  tuples)
# Values
## cases syntax, use\_fields syntax, let-output syntax, lambda values
- "cases" syntax let's you pattern match with literals or different or\_type
  values (Should be extended recursively for tuple\_types in the future).
- "use\_fields" syntax let's you refer to sub-values of a tuple\_type value
  with the names given in the tuple\_type defintion.
- in both previous cases "value" refers to the whole value.
- "let ... output ..." syntax let's you define some intermediate values that
  the output is going to use.
- 
# type\_predicates and type\_theorems
