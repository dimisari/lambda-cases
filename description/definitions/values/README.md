# Values
## cases syntax, use\_fields syntax, let-output syntax, lambda values
- "cases" syntax let's you pattern match with literals or different or\_type
  values (Should be extended recursively for tuple\_types in the future).
- "use\_fields" syntax let's you refer to sub-values of a tuple\_type value
  with the names given in the tuple\_type defintion.
- in both previous cases "value" refers to the whole value.
- "let ... output ..." syntax let's you define some intermediate values that
  the output is going to use.
