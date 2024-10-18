let
less_l :: ListOf's P.Integer
less_l =
  filter'with'(tail(l), (\pA0 -> pA0 !< head(l)))

greater_l :: ListOf's P.Integer
greater_l =
  filter'with'(tail(l), (\pA0 -> pA0 !>= head(l)))
in


let
sum_list :: ListOf's P.Integer -> P.Integer
sum_list =
  \pA0 ->
  case pA0 of
    empty_l -> (0 :: P.Integer)
    Cnon_empty_l l -> head(l) !+ sum_list'(tail(l))
in


let
s1 :: P.String
s1 =
  "Hello, my name is Struggling Programmer."

s2 :: P.String
s2 =
  "I have tried way too many times to fit a big chunk of text"

s3 :: P.String
s3 =
  "inside my program, without it hitting the half-screen mark!"

s4 :: P.String
s4 =
  "I am so glad I finally discovered lcases!"
in


