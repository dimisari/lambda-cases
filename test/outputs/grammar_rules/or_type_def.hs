data Bool =
  Ctrue |
  Cfalse

data Possibly' a1 =
  Cthe_value a1 |
  Cno_value

data Error'OrResult' a1 a2 =
  Cerror a1 |
  Cresult a2

data ListOf's a1 =
  Cnon_empty_l (NonEmptyListOf's a1) |
  Cempty_l

data Bool =
  Ctrue |
  Cfalse

data Possibly' a1 =
  Cthe_value a1 |
  Cno_value

data ListOf's a1 =
  Cnon_empty_l (NonEmptyListOf's a1) |
  Cempty_l

data A'Or' a1 a2 =
  Ct1 a1 |
  Ct2 a2

