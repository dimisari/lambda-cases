data Name =
  Name' { first_name :: P.String, last_name :: P.String }

instance FromTuple2 P.String P.String Name where
  ft2 = \(x1, x2) -> Name' x1 x2

c0first_name :: P.String -> Name -> Name
c0last_name :: P.String -> Name -> Name
c0first_name = \new x -> x { first_name = new }
c0last_name = \new x -> x { last_name = new }

data Date =
  Date' { day :: P.Integer, month :: P.Integer, year :: P.Integer }

instance FromTuple3 P.Integer P.Integer P.Integer Date where
  ft3 = \(x1, x2, x3) -> Date' x1 x2 x3

c0day :: P.Integer -> Date -> Date
c0month :: P.Integer -> Date -> Date
c0year :: P.Integer -> Date -> Date
c0day = \new x -> x { day = new }
c0month = \new x -> x { month = new }
c0year = \new x -> x { year = new }

data MathematicianInfo =
  MathematicianInfo' { name :: Name, nationality :: P.String, date_of_birth :: Date }

instance FromTuple3 Name P.String Date MathematicianInfo where
  ft3 = \(x1, x2, x3) -> MathematicianInfo' x1 x2 x3

c0name :: Name -> MathematicianInfo -> MathematicianInfo
c0nationality :: P.String -> MathematicianInfo -> MathematicianInfo
c0date_of_birth :: Date -> MathematicianInfo -> MathematicianInfo
c0name = \new x -> x { name = new }
c0nationality = \new x -> x { nationality = new }
c0date_of_birth = \new x -> x { date_of_birth = new }

data TreeOf's a1 =
  TreeOf's' { root :: a1, subtrees :: ListOf's (TreeOf's a1) }

instance FromTuple2 a1 (ListOf's (TreeOf's a1)) (TreeOf's a1) where
  ft2 = \(x1, x2) -> TreeOf's' x1 x2

c0root :: a1 -> TreeOf's a1 -> TreeOf's a1
c0subtrees :: ListOf's (TreeOf's a1) -> TreeOf's a1 -> TreeOf's a1
c0root = \new x -> x { root = new }
c0subtrees = \new x -> x { subtrees = new }

data Indexed' a1 =
  Indexed'' { index :: P.Integer, val :: a1 }

instance FromTuple2 P.Integer a1 (Indexed' a1) where
  ft2 = \(x1, x2) -> Indexed'' x1 x2

c0index :: P.Integer -> Indexed' a1 -> Indexed' a1
c0val :: a1 -> Indexed' a1 -> Indexed' a1
c0index = \new x -> x { index = new }
c0val = \new x -> x { val = new }

data Name =
  Name' { first_name :: P.String, last_name :: P.String }

instance FromTuple2 P.String P.String Name where
  ft2 = \(x1, x2) -> Name' x1 x2

c0first_name :: P.String -> Name -> Name
c0last_name :: P.String -> Name -> Name
c0first_name = \new x -> x { first_name = new }
c0last_name = \new x -> x { last_name = new }

data Date =
  Date' { day :: P.Integer, month :: P.Integer, year :: P.Integer }

instance FromTuple3 P.Integer P.Integer P.Integer Date where
  ft3 = \(x1, x2, x3) -> Date' x1 x2 x3

c0day :: P.Integer -> Date -> Date
c0month :: P.Integer -> Date -> Date
c0year :: P.Integer -> Date -> Date
c0day = \new x -> x { day = new }
c0month = \new x -> x { month = new }
c0year = \new x -> x { year = new }

data MathematicianInfo =
  MathematicianInfo' { name :: Name, nationality :: P.String, date_of_birth :: Date }

instance FromTuple3 Name P.String Date MathematicianInfo where
  ft3 = \(x1, x2, x3) -> MathematicianInfo' x1 x2 x3

c0name :: Name -> MathematicianInfo -> MathematicianInfo
c0nationality :: P.String -> MathematicianInfo -> MathematicianInfo
c0date_of_birth :: Date -> MathematicianInfo -> MathematicianInfo
c0name = \new x -> x { name = new }
c0nationality = \new x -> x { nationality = new }
c0date_of_birth = \new x -> x { date_of_birth = new }

data NonEmptyListOf's a1 =
  NonEmptyListOf's' { head :: a1, tail :: ListOf's a1 }

instance FromTuple2 a1 (ListOf's a1) (NonEmptyListOf's a1) where
  ft2 = \(x1, x2) -> NonEmptyListOf's' x1 x2

c0head :: a1 -> NonEmptyListOf's a1 -> NonEmptyListOf's a1
c0tail :: ListOf's a1 -> NonEmptyListOf's a1 -> NonEmptyListOf's a1
c0head = \new x -> x { head = new }
c0tail = \new x -> x { tail = new }

