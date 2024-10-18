type Ints = ListOf's P.Integer

type IntStringPairs = ListOf's (P.Integer, P.String)

type IO = A'FromIO EmptyVal

type ErrOrRes' a1 = Error'OrResult' P.String a1

type Parse'FuncT a1 = P.String -> (a1, P.String)

type IntsTwo = ListOf's P.Integer

type IntStringPairsTwo = ListOf's (P.Integer, P.String)

type IOTwo = A'FromIO EmptyVal

