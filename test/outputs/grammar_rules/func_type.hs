P.String -> P.String

P.Double -> P.Integer

a1 -> a1

(P.Integer, P.Integer) -> P.Integer

(P.Double, P.Double, P.Double) -> P.Double

(a1 -> a2, a2 -> a3) -> (a1 -> a3)

(P.Integer -> P.Integer) -> (P.Integer -> P.Integer)

((P.Integer -> P.Integer) -> P.Integer, P.Integer) -> P.Integer

ListOf's a1 -> ListOf's a1

ListOf's (ListOf's a1) -> ListOf's (ListOf's a1)

((A, A) -> A, A, ListOf's A) -> A

