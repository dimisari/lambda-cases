(5 :: P.Integer) !* 'a'

(1 :: P.Integer) !+ (2 :: P.Integer)

(1 :: P.Integer) !+ x !* (3 :: P.Integer) !^ y

"Hello " !+ "World!"

x &> f &> g

f .> g .> h

x !== y

x !>= y !- z !& x !< (2 :: P.Integer) !* y

get_line !>> get_line !>>= \line -> print'("Second line: " !+ line)

(\pA0 -> (2 :: P.Integer) !* pA0)

(\pA0 -> pA0 !- (1 :: P.Integer))

(\pA0 -> "Hello " !+ "it's me, " !+ pA0)

(\(pA0, pA1) -> pA0 !+ "string in the middle of the arguments" !+ pA1)

x !+ p1st(x)

fsf &> (\x' -> p2nd(x'))

(\x' -> p2nd(x')) &> fsf

(\x' -> b(x')) &> a

a &> (\x' -> b(x'))

