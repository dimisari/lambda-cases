\x -> (17 :: P.Integer) !* x !+ (42 :: P.Integer)

\(a, b) -> a !+ (2 :: P.Integer) !* b

\(x, y, z) -> sqrt'(x !^ (2 :: P.Integer) !+ y !^ (2 :: P.Integer) !+ z !^ (2 :: P.Integer))

\_ -> (42 :: P.Integer)

\(x, _, z) -> x !+ z

\((x1, y1), (x2, y2)) -> ft2(x1 !+ x2, y1 !+ y2)

\x -> (\y -> x)

