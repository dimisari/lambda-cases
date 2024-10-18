ft2(x, y)

ft2((1 :: P.Integer), "What's up, doc?")

ft3((2 :: P.Integer), "Alrighty then!", (3.14 :: P.Double))

ft4(x, y, z, w)

ft3((1 :: P.Integer), my_function, \(x, y, z) -> (x !^ (2 :: P.Integer) !+ y !^ (2 :: P.Integer) !+ z !^ (2 :: P.Integer)) !^ ((1 :: P.Integer) !/ (2 :: P.Integer)))

(\pA0 -> ft2((42 :: P.Integer), pA0))

(\(pA0, pA1) -> ft3(pA0, (3.14 :: P.Double), pA1))

(\(pA0, pA1) -> ft3(pA0, pA1, "Hello from 3rd field"))

ft2(x1 !+ x2, y1 !+ y2)

