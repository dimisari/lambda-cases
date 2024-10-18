((1 :: P.Integer) !+ (2 :: P.Integer))

((((1 :: P.Integer) !+ (2 :: P.Integer)) !* (3 :: P.Integer)) !^ (4 :: P.Integer))

(\n -> (3 :: P.Integer) !* n !+ (1 :: P.Integer))

(get_line !>>= \line -> print'("Line is: " !+ line))

