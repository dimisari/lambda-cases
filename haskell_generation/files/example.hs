{-# language LambdaCase #-}

get_first = fst

gcd_help :: Int -> Int -> Int
gcd_help = \x -> \y -> (\case
  True -> x
  False -> gcd_help (y) (mod x y)) (y == 0)

my_gcd :: Int -> Int -> Int
my_gcd = \x -> \y -> abs (gcd_help (x) (y))
