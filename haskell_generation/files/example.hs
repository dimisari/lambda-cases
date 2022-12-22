{-# language LambdaCase #-}

get_first = fst

data SortedAbsoluteValues =
  SortedAbsoluteValuesC { get_max_abs :: Int, get_min_abs :: Int }
  deriving Show

to_sorted_asbolute_values :: Int -> Int -> SortedAbsoluteValues
to_sorted_asbolute_values = \x y -> 
  let
  
  abs_x :: Int
  abs_x = (abs x)

  abs_y :: Int
  abs_y = (abs y)

  in
  (SortedAbsoluteValuesC
    (max (abs_x) (abs_y))
    (min (abs_x) (abs_y)))

gcd_help :: SortedAbsoluteValues -> Int
gcd_help = \(SortedAbsoluteValuesC max_abs min_abs) -> ((\case
  True -> max_abs
  False -> (gcd_help (SortedAbsoluteValuesC
    (min_abs)
    (((mod max_abs) min_abs))))) (min_abs == 0))
