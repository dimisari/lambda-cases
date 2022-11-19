{-# language LambdaCase #-}

get_first = fst

data SortedAbsoluteValues =
  SortedAbsoluteValuesC { get_max_abs :: Int, get_min_abs :: Int }
  deriving Show
to_sorted_asbolute_values = ( \x y -> 
  let
  abs_x = abs x
    :: Int
  abs_y = abs y
    :: Int
  in
  SortedAbsoluteValuesC (max abs_x abs_y) (min abs_x abs_y) )
  :: Int -> Int -> SortedAbsoluteValues
