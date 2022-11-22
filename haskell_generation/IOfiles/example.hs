{-# language LambdaCase #-}

get_first = fst

data SortedAbsoluteValues =
  SortedAbsoluteValuesC { get_max_abs :: Int, get_min_abs :: Int }
  deriving Show
gcd_help = (\(SortedAbsoluteValuesC max_abs min_abs) ->
  
  let
  b = (min_abs == 0)
    :: Int
  c = SortedAbsoluteValuesC (min_abs) (mod max_abs min_abs)
    :: SortedAbsoluteValues
  f = \case
    1 -> max_abs
    0 -> gcd_help c
    :: Int -> Int
  in
  f b )
  :: SortedAbsoluteValues -> Int
