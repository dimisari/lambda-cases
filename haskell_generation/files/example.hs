{-# language LambdaCase #-}

get_first = fst

ee_recursion :: ( Int, Int ) -> ( Int, Int ) -> Int -> Int -> ( Int, Int, Int )
ee_recursion = \a_coeffs b_coeffs x -> \case
  0 ->( x, get_first a_coeffs, get_first b_coeffs )
  y ->
    ee_recursion (next a_coeffs) (next b_coeffs) (y) (mod x y) where
    next :: ( Int, Int ) -> ( Int, Int )
    next = \( prev_prev, prev ) -> ( prev, prev_prev - div x y * prev )


extended_euclidean :: Int -> Int -> ( Int, Int, Int )
extended_euclidean = ee_recursion (( 1, 0 )) (( 0, 1 ))
