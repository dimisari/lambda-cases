{-# LANGUAGE LambdaCase #-}

get_first = fst

data PreviousCoeffs =
  PreviousCoeffsC { get_previous_previous :: Int, get_previous :: Int }
initial_a_coeffs = ( 1, 0 )
  :: PreviousCoeffs
initial_b_coeffs = ( 0, 1 )
  :: PreviousCoeffs
data GcdAndCoeffs =
  GcdAndCoeffsC { get_gcd :: Int, get_a :: Int, get_b :: Int }
extended_euclidean = ee_recursion initial_a_coeffs initial_b_coeffs
  :: Int -> Int -> GcdAndCoeffs
ee_recursion = ( \a_coeffs -> \b_coeffs -> \x -> \case
  0 -> ( x, get_previous_previous a_coeffs, get_previous_previous b_coeffs )
  y -> 
    let
    compute_new = ( \( prev_prev, prev ) -> ( prev, prev_prev - div x y * prev ) )
      :: PreviousCoeffs -> PreviousCoeffs
    in
    ee_recursion (compute_new a_coeffs) (compute_new b_coeffs) y (mod x y) )
  :: PreviousCoeffs -> PreviousCoeffs -> Int -> Int -> GcdAndCoeffs
