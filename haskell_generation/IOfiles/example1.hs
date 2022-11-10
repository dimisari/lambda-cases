{-# LANGUAGE LambdaCase #-}

get_first = fst

data PreviousCoeffs =
  PreviousCoeffsC { get_previous_previous :: Int, get_previous :: Int }
  deriving Show
initial_a_coeffs = PreviousCoeffsC (1) (0)
  :: PreviousCoeffs
initial_b_coeffs = PreviousCoeffsC (0) (1)
  :: PreviousCoeffs
data GcdAndCoeffs =
  GcdAndCoeffsC { get_gcd :: Int, get_a :: Int, get_b :: Int }
  deriving Show
extended_euclidean = ee_recursion initial_a_coeffs initial_b_coeffs
  :: Int -> Int -> GcdAndCoeffs
ee_recursion = ( \a_coeffs -> \b_coeffs -> \x -> \case
  0 -> GcdAndCoeffsC (x) (get_previous_previous a_coeffs) (get_previous_previous b_coeffs)
  y -> 
    let
    compute_new = (\(PreviousCoeffsC previous_previous previous) ->
      PreviousCoeffsC (previous) (previous_previous - div x y * previous) )
      :: PreviousCoeffs -> PreviousCoeffs
    in
    ee_recursion (compute_new a_coeffs) (compute_new b_coeffs) y (mod x y) )
  :: PreviousCoeffs -> PreviousCoeffs -> Int -> Int -> GcdAndCoeffs
