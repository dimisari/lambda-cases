{-# language LambdaCase #-}

get_first = fst

data PrevCoeffs =
  PrevCoeffsC { get_prev_prev :: Int, get_prev :: Int }
  deriving Show

data GcdAndCoeffs =
  GcdAndCoeffsC { get_gcd :: Int, get_a :: Int, get_b :: Int }
  deriving Show

ee_recursion :: PrevCoeffs -> PrevCoeffs -> Int -> Int -> GcdAndCoeffs
ee_recursion = \a_coeffs -> \b_coeffs -> \x -> \case
  0 -> 
    GcdAndCoeffsC (x) (get_prev_prev a_coeffs) (get_prev_prev b_coeffs)
  y -> 
    ee_recursion (next a_coeffs) (next b_coeffs) (y) (mod x y) where
    next :: PrevCoeffs -> PrevCoeffs
    next = \value@(PrevCoeffsC prev_prev prev) -> 
      PrevCoeffsC (prev) (prev_prev - div x y * prev)


init_a_coeffs :: PrevCoeffs
init_a_coeffs = 
  PrevCoeffsC (1) (0)

init_b_coeffs :: PrevCoeffs
init_b_coeffs = 
  PrevCoeffsC (0) (1)

extended_euclidean :: Int -> Int -> GcdAndCoeffs
extended_euclidean = ee_recursion (init_a_coeffs) (init_b_coeffs)
