{-# language LambdaCase #-}

get_first = fst

data PreviousCoeffs =
  PreviousCoeffsC { get_previous_previous :: Int, get_previous :: Int }
  deriving Show

data GcdAndCoeffs =
  GcdAndCoeffsC { get_gcd :: Int, get_a :: Int, get_b :: Int }
  deriving Show

ee_recursion :: PreviousCoeffs -> PreviousCoeffs -> Int -> Int -> GcdAndCoeffs
ee_recursion = \a_coeffs b_coeffs x -> \case
  0 ->GcdAndCoeffsC
    (x)
    (get_previous_previous a_coeffs)
    (get_previous_previous b_coeffs)
  y ->
    let
    
    next :: PreviousCoeffs -> PreviousCoeffs
    next = \(PreviousCoeffsC previous_previous previous) -> PreviousCoeffsC
      (previous)
      (previous_previous - div x y * previous)

    in
    ee_recursion (next a_coeffs) (next b_coeffs) (y) (mod x y)

initial_a_coeffs :: PreviousCoeffs
initial_a_coeffs = PreviousCoeffsC
  (1)
  (0)

initial_b_coeffs :: PreviousCoeffs
initial_b_coeffs = PreviousCoeffsC
  (0)
  (1)

extended_euclidean :: Int -> Int -> GcdAndCoeffs
extended_euclidean = ee_recursion (initial_a_coeffs) (initial_b_coeffs)
