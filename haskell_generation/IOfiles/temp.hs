{-# LANGUAGE LambdaCase #-}

get_first = fst

data PreviousCoeffs =
  PreviousCoeffsC { get_previous_previous :: Int, get_previous :: Int }
initial_a_coeffs = PreviousCoeffsC 1 0
  :: PreviousCoeffs
initial_b_coeffs = PreviousCoeffsC 0 1
  :: PreviousCoeffs
