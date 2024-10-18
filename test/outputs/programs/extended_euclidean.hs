{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

data Coeffs =
  Coeffs' { prev :: P.Integer, curr :: P.Integer }

instance FromTuple2 P.Integer P.Integer Coeffs where
  ft2 = \(x1, x2) -> Coeffs' x1 x2

c0prev :: P.Integer -> Coeffs -> Coeffs
c0curr :: P.Integer -> Coeffs -> Coeffs
c0prev = \new x -> x { prev = new }
c0curr = \new x -> x { curr = new }

data GcdAndCoeffs =
  GcdAndCoeffs' { gcd_ :: P.Integer, a :: P.Integer, b :: P.Integer }

instance FromTuple3 P.Integer P.Integer P.Integer GcdAndCoeffs where
  ft3 = \(x1, x2, x3) -> GcdAndCoeffs' x1 x2 x3

c0gcd_ :: P.Integer -> GcdAndCoeffs -> GcdAndCoeffs
c0a :: P.Integer -> GcdAndCoeffs -> GcdAndCoeffs
c0b :: P.Integer -> GcdAndCoeffs -> GcdAndCoeffs
c0gcd_ = \new x -> x { gcd_ = new }
c0a = \new x -> x { a = new }
c0b = \new x -> x { b = new }

extended_euclidean_of'and' :: (P.Integer, P.Integer) -> GcdAndCoeffs
extended_euclidean_of'and' =
  let
  init_a_coeffs :: Coeffs
  init_a_coeffs =
    ft2((1 :: P.Integer), (0 :: P.Integer))

  init_b_coeffs :: Coeffs
  init_b_coeffs =
    ft2((0 :: P.Integer), (1 :: P.Integer))

  ee_recursion''' :: (Coeffs, Coeffs, (P.Integer, P.Integer)) -> GcdAndCoeffs
  ee_recursion''' =
    \(a_coeffs, b_coeffs, (x, pA0)) ->
    case pA0 of
      0 -> ft3(x, prev(a_coeffs), prev(b_coeffs))
      y ->
        let
        next' :: Coeffs -> Coeffs
        next' =
          (\x' -> (c0prev(curr(x')) .> c0curr(prev(x') !- a'div'(x, y) !* curr(x'))) x')
        in
        ee_recursion'''(next'(a_coeffs), next'(b_coeffs), ft2(y, a'mod'(x, y)))
  in
  (\pA0 -> ee_recursion'''(init_a_coeffs, init_b_coeffs, pA0))

read_two_ints :: A'FromIO (P.Integer, P.Integer)
read_two_ints =
  print'("Please give me 2 ints") !>> 
  get_line !>>= (\pA0 -> split'to_words(pA0)) .> \pA0 ->
  case pA0 of
    [i, j] -> ft2(from_string'(i), from_string'(j)) &> (\pA0 -> a'from_io(pA0))
    _ -> throw_err'("You didn't give me 2 ints")

print_gcd_and_coeffs_of' :: GcdAndCoeffs -> IO
print_gcd_and_coeffs_of' =
  \gac ->
  let
  message :: P.String
  message =
    "Gcd: " !+ gcd_(gac) !+ "\nCoefficients: a = " !+ a(gac) !+ ", b = " !+ b(gac)
  in
  print'(message)

main :: IO
main =
  read_two_ints !>>= 
  (\(pA0, pA1) -> extended_euclidean_of'and'(pA0, pA1)) .> (\pA0 -> print_gcd_and_coeffs_of'(pA0))