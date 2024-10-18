{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

extended_euclidean'' :: (P.Integer, P.Integer) -> (P.Integer, P.Integer, P.Integer)
extended_euclidean'' =
  (\pA0 -> ee_recursion'''(ft2((1 :: P.Integer), (0 :: P.Integer)), ft2((0 :: P.Integer), (1 :: P.Integer)), pA0))

ee_recursion''' :: ((P.Integer, P.Integer), (P.Integer, P.Integer), (P.Integer, P.Integer)) -> (P.Integer, P.Integer, P.Integer)
ee_recursion''' =
  \(a_coeffs, b_coeffs, (x, pA0)) ->
  case pA0 of
    0 -> ft3(x, p1st(a_coeffs), p1st(b_coeffs))
    y ->
      let
      next' :: (P.Integer, P.Integer) -> (P.Integer, P.Integer)
      next' =
        (\x' -> (c1st(p2nd(x')) .> c2nd(p1st(x') !- a'div'(x, y) !* p2nd(x'))) x')
      in
      ee_recursion'''(next'(a_coeffs), next'(b_coeffs), ft2(y, a'mod'(x, y)))

read_two_ints :: A'FromIO (P.Integer, P.Integer)
read_two_ints =
  print'("Please give me 2 ints") !>> 
  get_line !>>= (\pA0 -> split'to_words(pA0)) .> \pA0 ->
  case pA0 of
    [x, y] -> ft2(from_string'(x), from_string'(y)) &> (\pA0 -> a'from_io(pA0))
    _ -> throw_err'("You didn't give me 2 ints")

main :: IO
main =
  read_two_ints !>>= \(i1, i2) ->
  print'("The result is " !+ extended_euclidean''(i1, i2))