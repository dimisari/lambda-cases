{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

my_gcd_of'and' :: (P.Integer, P.Integer) -> P.Integer
my_gcd_of'and' =
  \(x, pA0) ->
  case pA0 of
    0 -> x
    y -> my_gcd_of'and'(y, a'mod'(x, y))

read_two_ints :: A'FromIO (P.Integer, P.Integer)
read_two_ints =
  print'("Please give me 2 ints") !>> 
  get_line !>>= (\pA0 -> split'to_words(pA0)) .> \pA0 ->
  case pA0 of
    [s1, s2] -> ft2(from_string'(s1), from_string'(s2)) &> (\pA0 -> a'from_io(pA0))
    _ -> throw_err'("You didn't give me 2 ints")

ints'''to_message :: (P.Integer, P.Integer, P.Integer) -> P.String
ints'''to_message =
  \(x, y, gcd) -> "The GCD of " !+ x !+ " and " !+ y !+ " is " !+ gcd

main :: IO
main =
  read_two_ints !>>= \(i1, i2) ->
  ints'''to_message(i1, i2, my_gcd_of'and'(i1, i2)) &> (\pA0 -> print'(pA0))