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
    [x, y] -> ft2(from_string'(x), from_string'(y)) &> (\pA0 -> a'from_io(pA0))
    _ -> throw_err'("You didn't give me 2 ints")

data NumsAndGcd =
  NumsAndGcd' { x :: P.Integer, y :: P.Integer, gcd_ :: P.Integer }

instance FromTuple3 P.Integer P.Integer P.Integer NumsAndGcd where
  ft3 = \(x1, x2, x3) -> NumsAndGcd' x1 x2 x3

c0x :: P.Integer -> NumsAndGcd -> NumsAndGcd
c0y :: P.Integer -> NumsAndGcd -> NumsAndGcd
c0gcd_ :: P.Integer -> NumsAndGcd -> NumsAndGcd
c0x = \new x -> x { x = new }
c0y = \new x -> x { y = new }
c0gcd_ = \new x -> x { gcd_ = new }

nag'to_message :: NumsAndGcd -> P.String
nag'to_message =
  \nag -> "The GCD of " !+ x(nag) !+ " and " !+ y(nag) !+ " is " !+ gcd_(nag)

main :: IO
main =
  read_two_ints !>>= \(i1, i2) ->
  ft3(i1, i2, my_gcd_of'and'(i1, i2)) &> (\pA0 -> nag'to_message(pA0)) &> (\pA0 -> print'(pA0))