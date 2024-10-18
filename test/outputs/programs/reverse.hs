{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

reverse' :: forall a1. ListOf's a1 -> ListOf's a1
reverse' =
  \pA0 ->
  case pA0 of
    [] -> []
    head : tail -> reverse'(tail) !+ head

my_int_list :: ListOf's P.Integer
my_int_list =
  [(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer), (4 :: P.Integer), (5 :: P.Integer)]

main :: IO
main =
  reverse'(my_int_list) &> (\pA0 -> P.show(pA0)) &> (\pA0 -> print'(pA0))