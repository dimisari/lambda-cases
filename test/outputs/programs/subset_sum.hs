{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

sub_set_sum'' :: (ListOf's P.Integer, P.Integer) -> Possibly' (ListOf's P.Integer)
sub_set_sum'' =
  \(pA0, pA1) ->
  case (pA0, pA1) of
    (_, 0) -> P.Just([])
    ([], _) -> P.Nothing
    (x : xs, i) ->
      (x !> i) &> \pA0 ->
      case pA0 of
        P.True -> sub_set_sum''(xs, i)
        P.False ->
          sub_set_sum''(xs, i !- x) &> \pA0 ->
          case pA0 of
            P.Just ys -> P.Just((x !+ ys))
            P.Nothing -> sub_set_sum''(xs, i)

list :: ListOf's P.Integer
list =
  [(1 :: P.Integer), (9 :: P.Integer), (4 :: P.Integer), (5 :: P.Integer), (6 :: P.Integer)]

res_msg :: Possibly' (ListOf's P.Integer) -> P.String
res_msg =
  \pA0 ->
  case pA0 of
    P.Nothing -> "Not possible"
    P.Just l -> "Here is the subset: " !+ l

main :: IO
main =
  print'("Trying 17") !>> sub_set_sum''(list, (17 :: P.Integer)) &> res_msg &> P.print !>> 
  print'("Trying 20") !>> sub_set_sum''(list, (20 :: P.Integer)) &> res_msg &> P.print