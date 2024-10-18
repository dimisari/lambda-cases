{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

type IsSorted = P.Bool

bubble_sort_pass' :: ListOf's P.Integer -> (ListOf's P.Integer, IsSorted)
bubble_sort_pass' =
  \pA0 ->
  case pA0 of
    [] -> ft2([], P.True)
    [x] -> ft2([x], P.True)
    x1 : x2 : rest ->
      let
      not_sorted' :: (ListOf's P.Integer, IsSorted) -> (ListOf's P.Integer, IsSorted)
      not_sorted' =
        (\x' -> c2nd(P.False) x')

      prepend'to' :: (P.Integer, (ListOf's P.Integer, IsSorted)) -> (ListOf's P.Integer, IsSorted)
      prepend'to' =
        \(elem, (l, b)) -> ft2(elem !+ l, b)
      in
      (x1 !> x2) &> \pA0 ->
      case pA0 of
        P.True -> (\pA0 -> not_sorted'(pA0)) <& prepend'to'(x2, bubble_sort_pass'(x1 !+ rest))
        P.False -> prepend'to'(x1, bubble_sort_pass'(x2 !+ rest))

bubble_sort' :: ListOf's P.Integer -> ListOf's P.Integer
bubble_sort' =
  (\pA0 -> bubble_sort_pass'(pA0)) .> 
  \(l, pA0) ->
  case pA0 of
    P.True -> l
    P.False -> bubble_sort'(l)

initial :: ListOf's P.Integer
initial =
  let
  compute_list'' :: (P.Integer, P.Integer) -> ListOf's P.Integer
  compute_list'' =
    \(pA0, seed) ->
    case pA0 of
      0 -> []
      n ->
        let
        new_seed :: P.Integer
        new_seed =
          a'mod'(seed !* (137 :: P.Integer) !+ (236 :: P.Integer) !- n, (101 :: P.Integer))
        in
        new_seed !+ compute_list''(n !- (1 :: P.Integer), new_seed)
  in
  compute_list''((16 :: P.Integer), (65 :: P.Integer))

main :: IO
main =
  print'("Initial list = " !+ initial) !>> 
  print'("Sorted list = " !+ bubble_sort'(initial))