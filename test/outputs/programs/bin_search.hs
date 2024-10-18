{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

data Comparison =
  Clesser |
  Cequal |
  Cgreater

compare'with' :: (P.Integer, P.Integer) -> Comparison
compare'with' =
  \(a, b) ->
  (a !< b) &> \pA0 ->
  case pA0 of
    P.True -> Clesser
    P.False ->
      (a !== b) &> \pA0 ->
      case pA0 of
        P.True -> Cequal
        P.False -> Cgreater

my_split'at' :: forall a1. (ListOf's a1, P.Integer) -> Possibly' (a1, (ListOf's a1, ListOf's a1))
my_split'at' =
  \(pA0, pA1) ->
  case (pA0, pA1) of
    ([], _) -> P.Nothing
    (head : tail, 0) -> P.Just(ft2(head, ft2([], tail)))
    (head : tail, n) ->
      my_split'at'(tail, n !- (1 :: P.Integer)) &> \pA0 ->
      case pA0 of
        P.Nothing -> P.Nothing
        P.Just (item, (left_l, right_l)) ->
          P.Just(ft2(item, ft2(head !+ left_l, right_l)))

bin_search'' :: (ListOf's P.Integer, P.Integer) -> Possibly' P.Integer
bin_search'' =
  \(list, target) ->
  let
  middle_ind :: P.Integer
  middle_ind =
    a'div'(a'length(list) !- (1 :: P.Integer), (2 :: P.Integer))
  in
  my_split'at'(list, middle_ind) &> \pA0 ->
  case pA0 of
    P.Nothing -> P.Nothing
    P.Just (middle, (left_l, right_l)) ->
      compare'with'(middle, target) &> \pA0 ->
      case pA0 of
        Clesser ->
          bin_search''(right_l, target) &> \pA0 ->
          case pA0 of
            P.Nothing -> P.Nothing
            P.Just i -> P.Just((middle_ind !+ (1 :: P.Integer) !+ i))
        Cequal -> P.Just(middle_ind)
        Cgreater -> bin_search''(left_l, target)

list :: ListOf's P.Integer
list =
  [(2 :: P.Integer), (4 :: P.Integer), (6 :: P.Integer), (8 :: P.Integer)]

result_msg' :: Possibly' P.Integer -> IO
result_msg' =
  \pA0 ->
  case pA0 of
    P.Nothing -> print'("Target Not Found")
    P.Just ind -> print'("Found! The index is: " !+ ind)

try' :: P.Integer -> IO
try' =
  \i -> print'("Trying " !+ P.show(i)) !>> result_msg'(bin_search''(list, i))

try_list' :: ListOf's P.Integer -> IO
try_list' =
  \pA0 ->
  case pA0 of
    [] -> do_nothing
    i : is -> try'(i) !>> try_list'(is)

main :: IO
main =
  try_list'([(0 :: P.Integer), (1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer), (4 :: P.Integer), (5 :: P.Integer), (6 :: P.Integer), (7 :: P.Integer), (8 :: P.Integer)])