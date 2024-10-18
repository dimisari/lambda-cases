{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

data FibsAndIndex =
  FibsAndIndex' { prev :: P.Integer, cur :: P.Integer, cur_index :: P.Integer }

instance FromTuple3 P.Integer P.Integer P.Integer FibsAndIndex where
  ft3 = \(x1, x2, x3) -> FibsAndIndex' x1 x2 x3

c0prev :: P.Integer -> FibsAndIndex -> FibsAndIndex
c0cur :: P.Integer -> FibsAndIndex -> FibsAndIndex
c0cur_index :: P.Integer -> FibsAndIndex -> FibsAndIndex
c0prev = \new x -> x { prev = new }
c0cur = \new x -> x { cur = new }
c0cur_index = \new x -> x { cur_index = new }

print'fib_nums :: P.Integer -> IO
print'fib_nums =
  \pA0 ->
  case pA0 of
    0 -> do_nothing
    1 -> print_fib''((1 :: P.Integer), (0 :: P.Integer))
    n ->
      let
      print'fibs_with_init' :: (P.Integer, FibsAndIndex) -> IO
      print'fibs_with_init' =
        \(pA0, fai) ->
        case pA0 of
          0 -> do_nothing
          n -> advance'and_print(fai) !>>= (\pA0 -> print'fibs_with_init'(n !- (1 :: P.Integer), pA0))
      in
      print_fib''((1 :: P.Integer), (0 :: P.Integer)) !>> print_fib''((2 :: P.Integer), (1 :: P.Integer)) !>> print'fibs_with_init'(n !- (2 :: P.Integer), ft3((0 :: P.Integer), (1 :: P.Integer), (2 :: P.Integer)))

advance'and_print :: FibsAndIndex -> A'FromIO FibsAndIndex
advance'and_print =
  \fai ->
  let
  next :: FibsAndIndex
  next =
    ft3(cur(fai), prev(fai) !+ cur(fai), cur_index(fai) !+ (1 :: P.Integer))
  in
  print_fib''(cur_index(next), cur(next)) !>> a'from_io(next)

print_fib'' :: (P.Integer, P.Integer) -> IO
print_fib'' =
  \(index, number) -> print'("Fib num " !+ index !+ " = " !+ number)

main :: IO
main =
  print'("How many fibonacci numbers?") !>> 
  get_line !>>= (\pA0 -> from_string'(pA0)) .> (\pA0 -> print'fib_nums(pA0))