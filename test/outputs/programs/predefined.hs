{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

forty_two_over' :: Z -> Result'OrError' R P.String
forty_two_over' =
  \pA0 ->
  case pA0 of
    0 -> P.Left("Dividing by 0! BOOM")
    i -> P.Right(((42 :: P.Integer) !/ i))

forty_two_over'plus_forty_two_over' :: (Z, Z) -> Result'OrError' R P.String
forty_two_over'plus_forty_two_over' =
  \(x, y) ->
  forty_two_over'(x) !>>= \z ->
  forty_two_over'(y) !>>= \w ->
  wrap'(z !+ w)

test1 :: Result'OrError' R P.String
test1 =
  forty_two_over'plus_forty_two_over'((1 :: P.Integer), (0 :: P.Integer))

test2 :: Result'OrError' R P.String
test2 =
  forty_two_over'plus_forty_two_over'((0 :: P.Integer), (1 :: P.Integer))

test3 :: Result'OrError' R P.String
test3 =
  forty_two_over'plus_forty_two_over'((2 :: P.Integer), (2 :: P.Integer))

print_result' :: Result'OrError' R P.String -> IO
print_result' =
  \pA0 ->
  case pA0 of
    P.Left err -> print'(err)
    P.Right res -> print'(res)

main :: A'FromIO EmptyVal
main =
  print'(P.pi) !>> print'(id'(P.pi)) !>> print'(sqrt_of'(P.pi)) !>> print'(abs_val_of'((-1 :: P.Integer))) !>> 
  print'(max_of'and'((1 :: P.Integer), (2 :: P.Integer))) !>> print'(min_of'and'((1 :: P.Integer), (2 :: P.Integer))) !>> 
  print'(sin'(P.pi)) !>> print'(cos'(P.pi)) !>> print'(tan'(P.pi)) !>> 
  print'(asin'((1.0 :: P.Double))) !>> print'(acos'((1.0 :: P.Double))) !>> print'(atan'((1.0 :: P.Double))) !>> 
  print'(gcd_of'and'((42 :: P.Integer), (18 :: P.Integer))) !>> print'(lcm_of'and'((4 :: P.Integer), (6 :: P.Integer))) !>> 
  print'(a'is_odd((2 :: P.Integer))) !>> print'(a'is_even((2 :: P.Integer))) !>> 
  print'(truncate'((1.1 :: P.Double))) !>> print'(round'((1.1 :: P.Double))) !>> 
  print'(floor'((1.1 :: P.Double))) !>> print'(ceiling'((1.1 :: P.Double))) !>> 
  print'(exp'((1.0 :: P.Double))) !>> print'(ln'(exp'((1.0 :: P.Double)))) !>> 
  print'(log_of'base'((1024.0 :: P.Double), (2.0 :: P.Double))) !>> 
  print'(take'from'((2 :: P.Integer), [(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer)])) !>> 
  print'(ignore'from'((2 :: P.Integer), [(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer)])) !>> 
  print'(filter'with'([(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer)], (\pA0 -> pA0 !== (2 :: P.Integer)))) !>> 
  print'(zip'with'([(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer)], [(4 :: P.Integer), (5 :: P.Integer), (6 :: P.Integer)])) !>> 
  print'(unzip'([ft2((1 :: P.Integer), (4 :: P.Integer)), ft2((2 :: P.Integer), (5 :: P.Integer)), ft2((3 :: P.Integer), (6 :: P.Integer))])) !>> 
  print'(apply'to_all_in_zipped''((\(pA0, pA1) -> pA0 !+ pA1), [(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer)], [(6 :: P.Integer), (5 :: P.Integer), (4 :: P.Integer)])) !>> 
  print'(split'at'([(1 :: P.Integer), (2 :: P.Integer), (3 :: P.Integer), (4 :: P.Integer), (5 :: P.Integer), (6 :: P.Integer)], (3 :: P.Integer))) !>> 
  (get_char !>>= (\pA0 -> print'(pA0))) !>> 
  (get_input !>>= (\pA0 -> print'(pA0))) !>> 
  (read_file'("../../hello_world.lc") !>>= (\pA0 -> write'to_file'(pA0, "out.txt"))) !>> 
  print_string'("Hello ") !>> print'("World!") !>> 
  print_result'(test1) !>> print_result'(test2) !>> print_result'(test3) !>> 
  P.undefined