{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

f'' :: A'To_The'Is' b0 P.Integer P.Double => (b0, b0) -> P.Double
f'' =
  (\(pA0, pA1) -> pA0 !^ (2 :: P.Integer) !+ pA1 !^ (2 :: P.Integer))

main :: IO
main =
  print'("2 to the 3 is: " !+ (2 :: P.Integer) !^ (3 :: P.Integer)) !>> 
  print'("6 times 7 is: " !+ (6 :: P.Integer) !* (7 :: P.Integer)) !>> 
  print'("42 divided by 6 is: " !+ (42 :: P.Integer) !/ (6 :: P.Integer)) !>> 
  print'("42 minus 378 is: " !+ ((42 :: P.Integer) !+ (378 :: P.Integer))) !>> 
  print'("42 minus 25 is: " !+ ((42 :: P.Integer) !- (25 :: P.Integer))) !>> 
  print'("2 is equal to 3 is: " !+ ((2 :: P.Integer) !== (3 :: P.Integer))) !>> 
  print'("2 is not equal to 3 is: " !+ ((2 :: P.Integer) !!= (3 :: P.Integer))) !>> 
  print'("2 is greater than 3 is: " !+ ((2 :: P.Integer) !> (3 :: P.Integer))) !>> 
  print'("2 is less than 3 is: " !+ ((2 :: P.Integer) !< (3 :: P.Integer))) !>> 
  print'("2 is greater than or equal to 3 is: " !+ ((2 :: P.Integer) !>= (3 :: P.Integer))) !>> 
  print'("2 is less than or equal 3 is: " !+ ((2 :: P.Integer) !<= (3 :: P.Integer))) !>> 
  print'("true and false is " !+ (P.True !& P.False)) !>> 
  print'("true or false is " !+ (P.True !| P.False)) !>> 
  print'("2 to the 3.14 is: " !+ (2 :: P.Integer) !^ (3.14 :: P.Double)) !>> 
  print'("3.14 to the 2 is: " !+ (3.14 :: P.Double) !^ (2 :: P.Integer)) !>> 
  print'("1.61 to the 3.14 is: " !+ (1.61 :: P.Double) !^ (3.14 :: P.Double)) !>> 
  print'("2 times 3.14 is: " !+ (2 :: P.Integer) !* (3.14 :: P.Double)) !>> 
  print'("3.14 times 2 is: " !+ (3.14 :: P.Double) !* (2 :: P.Integer)) !>> 
  print'("5 times 'a' is: " !+ (5 :: P.Integer) !* 'a') !>> 
  print'("5 times \"hi\" is: " !+ (5 :: P.Integer) !* "hi") !>> 
  print'("2 over 3.14 is: " !+ (2 :: P.Integer) !/ (3.14 :: P.Double)) !>> 
  print'("3.14 over 2 is: " !+ (3.14 :: P.Double) !/ (2 :: P.Integer)) !>> 
  print'("2 plus 3.14 is: " !+ ((2 :: P.Integer) !+ (3.14 :: P.Double))) !>> 
  print'("3.14 plus 2 is: " !+ ((3.14 :: P.Double) !+ (2 :: P.Integer))) !>> 
  print'("'a' plus 'b' is: " !+ ('a' !+ 'b')) !>> 
  print'("'w' plus \"ord\" is: " !+ ('w' !+ "ord")) !>> 
  print'("\"Hello \" plus \"World!\" is: " !+ ("Hello " !+ "World!")) !>> 
  print'("\"1,2,3\" minus ',' is: " !+ ("1,2,3" !- ',')) !>> 
  print'("\"Hello!! World!!\" minus \"!!\" is: " !+ ("Hello!! World!!" !- "!!")) !>> 
  print'("f(1.0, 3.14) = " !+ f''((1.0 :: P.Double), (3.14 :: P.Double))) !>> 
  do_nothing