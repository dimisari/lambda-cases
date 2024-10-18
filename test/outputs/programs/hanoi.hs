{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

hanoi_with'rings_from'to'through' :: (P.Integer, P.String, P.String, P.String) -> IO
hanoi_with'rings_from'to'through' =
  \(pA0, source, target, auxiliary) ->
  case pA0 of
    0 -> do_nothing
    n ->
      let
      move_from'to' :: (P.String, P.String) -> IO
      move_from'to' =
        (\pA0 -> print'(pA0)) <. ((\(pA0, pA1) -> "Move from " !+ pA0 !+ " to " !+ pA1))
      in
      hanoi_with'rings_from'to'through'(n !- (1 :: P.Integer), source, auxiliary, target) !>> 
      move_from'to'(source, target) !>> 
      hanoi_with'rings_from'to'through'(n !- (1 :: P.Integer), auxiliary, target, source)

main :: IO
main =
  print'("Number of rings?") !>> 
  get_line !>>= (\pA0 -> from_string'(pA0)) .> 
  (\pA0 -> hanoi_with'rings_from'to'through'(pA0, "left", "right", "middle"))