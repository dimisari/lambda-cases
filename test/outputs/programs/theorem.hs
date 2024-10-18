{-# language FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, UndecidableInstances, FlexibleContexts #-}
import qualified Prelude as P
import PredefImports.Predefined
import PredefImports.OpsInHaskell

class A'Has_Internal_Appl b0 where
  apply'inside1' :: (a1 -> a2, b0 a1) -> b0 a2

class A'Has_Wrapped_Internal_App b0 where
  apply_wrp'inside' :: (b0 (a1 -> a2), b0 a1) -> b0 a2



instance (A'Has_Wrapped_Internal_App b0, A'Has_A_Wrapper b0) => A'Has_Internal_Appl b0 where
  apply'inside1' = \(f, x) -> apply_wrp'inside'(wrap'(f), x)



instance (A'And'Can_Be_Equal b0 b1, A'Can_Be_Greater_Than' b0 b1) => A'Can_Be_Gr_Or_Eq_To' b0 b1 where
  a !>= b = a !== b !| a !> b

main :: IO
main =
  print'("hi")