module SyntaxTreeGen.TypesAndClasses where

import Prelude qualified as P
import Control.Monad.State.Lazy qualified as CMSL

type Root = P.String
type Dot = P.String
type DotTuple = (Root, Dot)
type NumState = CMSL.State P.Int
type NumStateDotTuple = NumState DotTuple
type LabelCode = P.String
type NodeName = P.String

class ToDot a where
  to_dot :: a -> NumStateDotTuple

class AddNewRootCode a where
  add_new_root_code :: P.String -> a -> NumState Dot
