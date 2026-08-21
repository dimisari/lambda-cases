module SyntaxTreeGen.TypesAndClasses where

import Prelude qualified as P
import Control.Monad.State.Lazy qualified as CMSL

type Root = P.String
type Dot = P.String
type DotTuple = (Root, Dot)
type NumState = CMSL.State P.Int
type LabelCode = P.String
type NodeName = P.String

newtype StringTree = ST (Root, [StringTree])

class ToStringTree a where
  to_string_tree :: a -> StringTree

class ToStringTrees a where
  to_string_trees :: a -> [StringTree]

