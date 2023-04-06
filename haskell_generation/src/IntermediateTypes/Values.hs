module IntermediateTypes.Values where

import ParsingTypes.OperatorValues (BaseValue)

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show
