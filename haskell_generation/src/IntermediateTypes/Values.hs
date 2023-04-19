module IntermediateTypes.Values where

import ParsingTypes.OperatorValues (BaseValue, AddSubTerm)

-- All: Application, ApplicationTree

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

data Addition = 
  ExprPlusTerm AddSubOrTerm AddSubTerm

data Subtraction' = 
  ExprMinusTerm AddSubOrTerm AddSubTerm

data AddSubOrTerm =
  Addition Addition | Subtraction' Subtraction' | Term AddSubTerm
