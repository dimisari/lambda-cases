module IntermediateTypes.Values where

import ParsingTypes.OperatorValues (BaseValue, MultExpr)

-- All: Application, ApplicationTree

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

data Addition = 
  ExprPlusMExpr AddSubOrMExpr MultExpr

data Subtraction = 
  ExprMinusMExpr AddSubOrMExpr MultExpr

data AddSubOrMExpr =
  Addition Addition | Subtraction Subtraction | MultExpr MultExpr
