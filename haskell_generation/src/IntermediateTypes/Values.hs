module IntermediateTypes.Values where

import ParsingTypes.OperatorValues (BaseValue, MultExpr)

-- Application, ApplicationTree

data Application =
  AppTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

-- Addition, Subtraction, AddSubOrMExpr

data Addition = 
  ExprPlusMExpr AddSubOrMExpr MultExpr

data Subtraction = 
  ExprMinusMExpr AddSubOrMExpr MultExpr

data AddSubOrMExpr =
  Addition Addition | Subtraction Subtraction | MultExpr MultExpr
