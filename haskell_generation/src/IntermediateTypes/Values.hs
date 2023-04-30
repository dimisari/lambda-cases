module IntermediateTypes.Values where

import ParsingTypes.OperatorValues (BaseValue, MultExpr)

-- Application, ApplicationTree

data Application =
  AppTrees ApplicationTree ApplicationTree

instance Show Application where
  show = \(AppTrees tree1 tree2) -> show tree1 ++ "<==" ++ show tree2

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue

instance Show ApplicationTree where
  show = \case 
    Application app -> show app
    BaseValueLeaf base_val -> show base_val

-- Addition, Subtraction, AddSubOrMExpr

data Addition = 
  ExprPlusMExpr AddSubOrMExpr MultExpr

data Subtraction = 
  ExprMinusMExpr AddSubOrMExpr MultExpr

data AddSubOrMExpr =
  Addition Addition | Subtraction Subtraction | MultExpr MultExpr
