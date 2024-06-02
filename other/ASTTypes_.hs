module ASTTypes_ where

data Literal_ =
  Int_ Int | R_ Double | Ch_ Char | S_ String

newtype Identifier_ =
  Id_ String

data ParenExpr_ =
  LOE1_ LineOpExpr_ | LFE1_ LineFuncExpr_

newtype Tuple_ = T_ [LineExprOrUnder_]

data LineExprOrUnder_ =
  LE1_ LineExpr_ | Underscore1_

data LineExpr_ =
  BOAE1_ BasicOrAppExpr_ | LOE2_ LineOpExpr_ | LFE2_ LineFuncExpr_

data BasicOrAppExpr_ =
  BE3_ BasicExpr_ | PrFA1_ PreFuncApp_ | PoFA1_ PostFuncApp_

data BasicExpr_ =
  Lit1_ Literal_ | FA1 FuncApp | T1_ Tuple_ | L1_ List_ | SI1_ SpecialId

newtype BigTuple = BT (LineExprOrUnder, LineExprOrUnders, [LineExprOrUnders])


