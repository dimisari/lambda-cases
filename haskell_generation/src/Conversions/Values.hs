module Conversions.Values where

import ParsingTypes.OperatorValues
import IntermediateTypes.Values

-- All:
-- FuncAppChain to ApplicationTree, MathApplication to ApplicationTree

-- FuncAppChain to ApplicationTree:
-- func_app_chain_to_app_tree, fac_to_app_tree_help,
-- combine

func_app_chain_to_app_tree = (
  \(ValuesAndDirections base_val app_dir_base_vals) ->
  fac_to_app_tree_help base_val (reverse app_dir_base_vals)
  ) :: FuncAppChain -> ApplicationTree 

fac_to_app_tree_help = ( \base_val1 -> \case
  [] -> BaseValueLeaf base_val1
  (app_dir, base_val) : app_dir_base_vals ->
    combine
      (fac_to_app_tree_help base_val1 app_dir_base_vals)
      app_dir
      (BaseValueLeaf base_val)
  ) :: BaseValue -> [ (ApplicationDirection, BaseValue) ] -> ApplicationTree 

combine = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application $ ApplicationTrees at1 at2
  RightApplication -> Application $ ApplicationTrees at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

-- MathApplication to ApplicationTree:
-- math_app_to_app_tree, base_vals_to_app_tree, expr_to_base_value

math_app_to_app_tree = ( \(NameAndInputExprs value_name expr1 exprs) ->
  base_vals_to_app_tree $
    ValueName value_name : map expr_to_base_value (expr1 : exprs) 
  ) :: MathApplication -> ApplicationTree 

base_vals_to_app_tree = ( \case
  [] -> error "empty list in base_vals_to_app_tree"
  [ bv ] -> BaseValueLeaf bv
  bv : bvs ->
    Application $ ApplicationTrees (BaseValueLeaf bv) $ base_vals_to_app_tree bvs
  ) :: [ BaseValue ] -> ApplicationTree

expr_to_base_value = ( \expr -> case expr of
  PureOpExpr
    ( AddSubExpr
      (FirstAndOpTermPairs (Factors (ValuesAndDirections base_val []) []) [])
    ) -> base_val
  _ -> Parenthesis $ InnerExpression $ expr
  ) :: OperatorExpression -> BaseValue

-- AddSubExpr to AddSubOrMExpr:

add_sub_expr_conv = ( \(FirstAndOpTermPairs mult_expr1 op_term_pairs) ->
  add_sub_help_fun mult_expr1 op_term_pairs
  ) :: AddSubExpr -> AddSubOrMExpr

add_sub_help_fun = ( \mult_expr1 -> \case
  [] -> MultExpr mult_expr1
  (op, term) : pairs -> case op of
    Plus -> Addition $ ExprPlusMExpr (add_sub_help_fun mult_expr1 pairs) term
    Minus -> Subtraction $ ExprMinusMExpr (add_sub_help_fun mult_expr1 pairs) term
  ) :: MultExpr -> [ (PlusOrMinus, MultExpr) ] -> AddSubOrMExpr
