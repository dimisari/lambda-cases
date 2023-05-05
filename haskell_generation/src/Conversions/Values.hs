module Conversions.Values where

import ParsingTypes.OperatorValues
import IntermediateTypes.Values

-- All:
-- FuncAppChain to ApplicationTree, MathApplication to ApplicationTree

-- FuncAppChain to ApplicationTree:
-- func_app_chain_conv, func_app_chain_conv_help, combine

func_app_chain_conv = ( \(ValuesAndDirections base_val app_dir_base_vals) ->
  func_app_chain_conv_help base_val (reverse app_dir_base_vals)
  ) :: FuncAppChain -> ApplicationTree 

func_app_chain_conv_help = ( \base_val1 -> \case
  [] -> BaseValueLeaf base_val1
  (app_dir, base_val) : app_dir_base_vals ->
    combine
      (func_app_chain_conv_help base_val1 app_dir_base_vals)
      app_dir
      (BaseValueLeaf base_val)
  ) :: BaseValue -> [ (ApplicationDirection, BaseValue) ] -> ApplicationTree 

combine = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application $ AppTrees at1 at2
  RightApplication -> Application $ AppTrees at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

-- MathApp to ApplicationTree:

math_a_to_a_tree = ( \(NameAndParenExpr val_name paren_expr) ->
  Application $ AppTrees
    (BaseValueLeaf $ ValueName val_name)
    (BaseValueLeaf $ ParenExpr paren_expr)
  ) :: MathApp -> ApplicationTree 

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
