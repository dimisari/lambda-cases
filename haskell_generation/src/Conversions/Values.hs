module Conversions.Values where

import Helpers

import Parsing.Types.LowLevel
import Parsing.Types.OperatorValues

import IntermediateTypes.Values

-- All:
-- FuncAppChain to ApplicationTree, MathApplication to ApplicationTree

-- FuncAppChain to ApplicationTree:
-- func_app_chain_to_app_tree, func_app_chain_to_app_tree_help, combine

func_app_chain_to_app_tree = ( \(ValuesAndDirections base_val app_dir_base_vals) ->
  func_app_chain_to_app_tree_help base_val (reverse app_dir_base_vals)
  ) :: FuncAppChain -> ApplicationTree

func_app_chain_to_app_tree_help = ( \base_val1 -> \case
  [] -> base_val_to_app_tree base_val1
  (app_dir, base_val) : app_dir_base_vals ->
    combine
      (func_app_chain_to_app_tree_help base_val1 app_dir_base_vals)
      app_dir
      (base_val_to_app_tree base_val)
  ) :: BaseValue -> [ (ApplicationDirection, BaseValue) ] -> ApplicationTree

combine = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application $ AppTrees at1 at2
  RightApplication -> Application $ AppTrees at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

-- BaseValue to ApplicationTree:

base_val_to_app_tree = ( \case
  ParenExpr paren_expr -> BaseVal2Leaf $ ParenExpr2 paren_expr
  Literal lit -> BaseVal2Leaf $ Literal2 lit
  MathApp math_app -> math_app_to_app_tree $ remove_pos math_app
  ) :: BaseValue -> ApplicationTree

-- MathApp to ApplicationTree:

math_app_to_app_tree = ( \(NameAndParenExpr2 val_name maybe_paren_expr) ->
  let
  val_name_tree = BaseVal2Leaf $ ValueName2 val_name
    :: ApplicationTree
  in
  case maybe_paren_expr of
    Just paren_expr -> 
      Application $ AppTrees
        val_name_tree
        (BaseVal2Leaf $ ParenExpr2 paren_expr)
    Nothing -> 
      val_name_tree
  ) :: MathApp -> ApplicationTree

-- AddSubExpr to AddSubOrMExpr:

add_sub_e_to_add_sub_or_me = ( \(FirstAndOpTermPairs mult_expr1 op_term_pairs) ->
  add_sub_help_fun mult_expr1 op_term_pairs
  ) :: AddSubExpr -> AddSubOrMExpr

add_sub_help_fun = ( \mult_expr1 -> \case
  [] -> MultExpr mult_expr1
  (op, term) : pairs -> case op of
    Plus -> Addition $ ExprPlusMExpr (add_sub_help_fun mult_expr1 pairs) term
    Minus -> Subtraction $ ExprMinusMExpr (add_sub_help_fun mult_expr1 pairs) term
  ) :: Pos MultExpr -> [ (PlusOrMinus, Pos MultExpr) ] -> AddSubOrMExpr

-- Input to Input2:

input_to_input2 = ( \case
  OneAbstraction abstraction -> Input2 [ abstraction ]
  ManyAbstractions (Abstractions abs1 abs2 abs) ->
    Input2 $ abs1 : abs2 : abs
  ) :: Input -> Input2
