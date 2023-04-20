module Conversions.Values where

import ParsingTypes.OperatorValues
import IntermediateTypes.Values

-- All:
-- FuncAppChain to ApplicationTree, MathApplication to ApplicationTree

-- FuncAppChain to ApplicationTree:
-- func_app_chain_to_app_tree, func_app_chain_to_app_tree_help,
-- combine_with_reverse_direction

func_app_chain_to_app_tree = ( \(ValuesAndDirections bv_ad bv_ads bv_last) ->
  func_app_chain_to_app_tree_help bv_last (reverse $ bv_ad : bv_ads)
  ) :: FuncAppChain -> ApplicationTree 

func_app_chain_to_app_tree_help = ( \prev_bv -> \case
  [] -> BaseValueLeaf prev_bv
  (bv, ad) : bv_ads -> combine_with_reverse_direction
    (BaseValueLeaf prev_bv) ad (func_app_chain_to_app_tree_help bv bv_ads)
  ) :: BaseValue -> [ (BaseValue, ApplicationDirection) ] -> ApplicationTree 

combine_with_reverse_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application $ ApplicationTrees at2 at1
  RightApplication -> Application $ ApplicationTrees at1 at2
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
    (AddSubExpr
      (FirstAndOpTermPairs
        (MultFactor
          (BaseValue bv)
        ) []
      )
    ) -> bv
  _ -> Parenthesis $ InnerExpression $ expr
  ) :: OperatorExpression -> BaseValue

-- AddSubExpr to AddSubOrTerm:

add_sub_expr_conv = ( \(FirstAndOpTermPairs term1 op_term_pairs) ->
  add_sub_help_fun op_term_pairs term1
  ) :: AddSubExpr -> AddSubOrTerm

add_sub_help_fun = ( \case
  [] -> Term 
  (op, term) : pairs -> \term1 -> case op of
    Plus -> Addition $ ExprPlusTerm (add_sub_help_fun pairs term1) term
    Minus -> Subtraction' $ ExprMinusTerm (add_sub_help_fun pairs term1) term
  ) :: [ (PlusOrMinus, AddSubTerm) ] -> AddSubTerm -> AddSubOrTerm
