module CodeGenerators.OperatorValues where

import Data.List (intercalate)
import Control.Monad (zipWithM, (>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel 
import ParsingTypes.Types (TypeName(..))
import ParsingTypes.OperatorValues

import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions (TTField(..), TypeInfo(..))
import IntermediateTypes.Values

import Conversions.Values

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (equiv_types)

import CodeGenerators.LowLevel

-- All:
-- ParenExpr, MathApp, BaseValue,
-- FuncAppChain, MultExpr, AddSubExpr,
-- EqualityExpr, InputOpExpr, OpExpr

-- Tuple:
-- tuple_inside_g, tuple_t_name_g, tuple_tuple_t_g, correct_length_tuple_tt_g,
-- tuple_prod_t_g

tuple_inside_g = ( \exprs -> \case
  TypeApp (ConsAndTIns t_name _) -> tuple_t_name_g exprs t_name
  ProdType types -> tuple_prod_t_g exprs types
  val_t -> throwE $ wrong_type_for_tuple_err exprs val_t
  ) :: [ OpExpr ] -> ValType -> Stateful Haskell

tuple_t_name_g = ( \exprs t_name -> type_map_get t_name >>= \case
  OrType _ _ -> throwE $ or_type_for_tuple_err exprs
  TupleType _ fields -> tuple_tuple_t_g exprs fields t_name
  IntType -> throwE $ int_for_tuple_err exprs
  CharType -> throwE $ char_for_tuple_err exprs
  ) :: [ OpExpr ] -> TypeName -> Stateful Haskell

tuple_tuple_t_g = ( \exprs fields t_name -> case length exprs == length fields of 
  False -> throwE tuple_field_length_err
  True -> correct_length_tuple_tt_g exprs (map get_type fields) t_name
  ) :: [ OpExpr ] -> [ TTField ] -> TypeName -> Stateful Haskell

correct_length_tuple_tt_g = ( \exprs types t_name ->
  get_ind_lev >>= \ind_lev ->
  zipWithM operator_expression_g exprs types >>= \exprs_hs -> 
  return $ "C" ++ show t_name ++ concatMap ((" (" ++) .> (++ ")")) exprs_hs
  ) :: [ OpExpr ] -> [ ValType ] -> TypeName -> Stateful Haskell

tuple_prod_t_g = ( \exprs types -> case length exprs == length types of
  False -> throwE $ tuple_prod_t_lengths_err exprs $ ProdType types
  True -> 
    zipWithM operator_expression_g exprs types >>= \exprs_hs ->
    return $ intercalate ", " exprs_hs
  ) :: [ OpExpr ] -> [ ValType ] -> Stateful Haskell

-- ParenExpr: paren_expr_g, paren_expr_type_inf_g

paren_expr_g = ( \expr val_t ->
  paren_expr_inside_g expr val_t >>= \expr_hs ->
  return $ "(" ++ expr_hs ++ ")"
  ) :: ParenExpr -> ValType -> Stateful Haskell

paren_expr_inside_g = ( \(ParenExprs expr1 exprs) val_t -> case exprs of
  [] -> operator_expression_g expr1 val_t
  _ : _ -> tuple_inside_g (expr1 : exprs) val_t
  ) :: ParenExpr -> ValType -> Stateful Haskell

paren_expr_type_inf_g = ( \expr ->
  undefined
  ) :: ParenExpr -> Stateful (Haskell, ValType)

-- PosParenExpr: pos_paren_expr_g, paren_expr_type_inf_g

pos_paren_expr_g = ( \(PPE pos paren_expr) val_t ->
  catchE (paren_expr_g paren_expr val_t) (add_pos_to_err pos)
  ) :: PosParenExpr -> ValType -> Stateful Haskell

-- MathApp: math_app_g, math_app_type_inf_g

math_app_g =
  math_a_to_a_tree .> application_tree_g
  :: MathApp -> ValType -> Stateful Haskell

math_app_type_inf_g =  
  math_a_to_a_tree .> app_tree_type_inf_g
  :: MathApp -> Stateful (Haskell, ValType)

-- PosMathApp: pos_math_app_g

pos_math_app_g = ( \(PMApp pos math_app) val_t -> 
  catchE (math_app_g math_app val_t) (add_pos_to_err pos)
  ) :: PosMathApp -> ValType -> Stateful Haskell

-- BaseValue: base_value_g, base_value_type_inf_g

base_value_g = ( \case
  ParenExpr paren_expr -> pos_paren_expr_g paren_expr
  Literal literal -> pos_literal_g literal
  ValueName value_name -> pos_value_name_g value_name
  MathApp math_application -> pos_math_app_g math_application
  ) :: BaseValue -> ValType -> Stateful Haskell

base_value_type_inf_g = ( \case
  ParenExpr paren_expr -> paren_expr_type_inf_g $ ppe_to_pe paren_expr
  Literal literal -> literal_type_inf_g $ plit_to_lit literal
  ValueName value_name -> value_name_type_inf_g $ pvn_to_vn value_name
  MathApp math_application -> math_app_type_inf_g $ pmapp_to_mapp math_application
  ) :: BaseValue -> Stateful (Haskell, ValType)

-- FuncAppChain: func_app_chain_g, func_app_chain_type_inf_g,

func_app_chain_g = 
  func_app_chain_conv .> application_tree_g
  :: FuncAppChain -> ValType -> Stateful Haskell

func_app_chain_type_inf_g = 
  func_app_chain_conv .> app_tree_type_inf_g
  :: FuncAppChain -> Stateful (Haskell, ValType)

-- PosFuncAppChain: pos_func_app_chain_g

pos_func_app_chain_g = ( \(PFAC pos fac) val_type ->
  catchE (func_app_chain_g fac val_type) (add_pos_to_err pos)
  ) :: PosFuncAppChain -> ValType -> Stateful Haskell

-- ApplicationTree: application_tree_g, app_tree_type_inf_g

application_tree_g = ( \case 
  BaseValueLeaf base_value -> base_value_g base_value
  Application application -> application_g application
  ) :: ApplicationTree -> ValType -> Stateful Haskell

app_tree_type_inf_g = ( \case 
  Application application -> application_type_inf_g application
  BaseValueLeaf base_value -> base_value_type_inf_g base_value
  ) :: ApplicationTree -> Stateful (Haskell, ValType)

-- Application: application_g, application_type_inf_g, normal_app_type_inf_g

application_g = ( \application val_type ->
  application_type_inf_g application >>= \(application_hs, application_t) ->
  equiv_types val_type application_t >>= \case 
    True -> return application_hs
    False -> throwE $ type_check_err (show application) val_type application_t
  ) :: Application -> ValType -> Stateful Haskell

application_type_inf_g = ( \application@(AppTrees tree1 tree2) ->
  app_tree_type_inf_g tree1 >>= \(tree1_hs, tree1_t) -> case tree1_t of 
    FuncType func_type ->
      catchE
        (normal_app_type_inf_g tree1_hs func_type tree2)
        (application_handler application func_type)
    _ -> throwE $ cant_apply_non_func_err tree1 tree2 tree1_t
  ) :: Application -> Stateful (Haskell, ValType)

normal_app_type_inf_g = (
  \tree1_hs func_type@(InAndOutTs input_t output_t) tree2 -> 
  application_tree_g tree2 input_t >>= \tree2_hs ->
  let 
  tree2_hs' = case tree2 of 
    Application _ -> "(" ++ tree2_hs ++ ")"
    _ -> tree2_hs
    :: Haskell
  in
  return (tree1_hs ++ " " ++ tree2_hs', output_t) 
  ) :: Haskell -> FuncType -> ApplicationTree -> Stateful (Haskell, ValType)

-- Application (handlers): application_handler

application_handler = ( \app@(AppTrees tree1 tree2) (InAndOutTs input_t _) err ->
  case tree2 of 
    BaseValueLeaf (ParenExpr (PPE _ paren_expr@(ParenExprs _ (_:_)))) ->
      app_handler_tuple tree1 paren_expr input_t err
    _ -> general_app_handler app input_t err
  ) :: Application -> FuncType -> Error -> Stateful (Haskell, ValType)

-- app_handler_tuple, app_handler_tuple_correct

app_handler_tuple = ( \tree1 paren_expr@(ParenExprs expr1 _) input_t err -> 
  op_expr_type_inf_g expr1 >>= \(_, op_expr_t) ->
    equiv_types input_t op_expr_t >>= \case 
      True -> app_handler_tuple_correct tree1 paren_expr
      _ -> throwE err
  ) :: ApplicationTree -> ParenExpr -> ValType -> Error ->
       Stateful (Haskell, ValType)

app_handler_tuple_correct = ( \tree1 paren_expr@(ParenExprs expr1 (expr2:exprs)) -> 
  let
  exprs_to_tree = ( \expr1 exprs ->
    BaseValueLeaf $ ParenExpr $ PPE dummy_pos $ ParenExprs expr1 exprs
    ) :: OpExpr -> [ OpExpr ] -> ApplicationTree
  tree1' = Application $ AppTrees tree1 $ exprs_to_tree expr1 []
  tree2' = exprs_to_tree expr2 exprs
  in
  application_type_inf_g $ AppTrees tree1' tree2'
  ) :: ApplicationTree -> ParenExpr -> Stateful (Haskell, ValType)

-- general_app_handler, general_app_handler_correct

general_app_handler = ( \app@(AppTrees tree1 tree2) input_t err ->
  app_tree_type_inf_g tree2 >>= \case
    (_,ProdType (t1:_)) -> equiv_types input_t t1 >>= \case 
      True -> general_app_handler_correct app
      _ -> throwE err
    _ -> throwE err
  ) :: Application -> ValType -> Error -> Stateful (Haskell, ValType)

general_app_handler_correct = ( \(AppTrees tree1 tree2) ->
  let
  tree2_1st = Application $ AppTrees get_1st_tree tree2
    :: ApplicationTree
  tree1' = Application $ AppTrees tree1 tree2_1st
    :: ApplicationTree

  tree2_rest = Application $ AppTrees get_all_but_1st_tree tree2
    :: ApplicationTree
  in
  application_type_inf_g $ AppTrees tree1' tree2_rest
  ) :: Application -> Stateful (Haskell, ValType)

-- general_app_handler (helpers): get_1st_tree, get_all_but_1st_tree

get_1st_tree = BaseValueLeaf $ ValueName $ vn_to_pvn $ VN "get_1st"
  :: ApplicationTree

get_all_but_1st_tree = BaseValueLeaf $ ValueName $ vn_to_pvn $ VN "get_all_but_1st"
  :: ApplicationTree

-- MultExpr: mult_expr_g

mult_expr_g = ( \(Factors mul_f1 mul_fs) val_type -> 
  mapM (flip pos_func_app_chain_g val_type) (mul_f1 : mul_fs) >>=
  intercalate " * " .> return
  ) :: MultExpr -> ValType -> Stateful Haskell

mult_expr_type_inf_g = ( \(Factors mul_f1 mul_fs) ->
  func_app_chain_type_inf_g (pfac_to_fac mul_f1) >>= \(hs1, t1) ->
  case mul_fs of
    [] -> return (hs1, t1)
    _ -> undefined
  ) :: MultExpr -> Stateful (Haskell, ValType)

-- AddSubExpr:
-- add_sub_expr_g, add_sub_expr_type_inf_g, add_sub_or_term_g,
-- add_sub_expr_type_inf_g, addition_g, subtraction_g, add_sub_g

add_sub_expr_g = add_sub_expr_conv .> add_sub_or_mexpr_g
  :: AddSubExpr -> ValType -> Stateful Haskell

add_sub_expr_type_inf_g = add_sub_expr_conv .> add_sub_or_term_type_inf_g 
  :: AddSubExpr -> Stateful (Haskell, ValType)

add_sub_or_mexpr_g = ( \case
  Addition addition -> addition_g addition
  Subtraction subtraction -> subtraction_g subtraction
  MultExpr mult_expr -> mult_expr_g mult_expr
  ) :: AddSubOrMExpr -> ValType -> Stateful Haskell

add_sub_or_term_type_inf_g = ( \case
  Addition addition -> undefined
  Subtraction subtr -> undefined
  MultExpr mult_expr -> mult_expr_type_inf_g mult_expr
  ) :: AddSubOrMExpr -> Stateful (Haskell, ValType)

addition_g = ( \(ExprPlusMExpr expr mult_expr) ->
  add_sub_g expr " + " mult_expr
  ) :: Addition -> ValType -> Stateful Haskell

subtraction_g = ( \(ExprMinusMExpr expr mult_expr) ->
  add_sub_g expr " - " mult_expr
  ) :: Subtraction -> ValType -> Stateful Haskell

add_sub_g = ( \expr op mult_expr val_type ->
  add_sub_or_mexpr_g expr val_type >>= \expr_hs ->
  mult_expr_g mult_expr val_type >>= \term_hs ->
  return $ expr_hs ++ op ++ term_hs
  ) :: AddSubOrMExpr -> String -> MultExpr -> ValType -> Stateful Haskell

-- EqualityExpr: equality_expr_g

equality_expr_g = ( \(EqExpr term1 maybe_term2) -> case maybe_term2 of
  Just term2 -> \case 
    TypeApp (ConsAndTIns (TN "Bool") []) ->
      add_sub_expr_g term1 int >>= \term1_hs ->
      add_sub_expr_g term2 int >>= \term2_hs ->
      return $ term1_hs ++ " == " ++ term2_hs
    val_t -> throwE $ equality_not_bool_err val_t
  Nothing -> add_sub_expr_g term1
  ) :: EqualityExpr -> ValType -> Stateful Haskell

equality_expr_type_inf_g = ( \(EqExpr term1 maybe_term2) ->
  case maybe_term2 of
    Just term2 -> undefined
    Nothing -> add_sub_expr_type_inf_g term1
  ) :: EqualityExpr -> Stateful (Haskell, ValType)

-- InputOpExpr: input_operator_expression_g, input_op_expr_type_inf_g

input_operator_expression_g = ( \(InputEqExpr input eq_expr) ->
  input_g input >=> \(out_t, input_hs) ->
  equality_expr_g eq_expr out_t >>= \op_expr_hs ->
  input_val_map_remove input >>
  return (input_hs ++ op_expr_hs)
  ) :: InputOpExpr -> ValType -> Stateful Haskell

input_op_expr_type_inf_g = ( \(InputEqExpr input eq_expr) ->
  undefined
  ) :: InputOpExpr -> Stateful (Haskell, ValType)

-- OpExpr: operator_expression_g, input_op_expr_type_inf_g

operator_expression_g = ( \case
  InputOpExpr input_op_expr -> input_operator_expression_g input_op_expr
  EqualityExpr eq_expr -> equality_expr_g eq_expr
  ) :: OpExpr -> ValType -> Stateful Haskell

op_expr_type_inf_g = ( \case
  InputOpExpr input_op_expr -> input_op_expr_type_inf_g input_op_expr
  EqualityExpr eq_expr -> equality_expr_type_inf_g eq_expr
  ) :: OpExpr -> Stateful (Haskell, ValType)
