module CodeGenerators.OperatorValues where

import Data.List (intercalate)
import Control.Monad (zipWithM, (>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers 

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

-- Generate:
-- ParenExpr, BaseValue2, FuncAppChain, ApplicationTree, Application, MultExpr,
-- AddSubExpr, AddSubOrMExpr, Addition, Subtraction, EqualityExpr,
-- InputOpExpr, OpExpr

instance Generate ParenExpr where
  generate = \expr val_t ->
    paren_expr_inside_g expr val_t >>= \expr_hs ->
    return $ "(" ++ expr_hs ++ ")"

instance Generate BaseValue2 where
  generate = \case
    ParenExpr2 paren_expr -> generate paren_expr
    Literal2 literal -> generate literal
    ValueName2 value_name -> generate value_name

instance Generate FuncAppChain where
  generate = func_app_chain_to_app_tree .> generate

instance Generate ApplicationTree where
  generate = \case 
    BaseVal2Leaf base_val2 -> generate base_val2
    Application application -> generate application

instance Generate Application where
  generate = \application val_type ->
    generate_infer application >>= \(application_hs, application_t) ->
    equiv_types val_type application_t >>= \case 
      True -> return application_hs
      False -> throwE $ type_check_err (show application) val_type application_t

instance Generate MultExpr where
  generate = \(Factors mul_f1 mul_fs) val_type -> 
    mapM (flip generate val_type) (mul_f1 : mul_fs) >>= intercalate " * " .> return

instance Generate AddSubExpr where
  generate = add_sub_e_to_add_sub_or_me .> generate

instance Generate AddSubOrMExpr where
  generate = \case
    Addition addition -> generate addition
    Subtraction subtraction -> generate subtraction
    MultExpr mult_expr -> generate mult_expr

instance Generate Addition where
  generate = \(ExprPlusMExpr expr mult_expr) -> add_sub_g expr " + " mult_expr

instance Generate Subtraction where
  generate = \(ExprMinusMExpr expr mult_expr) -> add_sub_g expr " - " mult_expr

instance Generate EqualityExpr where
  generate = \(EqExpr term1 maybe_term2) -> case maybe_term2 of
    Just term2 -> \case 
      TypeApp (ConsAndTIns (TN "Bool") []) ->
        generate term1 int >>= \term1_hs ->
        generate term2 int >>= \term2_hs ->
        return $ term1_hs ++ " == " ++ term2_hs
      val_t -> throwE $ equality_not_bool_err val_t
    Nothing -> generate term1

instance Generate InputOpExpr where
  generate = \(InputEqExpr input eq_expr) ->
    input_g input >=> \(input_hs, out_t, inserted) ->
    generate eq_expr out_t >>= \op_expr_hs ->
    mapM_ value_map_remove inserted >>
    return (input_hs ++ op_expr_hs)

instance Generate OpExpr where 
  generate = \case
    InputOpExpr input_op_expr -> generate input_op_expr
    EqualityExpr eq_expr -> generate eq_expr

-- GenerateInfer:
-- ParenExpr, BaseValue2, FuncAppChain, ApplicationTree, Application, MultExpr,
-- AddSubExpr, AddSubOrMExpr, Addition, Subtraction, EqualityExpr,
-- InputOpExpr, OpExpr

instance GenerateInfer ParenExpr where
  generate_infer = undefined

instance GenerateInfer BaseValue2 where
  generate_infer = \case
    ParenExpr2 paren_expr -> generate_infer paren_expr
    Literal2 literal -> generate_infer literal
    ValueName2 value_name -> generate_infer value_name

instance GenerateInfer FuncAppChain where
  generate_infer = func_app_chain_to_app_tree .> generate_infer

instance GenerateInfer ApplicationTree where
  generate_infer = \case 
    BaseVal2Leaf base_val2 -> generate_infer base_val2
    Application application -> generate_infer application

instance GenerateInfer Application where
  generate_infer = \application@(AppTrees tree1 tree2) ->
    generate_infer tree1 >>= \(tree1_hs, tree1_t) -> case tree1_t of 
      FuncType func_type ->
        catchE
          (normal_app_type_inf_g tree1_hs func_type tree2)
          (application_handler application func_type)
      _ -> throwE $ cant_apply_non_func_err2 tree1 tree2 tree1_t

instance GenerateInfer MultExpr where
  generate_infer = \(Factors mul_f1 mul_fs) ->
    generate_infer mul_f1 >>= \(hs1, t1) ->
    case mul_fs of
      [] -> return (hs1, t1)
      _ -> undefined

instance GenerateInfer AddSubExpr where
  generate_infer = add_sub_e_to_add_sub_or_me .> generate_infer

instance GenerateInfer AddSubOrMExpr where
  generate_infer = \case
    Addition addition -> generate_infer addition
    Subtraction subtraction -> generate_infer subtraction
    MultExpr mult_expr -> generate_infer mult_expr

instance GenerateInfer Addition where
  generate_infer = undefined

instance GenerateInfer Subtraction where
  generate_infer = undefined

instance GenerateInfer EqualityExpr where
  generate_infer = \(EqExpr term1 maybe_term2) -> case maybe_term2 of
    Just term2 -> undefined
    Nothing -> generate_infer term1

instance GenerateInfer InputOpExpr where
  generate_infer = undefined
   
instance GenerateInfer OpExpr where 
  generate_infer = \case
    InputOpExpr input_op_expr -> generate_infer input_op_expr
    EqualityExpr eq_expr -> generate_infer eq_expr

-- Tuple:
-- tuple_inside_g, tuple_t_name_g, tuple_tuple_t_g, correct_length_tuple_tt_g,
-- tuple_prod_t_g

tuple_inside_g = ( \exprs -> \case
  TypeApp (ConsAndTIns t_name _) -> tuple_t_name_g exprs t_name
  ProdType prod_type -> tuple_prod_t_g exprs prod_type
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
  zipWithM generate exprs types >>= \exprs_hs -> 
  return $ "C" ++ show t_name ++ concatMap ((" (" ++) .> (++ ")")) exprs_hs
  ) :: [ OpExpr ] -> [ ValType ] -> TypeName -> Stateful Haskell

tuple_prod_t_g = ( \exprs prod_type@(ProdTypes types) ->
  case length exprs == length types of
    False -> throwE $ tuple_prod_t_lengths_err exprs $ ProdType prod_type
    True -> 
      zipWithM generate exprs types >>= \exprs_hs ->
      return $ intercalate ", " exprs_hs
  ) :: [ OpExpr ] -> ProdType -> Stateful Haskell

-- ParenExpr: paren_expr_type_inf_g

paren_expr_inside_g = ( \(ParenExprs expr1 exprs) val_t -> case exprs of
  [] -> generate expr1 val_t
  _ : _ -> tuple_inside_g (expr1 : exprs) val_t
  ) :: ParenExpr -> ValType -> Stateful Haskell

-- Application: normal_app_type_inf_g

normal_app_type_inf_g = (
  \tree1_hs func_type@(InAndOutTs input_t output_t) tree2 -> 
  generate tree2 input_t >>= \tree2_hs ->
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
    BaseVal2Leaf (ParenExpr2 (WithPos _ paren_expr@(ParenExprs _ (_:_)))) ->
      app_handler_tuple tree1 paren_expr input_t err
    _ -> gen_app_hand app input_t err
  ) :: Application -> FuncType -> Error -> Stateful (Haskell, ValType)

-- app_handler_tuple, app_hand_tuple_correct

app_handler_tuple = ( \tree1 paren_expr@(ParenExprs expr1 _) input_t err -> 
  generate_infer expr1 >>= \(_, op_expr_t) ->
    equiv_types input_t op_expr_t >>= \case 
      True -> app_hand_tuple_correct tree1 paren_expr
      _ -> throwE err
  ) :: ApplicationTree -> ParenExpr -> ValType -> Error ->
       Stateful (Haskell, ValType)

app_hand_tuple_correct = (
  \tree1 paren_expr@(ParenExprs expr1 (expr2:exprs)) -> 
  let
  exprs_to_tree = ( \expr1 exprs ->
    BaseVal2Leaf $ ParenExpr2 $ add_dummy_pos $ ParenExprs expr1 exprs
    ) :: OpExpr -> [ OpExpr ] -> ApplicationTree
  tree1' = Application $ AppTrees tree1 $ exprs_to_tree expr1 []
  tree2' = exprs_to_tree expr2 exprs
  in
  generate_infer $ AppTrees tree1' tree2'
  ) :: ApplicationTree -> ParenExpr -> Stateful (Haskell, ValType)

-- gen_app_hand

gen_app_hand = ( \app@(AppTrees tree1 tree2) input_t err ->
  generate_infer tree2 >>= \case
    (_,ProdType (ProdTypes (t1:_))) -> equiv_types input_t t1 >>= \case 
      True -> gen_app_hand_correct app
      _ -> throwE err
    _ -> throwE err
  ) :: Application -> ValType -> Error -> Stateful (Haskell, ValType)

gen_app_hand_correct = ( \(AppTrees tree1 tree2) ->
  let
  tree2_1st = Application $ AppTrees get_1st_tree tree2
    :: ApplicationTree
  tree1' = Application $ AppTrees tree1 tree2_1st
    :: ApplicationTree

  tree2_rest = Application $ AppTrees get_all_but_1st_tree tree2
    :: ApplicationTree
  in
  generate_infer $ AppTrees tree1' tree2_rest
  ) :: Application -> Stateful (Haskell, ValType)

get_1st_tree =
  BaseVal2Leaf $ ValueName2 $ add_dummy_pos $ VN "get_1st"
  :: ApplicationTree

get_all_but_1st_tree =
  BaseVal2Leaf $ ValueName2 $ add_dummy_pos $ VN "get_all_but_1st"
  :: ApplicationTree

-- AddSubExpr

add_sub_g = ( \expr op_hs mult_expr val_type ->
  generate expr val_type >>= \expr_hs ->
  generate mult_expr val_type >>= \term_hs ->
  return $ expr_hs ++ op_hs ++ term_hs
  ) :: AddSubOrMExpr -> String -> Pos MultExpr -> ValType -> Stateful Haskell
