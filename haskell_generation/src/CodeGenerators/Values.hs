module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM, zipWithM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel (ValueName(..), Abstraction(..))
import ParsingTypes.Types (TypeName(..), ValueType(..))
import ParsingTypes.Values

import IntermediateTypes.Values
import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions 

import Conversions.Types
import Conversions.TypeDefinitions

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (equiv_types)
import GenerationHelpers.Helpers

import CodeGenerators.LowLevel
import CodeGenerators.OperatorValues

-- All:
-- LitOrValName, Case, Cases,
-- ValueNamesTypesAndExpressions, Values,
-- Where, CasesOrWhere, ValueExpression

-- LitOrValName: lit_or_val_name_g

-- or_type_vn_g, last_or_type_vn_g

data ValNameIsOrCase = 
  IsCase { other_cases :: [ ValueName ] } | IsNotCase

check_is_or_case = ( \val_name -> \case
  (_, []) -> IsNotCase
  (acc, c:cs) -> case c == val_name of
    True -> IsCase $ acc ++ cs
    False -> check_is_or_case val_name (c:acc, cs)
  ) :: ValueName -> ([ValueName], [ValueName]) -> ValNameIsOrCase

or_type_vn_g = ( \val_name val_type or_t_cs ->
  case check_is_or_case val_name ([], or_t_cs) of
    IsCase other_names ->
      maybe_value_g val_name val_type >>= \maybe_value_hs ->
      return (other_names, "C" ++ show val_name ++ maybe_value_hs)
    IsNotCase -> throwE $ not_or_type_case_err val_name val_type
  ) :: ValueName -> ValType -> [ ValueName ] -> Stateful ([ ValueName ], Haskell)

last_or_type_vn_g = ( \val_name or_t_cs val_type -> case elem val_name or_t_cs of
  True -> case or_t_cs == [ val_name ] of 
    True ->
      maybe_value_g val_name val_type >>= \maybe_value_hs ->
      return $ "C" ++ show val_name ++ maybe_value_hs
    False -> throwE $ cases_not_covered val_name or_t_cs
  False -> val_n_ins_and_ret_hs val_name val_type
  ) :: ValueName -> [ ValueName ] -> ValType -> Stateful Haskell

-- int LitOrValName

not_last_int_lovn_g = ( \case 
  ValueName val_name -> throwE $ "case of Int type can't be: " ++ show val_name
  Literal literal -> literal_g literal int
  ) :: LitOrValName -> Stateful Haskell

last_int_lovn_g = ( \case 
  ValueName val_name -> val_n_ins_and_ret_hs val_name int
  Literal literal -> throwE $ last_int_case_err literal
  ) :: LitOrValName -> Stateful Haskell

-- Case: abs_g 

not_last_or_type_case_g = (
  \(InAndOutTs in_t out_t) (or_t_cs, hs_so_far) (val_name, val_expr) ->
  get_ind_lev >>= \ind_lev ->
  or_type_vn_g val_name in_t or_t_cs >>= \(new_names, val_name_hs) ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return
    ( new_names
    , hs_so_far ++ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs ++ "\n"
    )
  ) :: FuncType -> ([ ValueName ], Haskell) -> (ValueName, ValueExpression) ->
       Stateful ([ ValueName ], Haskell)

last_or_type_case_g = ( \(val_name, val_expr) (InAndOutTs in_t out_t) or_t_cs ->
  get_ind_lev >>= \ind_lev ->
  last_or_type_vn_g val_name or_t_cs in_t >>= \val_name_hs ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs
  ) :: (ValueName, ValueExpression) -> FuncType -> [ ValueName ] ->
       Stateful Haskell

-- int case

not_last_int_case_g = int_case_g not_last_int_lovn_g
  :: Case -> ValType -> Stateful Haskell

last_int_case_g = int_case_g last_int_lovn_g
  :: Case -> ValType -> Stateful Haskell

int_case_g = ( \lovn_g (Case lit_or_val_name val_expr) out_t ->
  get_ind_lev >>= \ind_lev ->
  lovn_g lit_or_val_name >>= \int_lovn_hs ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ int_lovn_hs ++ " -> " ++ val_expr_hs
  ) :: (LitOrValName -> Stateful Haskell) -> Case -> ValType ->
       Stateful Haskell

-- DefaultCase: default_case_g

default_case_g = ( \(DefaultCase value_expression) output_t -> 
  get_ind_lev >>= \ind_lev ->
  value_expression_g value_expression output_t >>= \value_expression_hs ->
  return $ indent ind_lev ++ "_ -> " ++ value_expression_hs
  ) :: DefaultCase -> ValType -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g

cases_g = ( \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
  get_ind_lev >>= \ind_lev ->
  update_ind_lev (ind_lev + 1) >>
  cases_list_g (case1 : cases) maybe_def_case val_type >>= \cases_hs ->
  update_ind_lev ind_lev >>
  return (indent ind_lev ++ "\\case\n" ++ cases_hs) 
  ) :: Cases -> ValType -> Stateful Haskell

cases_list_g = ( \cases maybe_def_case val_type ->
  check_type val_type >>= \case
    IntInput out_t -> int_cases_list_g cases maybe_def_case out_t
    OrTInput or_type_names func_t ->
      check_or_type_cases cases >>= \pairs ->
      or_type_cases_g pairs maybe_def_case or_type_names func_t
  ) :: [ Case ] -> Maybe DefaultCase -> ValType -> Stateful Haskell

-- int cases

int_cases_list_g = ( \cases maybe_def_case out_t -> case maybe_def_case of
  Just default_case -> int_cases_list_with_default_g cases default_case out_t
  Nothing -> int_cases_list_without_default_g cases out_t
  ) :: [ Case ] -> Maybe DefaultCase -> ValType -> Stateful Haskell

int_cases_list_with_default_g = ( \cases default_case out_t ->
  mapM (flip not_last_int_case_g out_t) cases >>= \cases_hs ->
  default_case_g default_case out_t >>= \default_case_hs ->
  return $ intercalate "\n" $ cases_hs ++ [ default_case_hs ]
  ) :: [ Case ] -> DefaultCase -> ValType -> Stateful Haskell

int_cases_list_without_default_g = ( \cases out_t -> 
  mapM (flip not_last_int_case_g out_t) (init cases) >>= \cases_hs ->
  last_int_case_g (last cases) out_t >>= \last_case_hs ->
  return $ intercalate "\n" $ cases_hs ++ [ last_case_hs ]
  ) :: [ Case ] -> ValType -> Stateful Haskell

-- or type cases

or_type_cases_g = ( \cases maybe_def_case or_type_names func_t ->
  case maybe_def_case of
    Just default_case ->
      or_type_cases_with_default_g cases default_case or_type_names func_t
    Nothing -> 
      or_type_cases_without_default_g cases or_type_names func_t
  ) :: [ (ValueName, ValueExpression) ] -> Maybe DefaultCase -> [ ValueName ] ->
       FuncType -> Stateful Haskell

or_type_cases_with_default_g = (
  \cases default_case or_type_names func_t@(InAndOutTs _ out_t) ->
  foldM (not_last_or_type_case_g func_t) (or_type_names, "") cases >>=
    \(_, cases_hs) ->
  default_case_g default_case out_t >>= \default_case_hs ->
  return $ cases_hs ++ default_case_hs
  ) :: [ (ValueName, ValueExpression) ] -> DefaultCase -> [ ValueName ] ->
       FuncType -> Stateful Haskell

or_type_cases_without_default_g = ( \cases or_type_names func_t ->
  foldM (not_last_or_type_case_g func_t) (or_type_names, "") (init cases) >>=
    \(final_names, cases_hs) ->
  last_or_type_case_g (last cases) func_t final_names >>= \last_case_hs ->
  return $ cases_hs ++ last_case_hs
  ) :: [ (ValueName, ValueExpression) ] -> [ ValueName ] -> FuncType ->
       Stateful Haskell

-- 

check_or_type_cases = ( \cases ->
  map case_lovn cases ==> check_lovns >>= \vns ->
  return $ zip vns $ map case_res_expr cases
  ) :: [ Case ] -> Stateful [ (ValueName, ValueExpression) ]

check_lovns = ( \lovns ->
  check_all_vns lovns >>= \vns ->
  check_dup_vns vns >> return vns
  ) :: [ LitOrValName ] -> Stateful [ ValueName ]

check_all_vns = mapM check_vn
  :: [ LitOrValName ] -> Stateful [ ValueName ]

check_vn = ( \case
  ValueName vn -> return vn
  Literal lit -> throwE $ lit_in_or_type_case_err lit
  ) :: LitOrValName -> Stateful ValueName

check_dup_vns = ( \case
  [] -> return ()
  vn : vns -> case elem vn vns of
    True -> throwE $ "Duplicate or_type case: " ++ show vn
    False -> check_dup_vns vns
  ) :: [ ValueName ] -> Stateful ()

data CasesTypeInfo =
  IntInput ValType | OrTInput [ ValueName ] FuncType

check_type = ( \case
  FuncType func_type -> check_func_type func_type
  other_t -> throwE $ cases_expr_not_func_t_err other_t
  ) :: ValType -> Stateful CasesTypeInfo

check_func_type = ( \func_type@(InAndOutTs in_t out_t) -> case in_t of
  TypeApp (ConsAndTIns type_name []) ->
    type_map_get type_name >>= \case
      OrType _ or_cases -> return $ OrTInput (map get_c_name or_cases) func_type
      IntType -> return $ IntInput out_t
      _ -> undefined
  other_t -> throwE $ cases_expr_wrong_in_t_err other_t
  ) :: FuncType -> Stateful CasesTypeInfo

cases_type_inference_g = (
  undefined
  ) :: Cases -> Stateful (Haskell, ValType)

-- ValueNameTypeAndExpression: name_type_and_value_g

name_type_and_value_g = ( \(value_name, value_type, value_expr) -> 
  get_ind_lev >>= \ind_lev ->
  let 
  val_type = val_type_conv value_type
    :: ValType
  in
  value_expression_g value_expr val_type >>= \val_expr_hs ->
  return $
    "\n" ++ indent ind_lev ++ show value_name ++ " :: " ++ show val_type ++
    "\n" ++ indent ind_lev ++ show value_name ++ " = " ++ val_expr_hs ++ "\n"
  ) :: (ValueName, ValueType, ValueExpression) -> Stateful Haskell

-- Values:
-- values_g, values_to_list, list_of_values_g, insert_value_to_map

values_g =
  values_to_list .> list_of_values_g
  :: Values -> Stateful Haskell

values_to_list = ( \ntav_lists -> case ntav_lists of
  NamesTypesAndExpressions
    (val_name : val_names) (val_type : val_types) (val_expr : val_exprs) ->
    (val_name, val_type, val_expr) :
      values_to_list (NamesTypesAndExpressions val_names val_types val_exprs)
  NamesTypesAndExpressions [] [] [] -> []
  _ -> error $ "values_to_list: should be impossible"
  ) :: Values -> [ (ValueName, ValueType, ValueExpression) ]

list_of_values_g = 
  mapM name_type_and_value_g .> fmap concat
  :: [ (ValueName, ValueType, ValueExpression) ] -> Stateful Haskell

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueExpressionWhereValues val_expr values) val_type ->
  get_ind_lev >>= \ind_lev -> update_ind_lev (ind_lev + 1) >>
  insert_values_to_map values >>
  mapM values_g values >>= concat .> \values_hs ->
  value_expression_g val_expr val_type >>= \val_expr_hs ->
  remove_values_from_map values >>
  update_ind_lev ind_lev >>
  return
    ( "\n" ++
      indent (ind_lev + 1) ++ "let" ++
      indent (ind_lev + 1) ++ values_hs ++
      indent (ind_lev + 1) ++ "in" ++
      "\n" ++
      indent (ind_lev + 1) ++ val_expr_hs
    )
  ) :: Where -> ValType -> Stateful Haskell

where_type_inference_g = ( \(ValueExpressionWhereValues val_expr values) ->
  undefined
  ) :: Where -> Stateful (Haskell, ValType)

insert_values_to_map = 
  concatMap values_to_list .> mapM_ insert_value_to_map
  :: [ Values ] -> Stateful ()

remove_values_from_map = 
  concatMap values_to_list .> mapM_ remove_value_from_map
  :: [ Values ] -> Stateful ()

insert_value_to_map = ( \(value_name, value_type, value_expr) ->
  value_map_insert value_name $ val_type_conv value_type
  ) :: (ValueName, ValueType, ValueExpression) -> Stateful ()

remove_value_from_map = ( \(value_name, _, _) -> value_map_remove value_name)
  :: (ValueName, ValueType, ValueExpression) -> Stateful ()

-- CasesOrWhere: cases_or_where_g, cases_or_where_type_inference_g

cases_or_where_g = ( \case
  Cases cases -> cases_g cases
  Where where_ -> where_g where_
  ) :: CasesOrWhere -> ValType -> Stateful Haskell

cases_or_where_type_inference_g = ( \case
  Cases cases -> cases_type_inference_g cases
  Where where_ -> where_type_inference_g where_
  ) :: CasesOrWhere -> Stateful (Haskell, ValType)

-- InputCasesOrWhere: abstraction_cases_or_where_g

abstraction_cases_or_where_g = ( \(InputAndCasesOrWhere input cases_or_where) ->
  input_g input >=> \(output_type, input_hs) ->
  cases_or_where_g cases_or_where output_type >>= \cases_or_where_hs ->
  input_val_map_remove input >>
  return (input_hs ++ cases_or_where_hs)
  ) :: InputCasesOrWhere -> ValType -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValType)

-- ValueExpression: value_expression_g, value_expression_type_inference_g

value_expression_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cases_or_where_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_g cases_or_where 
  OpExpr expr -> operator_expression_g expr
  ) :: ValueExpression -> ValType -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  OpExpr expr -> op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValType)
