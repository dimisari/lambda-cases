module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel (ValueName(..), Abstraction(..))
import ParsingTypes.Types (TypeName(..), ValueType(..))
import ParsingTypes.Values

import IntermediateTypes.Types 
import IntermediateTypes.Values

import Conversions.Types
import Conversions.TypeDefinitions

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (types_are_equivalent)

import CodeGenerators.LowLevel
import CodeGenerators.OperatorValues

-- All:
-- LitOrValName, SpecificCase, Cases,
-- ValueNamesTypesAndExpressions, Values,
-- Where, CasesOrWhere, ValueExpression

-- LitOrValName: lit_or_val_name_g, lit_or_val_name_type_inference_g

not_last_lit_or_val_name_g = ( \case
  Literal literal -> literal_g literal
  ValueName value_name -> value_name_g value_name
  ) :: LitOrValName -> ValueType' -> Stateful Haskell

last_lit_or_val_name_g = ( \case
  Literal literal -> literal_g literal
  ValueName value_name -> val_name_ins_and_ret value_name
  ) :: LitOrValName -> ValueType' -> Stateful Haskell

lit_or_val_name_type_inference_g = ( 
  undefined
  ) :: LitOrValName -> Stateful (Haskell, ValueType')

-- SpecificCase: specific_case_g, abs_g, specific_case_type_inference_g

specific_case_g = ( \lit_or_val_name_g spec_case -> \case
  FunctionType' func_type ->
    spec_case_func_type_g lit_or_val_name_g spec_case func_type
  other_t -> throwE $
    "cases expression has type:" ++ show other_t ++
    "\ninstead of a funtion type\n"
  ) :: (LitOrValName -> ValueType' -> Stateful Haskell) -> SpecificCase ->
       ValueType' -> Stateful Haskell

spec_case_func_type_g = (
  \lit_or_val_name_g
    (SpecificCase lit_or_val_name value_expression)
    (InAndOutType' input_t output_t) ->
  get_indent_level >>= \ind_lev ->
  lit_or_val_name_g lit_or_val_name input_t >>= \lit_or_val_name_hs ->
  value_expression_g value_expression output_t >>= \value_expr_hs ->
  return $ indent ind_lev ++ lit_or_val_name_hs ++ " -> " ++ value_expr_hs
  ) :: (LitOrValName -> ValueType' -> Stateful Haskell) -> SpecificCase ->
       FunctionType' -> Stateful Haskell

not_last_case_g = ( specific_case_g not_last_lit_or_val_name_g
  ) :: SpecificCase -> ValueType' -> Stateful Haskell

last_case_g = ( specific_case_g last_lit_or_val_name_g
  ) :: SpecificCase -> ValueType' -> Stateful Haskell

specific_case_type_inference_g = ( \(SpecificCase lit_or_val_name val_expr) ->
  get_indent_level >>= \ind_lev ->
  lit_or_val_name_type_inference_g lit_or_val_name >>= \(lovn_g, lovn_t) ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, v_t) ->
  return
    ( indent ind_lev ++ lovn_g ++ " -> " ++ val_expr_hs
    , FunctionType' $ InAndOutType' lovn_t v_t )
  ) :: SpecificCase -> Stateful (Haskell, ValueType')

-- DefaultCase: specific_case_g, specific_case_type_inference_g

default_case_g = ( \(DefaultCase value_expression) -> \case
  FunctionType' (InAndOutType' input_t output_t) -> 
    get_indent_level >>= \ind_lev ->
    value_expression_g value_expression output_t >>= \value_expression_hs ->
    return $ indent ind_lev ++ "_ -> " ++ value_expression_hs
  _ -> undefined
  ) :: DefaultCase -> ValueType' -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g

cases_g = ( \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
  get_indent_level >>= \indent_level ->
  update_indent_level (indent_level + 1) >>
  cases_help_g (case1 : cases) maybe_def_case val_type >>= \cases_hs ->
  update_indent_level indent_level >>
  return ( indent indent_level ++ "\\case\n" ++ cases_hs) 
  ) :: Cases -> ValueType' -> Stateful Haskell

cases_help_g = ( \cases maybe_def_case val_type -> case maybe_def_case of
  Just default_case ->
    mapM (flip not_last_case_g val_type) cases >>= \cases_hs ->
    default_case_g default_case val_type >>= \default_case_hs ->
    return $ intercalate "\n" $ cases_hs ++ [ default_case_hs ]
  Nothing -> 
    mapM (flip not_last_case_g val_type) (init cases) >>= \cases_hs ->
    last_case_g (last cases) val_type >>= \last_case_hs ->
    return $ intercalate "\n" $ cases_hs ++ [ last_case_hs ]
  ) :: [ SpecificCase ] -> Maybe DefaultCase -> ValueType' -> Stateful Haskell

cases_type_inference_g = (
  \(CasesAndMaybeDefault case1 cases maybe_def_case) -> 
  get_indent_level >>= \indent_level ->
  update_indent_level (indent_level + 1) >>
  specific_case_type_inference_g case1 >>= \(case1_hs, val_type) ->
  mapM (flip not_last_case_g val_type) cases >>= \cases_hs ->
  maybe_def_case_g maybe_def_case val_type >>= \maybe_def_case_hs ->
  update_indent_level indent_level >>
  return
    ( indent indent_level ++ "\\case\n" ++
      intercalate "\n" ((case1_hs : cases_hs) ++ [maybe_def_case_hs])
    , val_type
    ) 
  ) :: Cases -> Stateful (Haskell, ValueType')

maybe_def_case_g = ( \maybe_def_case val_type -> case maybe_def_case of
  Just default_case -> default_case_g default_case val_type
  Nothing -> return ""
  ) :: Maybe DefaultCase -> ValueType' -> Stateful Haskell

-- ValueNameTypeAndExpression: name_type_and_value_g

name_type_and_value_g = ( \(value_name, value_type, value_expr) -> 
  get_indent_level >>= \ind_lev ->
  let 
  val_type = value_type_conversion value_type
    :: ValueType'
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
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: Values -> [ (ValueName, ValueType, ValueExpression) ]

list_of_values_g = ( \values ->
  values==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ (ValueName, ValueType, ValueExpression) ] -> Stateful Haskell

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueExpressionWhereValues val_expr values) val_type ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  insert_values_to_map values >>
  mapM values_g values >>= concat .> \ntavs_hs ->
  value_expression_g val_expr val_type >>= \val_expr_hs ->
  update_indent_level ind_lev >>
  return ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs)
  ) :: Where -> ValueType' -> Stateful Haskell

where_type_inference_g = ( \(ValueExpressionWhereValues val_expr values) ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  insert_values_to_map values >>
  mapM values_g values >>= concat .> \ntavs_hs ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, val_type) ->
  update_indent_level ind_lev >>
  return
    ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs, val_type)
  ) :: Where -> Stateful (Haskell, ValueType')

insert_values_to_map = 
  concatMap values_to_list .> mapM_ insert_value_to_map
  :: [ Values ] -> Stateful ()

insert_value_to_map = ( \(value_name, value_type, value_expr) ->
  value_map_insert value_name $ value_type_conversion value_type
  ) :: (ValueName, ValueType, ValueExpression) -> Stateful ()

-- CasesOrWhere: cases_or_where_g, cases_or_where_type_inference_g

cases_or_where_g = ( \case
  Cases cases -> cases_g cases
  Where where_ -> where_g where_
  ) :: CasesOrWhere -> ValueType' -> Stateful Haskell

cases_or_where_type_inference_g = ( \case
  Cases cases -> cases_type_inference_g cases
  Where where_ -> where_type_inference_g where_
  ) :: CasesOrWhere -> Stateful (Haskell, ValueType')

-- InputCasesOrWhere: abstraction_cases_or_where_g

abstraction_cases_or_where_g = ( \(InputAndCasesOrWhere input cases_or_where) ->
  input_g input >=> \(output_type, input_hs) ->
  cases_or_where_g cases_or_where output_type >>= \cases_or_where_hs ->
  return $ input_hs ++ cases_or_where_hs
  ) :: InputCasesOrWhere -> ValueType' -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValueType')

-- ValueExpression: value_expression_g, value_expression_type_inference_g

value_expression_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cases_or_where_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_g cases_or_where 
  OperatorExpression expr -> operator_expression_g expr
  ) :: ValueExpression -> ValueType' -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  OperatorExpression expr -> op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValueType')
