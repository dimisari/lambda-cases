module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel (ValueName(..), Abstraction(..))
import ParsingTypes.Types (TypeName(..), ValueType(..))
import ParsingTypes.OperatorValues
import ParsingTypes.Values

import AfterParsing.Types 
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (types_are_equivalent)

import CodeGenerators.LowLevelValues
import CodeGenerators.OperatorValues

-- All:
-- CaseLiteralOrValueName, SpecificCase, Cases,
-- ValueNamesTypesAndExpressions, Values,
-- Where, CasesOrWhere, ValueExpression

-- CaseLiteralOrValueName: literal_or_value_name_g, lit_or_val_name_type_inference_g

literal_or_value_name_g = ( \case
  CaseLiteral literal -> literal_g literal
  CaseValueName value_name -> val_name_insert_and_return value_name
  ) :: CaseLiteralOrValueName -> ValueType' -> Stateful Haskell

lit_or_val_name_type_inference_g = ( 
  undefined
  ) :: CaseLiteralOrValueName -> Stateful (Haskell, ValueType')

-- SpecificCase: specific_case_g, abs_g, specific_case_type_inference_g

specific_case_g = ( \(SpecificCase lit_or_val_name value_expression) -> \case
  FunctionType' (InputAndOutputType' input_t output_t) ->
    get_indent_level >>= \ind_lev ->
    literal_or_value_name_g lit_or_val_name input_t >>= \lit_or_val_name_hs ->
    value_expression_g value_expression output_t >>= \value_expr_hs ->
    return $ indent ind_lev ++ abs_g lit_or_val_name_hs ++ " -> " ++ value_expr_hs
  _ -> undefined
  ) :: SpecificCase -> ValueType' -> Stateful Haskell

abs_g = \case 
  "true" -> "True"
  "false" -> "False"
  g -> g
  :: String -> Haskell

specific_case_type_inference_g = ( \(SpecificCase lit_or_val_name val_expr) ->
  get_indent_level >>= \ind_lev ->
  lit_or_val_name_type_inference_g lit_or_val_name >>= \(lovn_g, lovn_t) ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, v_t) ->
  return
    ( indent ind_lev ++ lovn_g ++ " -> " ++ val_expr_hs
    , FunctionType' $ InputAndOutputType' lovn_t v_t )
  ) :: SpecificCase -> Stateful (Haskell, ValueType')

-- DefaultCase: specific_case_g, specific_case_type_inference_g

default_case_g = ( \(DefaultCase value_expression) -> \case
  FunctionType' (InputAndOutputType' input_t output_t) -> 
    get_indent_level >>= \ind_lev ->
    value_map_insert (VN "value") input_t >>
    value_expression_g value_expression output_t >>= \value_expression_hs ->
    return $ indent ind_lev ++ "value -> " ++ value_expression_hs
  _ -> undefined
  ) :: DefaultCase -> ValueType' -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g

cases_g = ( \cases val_type -> case cases of
  OneAndDefault specific_case default_case ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    specific_case_g specific_case val_type >>= \specific_case_hs ->
    default_case_g default_case val_type >>= \default_case_hs ->
    update_indent_level indent_level >>
    return ("\\case\n" ++ specific_case_hs ++ "\n" ++ default_case_hs)

  Many spec_case1 spec_case2 spec_cases mdc ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    mapM (flip specific_case_g val_type) specific_cases >>= \spec_cases_hs ->
    update_indent_level indent_level >>
    return ("\\case\n" ++ intercalate "\n" spec_cases_hs) where
    specific_cases = spec_case1 : spec_case2 : spec_cases
      :: [ SpecificCase ]
  ) :: Cases -> ValueType' -> Stateful Haskell

cases_type_inference_g = ( \case
  OneAndDefault specific_case default_case -> undefined
  Many spec_c1 spec_c2 spec_cs mdc ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    specific_case_type_inference_g spec_c1 >>= \(spec_c1_hs, val_type) ->
    mapM (flip specific_case_g val_type) (spec_c2 : spec_cs) >>= \spec_cs_hs ->
    update_indent_level indent_level >>
    return
      ("\\case\n" ++ spec_c1_hs ++ "\n" ++ intercalate "\n" spec_cs_hs, val_type)
  ) :: Cases -> Stateful (Haskell, ValueType')

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

-- ValueNamesTypesAndExpressions: ns_ts_and_es_to_list

ns_ts_and_es_to_list = ( \ntav_lists -> case ntav_lists of
  NamesTypesAndExpressions
    (val_name : val_names) (val_type : val_types) (val_expr : val_exprs) ->
    (val_name, val_type, val_expr) :
      ns_ts_and_es_to_list (NamesTypesAndExpressions val_names val_types val_exprs)
  NamesTypesAndExpressions [] [] [] -> []
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: ValueNamesTypesAndExpressions -> [ (ValueName, ValueType, ValueExpression) ]

-- Values:
-- values_g, values_to_list, list_of_values_g,
-- insert_value_to_map

values_g =
  values_to_list .> list_of_values_g
  :: Values -> Stateful Haskell

values_to_list = ( \(Values values) -> concatMap ns_ts_and_es_to_list values)
  :: Values -> [ (ValueName, ValueType, ValueExpression) ]

list_of_values_g = ( \values ->
  mapM_ insert_value_to_map values >>
  values==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ (ValueName, ValueType, ValueExpression) ] -> Stateful Haskell

insert_value_to_map = ( \(value_name, value_type, value_expr) ->
  value_map_insert value_name $ value_type_conversion value_type
  ) :: (ValueName, ValueType, ValueExpression) -> Stateful ()

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueExpressionWhereValues val_expr values) val_type ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  values_g values >>= \ntavs_hs ->
  value_expression_g val_expr val_type >>= \val_expr_hs ->
  update_indent_level ind_lev >>
  return ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs)
  ) :: Where -> ValueType' -> Stateful Haskell

where_type_inference_g = ( \(ValueExpressionWhereValues val_expr values) ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  values_g values >>= \ntavs_hs ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, val_type) ->
  update_indent_level ind_lev >>
  return
    ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs, val_type)
  ) :: Where -> Stateful (Haskell, ValueType')

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
  OperatorExpression expr -> input_op_expr_or_op_expr_g expr
  ) :: ValueExpression -> ValueType' -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  OperatorExpression expr -> input_op_expr_or_op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValueType')
