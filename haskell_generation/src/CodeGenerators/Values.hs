module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM)
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
import GenerationHelpers.TypeChecking (types_are_equivalent)

import CodeGenerators.LowLevel
import CodeGenerators.OperatorValues

-- All:
-- LitOrValName, SpecificCase, Cases,
-- ValueNamesTypesAndExpressions, Values,
-- Where, CasesOrWhere, ValueExpression

-- LitOrValName: lit_or_val_name_g

data ValNameIsOrCase = 
  IsCase [ ValueName ] | IsNotCase

check_is_or_case = ( \val_name -> \case
  (_, []) -> IsNotCase
  (acc, c:cs) -> case c == val_name of
    True -> IsCase $ acc ++ cs
    False -> check_is_or_case val_name (c:acc, cs)
  ) :: ValueName -> ([ValueName], [ValueName])  -> ValNameIsOrCase

maybe_value_vn_g = ( \val_name val_type -> 
  value_map_get val_name >>= \val_name_t ->
  types_are_equivalent val_name_t val_type >>= \case
    True -> return $ "C" ++ show val_name
    False -> has_value_vn_g val_name val_name_t
  ) :: ValueName -> ValType -> Stateful Haskell

has_value_vn_g = ( \val_name -> \case
  FuncType (InAndOutTs in_t _) ->
    value_map_insert (VN "value") in_t >> case in_t of
    TypeApp (ConsAndInTs type_name []) -> type_map_get type_name >>= \case
      TupleType _ fields -> with_tuple_t_value_vn_g val_name type_name fields
      _ -> return ("C" ++ show val_name ++ " value")
    ProdType types -> with_prod_t_value_vn_g val_name types
    _ -> return ("C" ++ show val_name ++ " value")
  _ -> error
    "case_with_value_vn_g: val_name_t not a FuncType, should be impossible"
  ) :: ValueName -> ValType -> Stateful Haskell

with_tuple_t_value_vn_g = ( \val_name type_name fields ->
  mapM field_ins_and_ret_hs fields >>= \fields_hs ->
  return
    ( "C" ++ show val_name ++ " value@(C" ++ show type_name ++ 
      concatMap (" " ++) fields_hs ++ ")"
    )
  ) :: ValueName -> TypeName -> [ Field' ] -> Stateful Haskell

with_prod_t_value_vn_g = ( \val_name types ->
  zipWith val_n_ins_and_ret_hs prod_t_field_ns types ==> sequence >>= \fields_hs ->
  return
    ( "C" ++ show val_name ++ " value@(" ++ intercalate (", ") fields_hs ++ ")" )
  ) :: ValueName -> [ ValType ] -> Stateful Haskell

or_type_vn_g = ( \val_name names val_type ->
  case check_is_or_case val_name ([], names) of
    IsCase other_names ->
      maybe_value_vn_g val_name val_type >>= \hs ->
      return (other_names, hs)
    IsNotCase -> throwE $
      show val_name ++ " is not a case of the or_type: " ++ show val_type
  ) :: ValueName -> [ ValueName ] -> ValType ->
       Stateful ([ ValueName ], Haskell)

last_or_type_vn_g = ( \val_name names val_type -> case elem val_name names of
  True -> case names == [ val_name ] of 
    True -> maybe_value_vn_g val_name val_type
    False -> throwE $
      "the following cases are not covered: " ++ show (filter (/= val_name) names)
  False -> val_n_ins_and_ret_hs val_name val_type
  ) :: ValueName -> [ ValueName ] -> ValType -> Stateful Haskell

int_lovn_g = ( \case 
  ValueName val_name -> throwE $ "case of Int type can't be: " ++ show val_name
  Literal literal -> literal_g literal int
  ) :: LitOrValName -> Stateful Haskell

last_int_lovn_g = ( \case 
  ValueName val_name -> val_n_ins_and_ret_hs val_name int
  Literal literal ->
    throwE $
      "last case of Int type must be \"... ->\" or \"some_name ->\""
      ++
      "to catch all the remaining cases.\nInstead of: \""
      ++ show literal ++ " ->\""
  ) :: LitOrValName -> Stateful Haskell

not_last_lit_or_val_name_g = ( \case
  Literal literal -> literal_g literal
  ValueName value_name -> value_name_g value_name
  ) :: LitOrValName -> ValType -> Stateful Haskell

last_lit_or_val_name_g = ( \case
  Literal literal -> literal_g literal
  ValueName value_name -> val_n_ins_and_ret_hs value_name
  ) :: LitOrValName -> ValType -> Stateful Haskell

-- SpecificCase: abs_g 

or_type_spec_case_g = (
  \(InAndOutTs in_t out_t) (names, hs_so_far) (val_name, val_expr) ->
  get_ind_lev >>= \ind_lev ->
  or_type_vn_g val_name names in_t >>= \(new_names, val_name_hs) ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return
    ( new_names
    , hs_so_far ++ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs ++ "\n"
    )
  ) :: FuncType -> ([ ValueName ], Haskell) -> (ValueName, ValueExpression) ->
       Stateful ([ ValueName ], Haskell)

or_type_last_case_g = ( \(val_name, val_expr) (InAndOutTs in_t out_t) names ->
  get_ind_lev >>= \ind_lev ->
  last_or_type_vn_g val_name names in_t >>= \val_name_hs ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs
  ) :: (ValueName, ValueExpression) -> FuncType -> [ ValueName ] ->
       Stateful Haskell

int_specific_case_g = ( \(SpecificCase lit_or_val_name val_expr) out_t ->
  get_ind_lev >>= \ind_lev ->
  int_lovn_g lit_or_val_name >>= \int_lovn_hs ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ int_lovn_hs ++ " -> " ++ val_expr_hs
  ) :: SpecificCase -> ValType -> Stateful Haskell

int_last_case_g = ( \(SpecificCase lit_or_val_name val_expr) out_t ->
  get_ind_lev >>= \ind_lev ->
  last_int_lovn_g lit_or_val_name >>= \int_lovn_hs ->
  value_expression_g val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ int_lovn_hs ++ " -> " ++ val_expr_hs
  ) :: SpecificCase -> ValType -> Stateful Haskell

-- DefaultCase: 

default_case_g = ( \(DefaultCase value_expression) output_t -> 
  get_ind_lev >>= \ind_lev ->
  value_expression_g value_expression output_t >>= \value_expression_hs ->
  return $ indent ind_lev ++ "_ -> " ++ value_expression_hs
  ) :: DefaultCase -> ValType -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g

cases_g = ( \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
  get_ind_lev >>= \ind_lev ->
  update_ind_lev (ind_lev + 1) >>
  cases_help_g (case1 : cases) maybe_def_case val_type >>= \cases_hs ->
  update_ind_lev ind_lev >>
  return (indent ind_lev ++ "\\case\n" ++ cases_hs) 
  ) :: Cases -> ValType -> Stateful Haskell

cases_help_g = ( \cases maybe_def_case val_type ->
  check_type val_type >>= \case
    IntInput out_t -> int_cases_g cases maybe_def_case out_t
    OrTInput or_type_names func_t ->
      check_or_type_scs cases >>= \pairs ->
      or_type_cases_g pairs maybe_def_case or_type_names func_t
  ) :: [ SpecificCase ] -> Maybe DefaultCase -> ValType -> Stateful Haskell

int_cases_g = ( \cases maybe_def_case out_t -> case maybe_def_case of
  Just default_case ->
    mapM (flip int_specific_case_g out_t) cases >>= \cases_hs ->
    default_case_g default_case out_t >>= \default_case_hs ->
    return $ intercalate "\n" $ cases_hs ++ [ default_case_hs ]
  Nothing -> 
    mapM (flip int_specific_case_g out_t) (init cases) >>= \cases_hs ->
    int_last_case_g (last cases) out_t >>= \last_case_hs ->
    return $ intercalate "\n" $ cases_hs ++ [ last_case_hs ]
  ) :: [ SpecificCase ] -> Maybe DefaultCase -> ValType -> Stateful Haskell

or_type_cases_g = (
  \cases maybe_def_case or_type_names func_t@(InAndOutTs _ out_t) ->
  case maybe_def_case of
    Just default_case ->
      foldM (or_type_spec_case_g func_t) (or_type_names, "") cases >>=
        \(_, cases_hs) ->
      default_case_g default_case out_t >>= \default_case_hs ->
      return $ cases_hs ++ default_case_hs
    Nothing -> 
      foldM (or_type_spec_case_g func_t) (or_type_names, "") (init cases) >>=
        \(final_names, cases_hs) ->
      or_type_last_case_g (last cases) func_t final_names >>= \last_case_hs ->
      return $ cases_hs ++ last_case_hs
  ) :: [ (ValueName, ValueExpression) ] -> Maybe DefaultCase -> [ ValueName ] ->
       FuncType -> Stateful Haskell

check_or_type_scs = ( \scs ->
  (map sc_lovn scs ==> check_lovns) >>= \vns ->
  return $ zip vns $ map sc_val_e scs
  ) :: [ SpecificCase ] -> Stateful [ (ValueName, ValueExpression) ]

check_lovns = ( \lovns ->
  check_all_vns lovns >>= \vns ->
  check_dup_vns vns >> return vns
  ) :: [ LitOrValName ] -> Stateful [ ValueName ]

check_all_vns = mapM check_vn
  :: [ LitOrValName ] -> Stateful [ ValueName ]

check_vn = ( \case
  ValueName vn -> return vn
  Literal lit -> throwE $
    "can't have literal: " ++ show lit ++ "in case of or_type"
  ) :: LitOrValName -> Stateful ValueName

check_dup_vns = ( \case
  [] -> return ()
  vn : vns -> case elem vn vns of
    True -> throwE $ "duplicate or_type case: " ++ show vn
    False -> check_dup_vns vns
  ) :: [ ValueName ] -> Stateful ()

data CasesTypeInfo =
  IntInput ValType | OrTInput [ ValueName ] FuncType

check_type = ( \case
  FuncType func_type -> check_func_type func_type
  other_t -> throwE $
    "cases expression has type:" ++ show other_t ++
    "\ninstead of a funtion type\n"
  ) :: ValType -> Stateful CasesTypeInfo

check_func_type = ( \func_type@(InAndOutTs in_t out_t) -> case in_t of
  TypeApp (ConsAndInTs type_name []) ->
    type_map_get type_name >>= \case
      OrType _ or_cases ->
        return $ OrTInput (map get_c_name or_cases) func_type
      IntType -> return $ IntInput out_t
      _ -> undefined
  other_t -> throwE $ 
    "cases expression has input type:" ++ show other_t ++
    "\ninstead of: some or_type, Int or Char \n"
  ) :: FuncType -> Stateful CasesTypeInfo

cases_type_inference_g = (
  undefined
  ) :: Cases -> Stateful (Haskell, ValType)

-- ValueNameTypeAndExpression: name_type_and_value_g

name_type_and_value_g = ( \(value_name, value_type, value_expr) -> 
  get_ind_lev >>= \ind_lev ->
  let 
  val_type = value_type_conversion value_type
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
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: Values -> [ (ValueName, ValueType, ValueExpression) ]

list_of_values_g = ( \values ->
  values==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ (ValueName, ValueType, ValueExpression) ] -> Stateful Haskell

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueExpressionWhereValues val_expr values) val_type ->
  get_ind_lev >>= \ind_lev -> update_ind_lev (ind_lev + 1) >>
  insert_values_to_map values >>
  mapM values_g values >>= concat .> \values_hs ->
  value_expression_g val_expr val_type >>= \val_expr_hs ->
  remove_values_from_map values >>
  update_ind_lev ind_lev >>
  return ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ values_hs)
  ) :: Where -> ValType -> Stateful Haskell

where_type_inference_g = ( \(ValueExpressionWhereValues val_expr values) ->
  get_ind_lev >>= \ind_lev -> update_ind_lev (ind_lev + 1) >>
  insert_values_to_map values >>
  mapM values_g values >>= concat .> \values_hs ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, val_type) ->
  remove_values_from_map values >>
  update_ind_lev ind_lev >>
  return
    ( "\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ values_hs
    , val_type
    )
  ) :: Where -> Stateful (Haskell, ValType)

insert_values_to_map = 
  concatMap values_to_list .> mapM_ insert_value_to_map
  :: [ Values ] -> Stateful ()

remove_values_from_map = 
  concatMap values_to_list .> mapM_ remove_value_from_map
  :: [ Values ] -> Stateful ()

insert_value_to_map = ( \(value_name, value_type, value_expr) ->
  value_map_insert value_name $ value_type_conversion value_type
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
  return $ input_hs ++ cases_or_where_hs
  ) :: InputCasesOrWhere -> ValType -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValType)

-- ValueExpression: value_expression_g, value_expression_type_inference_g

value_expression_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cases_or_where_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_g cases_or_where 
  OperatorExpression expr -> operator_expression_g expr
  ) :: ValueExpression -> ValType -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  OperatorExpression expr -> op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValType)
