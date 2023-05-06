module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM, zipWithM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers 

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

-- Generate

instance Generate DefaultCase where
  generate = \(DefaultCase value_expression) output_t -> 
    get_ind_lev >>= \ind_lev ->
    generate value_expression output_t >>= \value_expression_hs ->
    return $ indent ind_lev ++ "_ -> " ++ value_expression_hs

instance Generate Cases where
  generate = \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
    get_ind_lev >>= \ind_lev ->
    update_ind_lev (ind_lev + 1) >>
    generate (CsAndDefC (case1 : cases) maybe_def_case) val_type >>= \cases_hs ->
    update_ind_lev ind_lev >>
    return (indent ind_lev ++ "\\case\n" ++ cases_hs) 

data CasesList = 
  CsAndDefC [ Case ] (Maybe DefaultCase)

instance Generate CasesList where
  generate = \(CsAndDefC cases maybe_def_case) val_type ->
    check_type val_type >>= \case
      IntInput out_t -> int_cases_list_g cases maybe_def_case out_t
      OrTInput or_type_names func_t ->
        check_or_type_cases cases >>= \pairs ->
        or_type_cases_g pairs maybe_def_case or_type_names func_t

instance Generate Where where
  generate = \(ValueExpressionWhereValues val_expr values) val_type ->
    get_ind_lev >>= \ind_lev -> update_ind_lev (ind_lev + 1) >>
    insert_values_to_map values >>
    mapM values_g values >>= concat .> \values_hs ->
    generate val_expr val_type >>= \val_expr_hs ->
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

instance Generate CasesOrWhere where
  generate =  \case
    Cases cases -> generate cases
    Where where_ -> generate where_

instance Generate InputCasesOrWhere where
  generate = \(InputAndCasesOrWhere input cases_or_where) ->
    input_g input >=> \(output_type, input_hs) ->
    generate cases_or_where output_type >>= \cases_or_where_hs ->
    input_val_map_remove input >>
    return (input_hs ++ cases_or_where_hs)

instance Generate ValueExpression where
  generate = \case
    InputCasesOrWhere input_cow -> generate input_cow
    CasesOrWhere cases_or_where -> generate cases_or_where 
    OpExpr expr -> generate expr

-- LitOrValName: lit_or_val_name_g

-- or_type_vn_g, last_or_type_vn_g

check_is_or_case = ( \val_name val_type -> \case
  (_, []) -> throwE $ not_or_type_case_err val_name val_type
  (acc, c:cs) -> case c == val_name of
    True -> return $ acc ++ cs
    False -> check_is_or_case val_name val_type (c:acc, cs)
  ) :: ValueName -> ValType -> ([ValueName], [ValueName]) ->
       Stateful [ ValueName ]

or_type_vn_g = ( \val_name val_type or_t_cs ->
  check_is_or_case val_name val_type ([], or_t_cs) >>= \other_names ->
  maybe_value_g val_name val_type >>= \maybe_value_hs ->
  return (other_names, "C" ++ show val_name ++ maybe_value_hs)
  ) :: ValueName -> ValType -> [ ValueName ] -> Stateful ([ ValueName ], Haskell)

last_or_type_vn_g = ( \val_name or_t_cs val_type -> case elem val_name or_t_cs of
  True -> case or_t_cs == [ val_name ] of 
    True ->
      maybe_value_g val_name val_type >>= \maybe_value_hs ->
      return $ "C" ++ show val_name ++ maybe_value_hs
    False -> throwE $ cases_not_covered val_name or_t_cs
  False -> val_n_ins_and_ret_hs val_name val_type
  ) :: ValueName -> [ ValueName ] -> ValType -> Stateful Haskell

not_last_or_type_case_g = (
  \(InAndOutTs in_t out_t) (or_t_cs, hs_so_far) (val_name, val_expr) ->
  get_ind_lev >>= \ind_lev ->
  or_type_vn_g val_name in_t or_t_cs >>= \(new_names, val_name_hs) ->
  generate val_expr out_t >>= \val_expr_hs ->
  return
    ( new_names
    , hs_so_far ++ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs ++ "\n"
    )
  ) :: FuncType -> ([ ValueName ], Haskell) -> (ValueName, ValueExpression) ->
       Stateful ([ ValueName ], Haskell)

last_or_type_case_g = ( \(val_name, val_expr) (InAndOutTs in_t out_t) or_t_cs ->
  get_ind_lev >>= \ind_lev ->
  last_or_type_vn_g val_name or_t_cs in_t >>= \val_name_hs ->
  generate val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs
  ) :: (ValueName, ValueExpression) -> FuncType -> [ ValueName ] ->
       Stateful Haskell

-- int LitOrValName

not_last_int_lovn_g = ( \case 
  ValueName val_name -> throwE $ wrong_int_case_err val_name
  Literal literal -> generate literal int
  ) :: LitOrValName -> Stateful Haskell

last_int_lovn_g = ( \case 
  ValueName val_name -> val_n_ins_and_ret_hs val_name int
  Literal literal -> throwE $ last_int_case_err literal
  ) :: LitOrValName -> Stateful Haskell

-- int case

not_last_int_case_g = int_case_g not_last_int_lovn_g
  :: Case -> ValType -> Stateful Haskell

last_int_case_g = int_case_g last_int_lovn_g
  :: Case -> ValType -> Stateful Haskell

int_case_g = ( \lovn_g (Case lit_or_val_name val_expr) out_t ->
  get_ind_lev >>= \ind_lev ->
  lovn_g lit_or_val_name >>= \int_lovn_hs ->
  generate val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ int_lovn_hs ++ " -> " ++ val_expr_hs
  ) :: (LitOrValName -> Stateful Haskell) -> Case -> ValType ->
       Stateful Haskell

-- int cases

int_cases_list_g = ( \cases maybe_def_case out_t -> case maybe_def_case of
  Just default_case -> int_cases_list_with_default_g cases default_case out_t
  Nothing -> int_cases_list_without_default_g cases out_t
  ) :: [ Case ] -> Maybe DefaultCase -> ValType -> Stateful Haskell

int_cases_list_with_default_g = ( \cases default_case out_t ->
  mapM (flip not_last_int_case_g out_t) cases >>= \cases_hs ->
  generate default_case out_t >>= \default_case_hs ->
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
  generate default_case out_t >>= \default_case_hs ->
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
    True -> throwE $ diplicate_case_err vn
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
  generate value_expr val_type >>= \val_expr_hs ->
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

-- Where: where_type_inference_g 

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

-- CasesOrWhere: cases_or_where_type_inference_g

cases_or_where_type_inference_g = ( \case
  Cases cases -> cases_type_inference_g $ remove_pos cases
  Where where_ -> where_type_inference_g where_
  ) :: CasesOrWhere -> Stateful (Haskell, ValType)

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValType)

-- ValueExpression: value_expression_type_inference_g

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  OpExpr expr -> generate_infer expr
  ) :: ValueExpression -> Stateful (Haskell, ValType)
