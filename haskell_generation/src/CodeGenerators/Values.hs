module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM, zipWithM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers 

import ParsingTypes.LowLevel
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
-- LitOrValName, Case, CasesExpr,
-- ValueNamesTypesAndExpressions, Values,
-- Where, CasesOrWhere, ValueExpression

-- Generate

instance Generate DefaultCase where
  generate = \(DefaultCase value_expression) output_t -> 
    get_ind_lev >>= \ind_lev ->
    generate value_expression output_t >>= \value_expression_hs ->
    return $ indent ind_lev ++ "_ -> " ++ value_expression_hs

instance Generate a => Generate (Maybe a) where
  generate = \case
    Just a -> generate a
    Nothing -> \_ -> return ""

instance Generate CasesExpr where
  generate = \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
    get_ind_lev >>= \ind_lev ->
    update_ind_lev (ind_lev + 1) >>
    generate (Cases (case1 : cases) maybe_def_case) val_type >>= \cases_hs ->
    update_ind_lev ind_lev >>
    return (indent ind_lev ++ "\\case\n" ++ cases_hs) 

data Cases = 
  Cases [ Case ] (Maybe DefaultCase)

instance Generate Cases where
  generate = \(Cases cases maybe_def_case) val_type ->
    case_to_lovns_exprs cases ==> \(lovns, exprs) ->
    check_is_func_type val_type >>= \func_t ->
    check_in_t_is_type_name func_t >>= \(type_name, out_t) ->
    let
    has_default = case maybe_def_case of
      Just _ -> True
      Nothing -> False
      :: Bool
    in
    type_map_get type_name >>= \case

      IntType ->
        check_lovns_int lovns has_default >>= \(ints, int_or_val_name) ->
        mapM (flip generate out_t) (zipWith IntCase ints {-- $ init --} exprs)
          >>= \int_cases_hs -> 
        generate (LastIntCase int_or_val_name (last exprs)) out_t
          >>= \last_int_case_hs ->
        generate maybe_def_case out_t
          >>= \maybe_def_case_hs ->
        return $
          intercalate "\n" $
          int_cases_hs ++ [ last_int_case_hs, maybe_def_case_hs ]

      OrType _ or_cases -> 
        let
        or_t_cases_names = map get_c_name or_cases
          :: [ ValueName ]
        in
        check_lovns_or_type lovns type_name or_t_cases_names has_default >>=
          \val_names ->
        or_type_cases_g
          (zipWith OrTypeCase val_names exprs)
          maybe_def_case
          or_t_cases_names
          func_t
      _ -> undefined

data IntCase = 
  IntCase Int ValueExpression

instance Generate IntCase where
  generate = \(IntCase i val_expr) out_t -> 
    get_ind_lev >>= \ind_lev ->
    generate val_expr out_t >>= \val_expr_hs ->
    return $ indent ind_lev ++ show i  ++ " -> " ++ val_expr_hs

data LastIntCase = 
  LastIntCase IntOrValName ValueExpression

instance Generate LastIntCase where
  generate = \(LastIntCase int_or_val_name val_expr) -> 
    case int_or_val_name of
      Int_ i -> generate (IntCase i val_expr)
      ValName_ val_name -> generate (ValNameIntCase val_name val_expr)

data ValNameIntCase = 
  ValNameIntCase ValueName ValueExpression

instance Generate ValNameIntCase where
  generate = \(ValNameIntCase val_name val_expr) out_t -> 
    get_ind_lev >>= \ind_lev ->
    val_n_ins_and_ret_hs val_name int >>= \val_name_hs ->
    generate val_expr out_t >>= \val_expr_hs ->
    return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs

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
    CasesExpr cases -> generate cases
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

--

not_last_or_type_case_g = (
  \(InAndOutTs in_t out_t) (OrTypeCase val_name val_expr) ->
  get_ind_lev >>= \ind_lev ->
  or_type_vn_g val_name in_t >>= \val_name_hs ->
  generate val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs ++ "\n"
  ) :: FuncType -> OrTypeCase -> Stateful Haskell

or_type_vn_g = ( \val_name val_type ->
  maybe_value_g val_name val_type >>= \maybe_value_hs ->
  return $ "C" ++ show val_name ++ maybe_value_hs
  ) :: ValueName -> ValType -> Stateful Haskell

-- 

last_or_type_case_g = (
  \(OrTypeCase val_name val_expr) (InAndOutTs in_t out_t) or_t_cases_names ->
  get_ind_lev >>= \ind_lev ->
  last_or_type_vn_g val_name or_t_cases_names in_t >>= \val_name_hs ->
  generate val_expr out_t >>= \val_expr_hs ->
  return $ indent ind_lev ++ val_name_hs ++ " -> " ++ val_expr_hs
  ) :: OrTypeCase -> FuncType -> [ ValueName ] -> Stateful Haskell

last_or_type_vn_g = ( \val_name or_t_cases_names val_type ->
  case elem val_name or_t_cases_names of
    True ->
      maybe_value_g val_name val_type >>= \maybe_value_hs ->
      return $ "C" ++ show val_name ++ maybe_value_hs
    False -> val_n_ins_and_ret_hs val_name val_type
  ) :: ValueName -> [ ValueName ] -> ValType -> Stateful Haskell

-- or type cases

or_type_cases_g = ( \cases maybe_def_case or_t_cases_names func_t ->
  case maybe_def_case of
    Just default_case -> or_type_cases_with_default_g cases default_case func_t
    Nothing -> or_type_cases_without_default_g cases func_t or_t_cases_names
  ) :: [ OrTypeCase ] -> Maybe DefaultCase -> [ ValueName ] ->
       FuncType -> Stateful Haskell

or_type_cases_with_default_g = (
  \cases default_case func_t@(InAndOutTs _ out_t) ->
  mapM (not_last_or_type_case_g func_t) cases >>= \cases_hs ->
  generate default_case out_t >>= \default_case_hs ->
  return $ concat cases_hs ++ default_case_hs
  ) :: [ OrTypeCase ] -> DefaultCase -> FuncType -> Stateful Haskell

or_type_cases_without_default_g = ( \cases func_t or_t_cases_names ->
  mapM (not_last_or_type_case_g func_t) (init cases) >>= \cases_hs ->
  last_or_type_case_g (last cases) func_t or_t_cases_names >>= \last_case_hs ->
  return $ concat cases_hs ++ last_case_hs
  ) :: [ OrTypeCase ] -> FuncType -> [ ValueName ] -> Stateful Haskell

-- Check cases for or_type

data OrTypeCase = 
  OrTypeCase ValueName ValueExpression
  
check_lovns_or_type = ( \lovns or_t_name or_t_cases_names has_default ->
  mapM check_lovn_is_val_name lovns >>= \val_names -> 
  catchE
    (check_val_names_in_cases has_default or_t_cases_names val_names)
    (append_to_err $ show or_t_name ++ "\n") >>= \cases_already_covered ->
  check_no_dupl_val_name val_names >> 
  catchE
    (check_all_cases_covered cases_already_covered val_names or_t_cases_names)
    (append_to_err $ cases_not_covered_err_cont or_t_name) >>
  return val_names
  ) ::
  [ LitOrValName ] -> TypeName -> [ ValueName ] -> Bool -> Stateful [ ValueName ]

check_lovn_is_val_name = ( \case
  ValueName val_name -> return val_name
  Literal lit -> throwE $ lit_in_or_type_case_err lit
  ) :: LitOrValName -> Stateful ValueName

check_val_names_in_cases = ( \has_default or_t_cases_names val_names ->
  case has_default of
    True ->
      mapM_ (check_val_name_in_cases or_t_cases_names) val_names >>
      return True
    False ->
      mapM_ (check_val_name_in_cases or_t_cases_names) (init val_names) >>
      return (not $ elem (last val_names) or_t_cases_names)
  ) :: Bool -> [ ValueName ] -> [ ValueName ] -> Stateful Bool

check_val_name_in_cases = ( \or_t_cases_names val_name ->
  case elem val_name or_t_cases_names of
    True -> return ()
    False -> throwE $ not_or_type_case_err_new val_name
  ) :: [ ValueName ] -> ValueName -> Stateful ()

check_no_dupl_val_name = ( \case
  [] -> return ()
  val_name : val_names -> case elem val_name val_names of
    True -> throwE $ duplicate_case_err val_name
    False -> check_no_dupl_val_name val_names
  ) :: [ ValueName ] -> Stateful ()

check_all_cases_covered = ( \cases_already_covered val_names or_t_cases_names ->
  case cases_already_covered of
    True -> return ()
    False -> check_all_cs_cov_no_def val_names or_t_cases_names
  ) :: Bool -> [ ValueName ] -> [ ValueName ] -> Stateful ()

check_all_cs_cov_no_def = ( \val_names or_t_cases_names ->
  filter (not . flip elem val_names) or_t_cases_names ==> \case
    [] -> return ()
    not_covered_cases -> throwE $ cases_not_covered_err not_covered_cases
  ) :: [ ValueName ] -> [ ValueName ] -> Stateful ()

--

data IntOrValName = 
  Int_ Int | ValName_ ValueName

check_lovns_int = ( \lovns has_default ->
  mapM check_lovn_is_int (init lovns) >>= \ints -> 
  check_last_lovn has_default (last lovns) >>= \int_or_val_name ->
  check_no_dupl_int_with_last ints int_or_val_name >>
  return (ints, int_or_val_name)
  ) :: [ LitOrValName ] -> Bool -> Stateful ([ Int ], IntOrValName)

check_lovn_is_int = ( \case
  ValueName val_name -> throwE $ wrong_int_case_err val_name
  Literal (Int i) -> return i
  ) :: LitOrValName -> Stateful Int

check_last_lovn = ( \has_default lovn -> case has_default of
  True -> check_lovn_is_int lovn >>= \i -> return $ Int_ i
  False -> check_last_lovn_is_val_name lovn
  ) :: Bool -> LitOrValName -> Stateful IntOrValName

check_last_lovn_is_val_name = ( \case
  ValueName val_name -> return $ ValName_ val_name
  Literal lit -> throwE $ last_int_case_err lit
  ) :: LitOrValName -> Stateful IntOrValName

check_no_dupl_int_with_last = ( \ints -> \case
  Int_ int -> check_no_dupl_int $ int : ints
  ValName_ _ -> check_no_dupl_int ints
  ) :: [ Int ] -> IntOrValName -> Stateful ()

check_no_dupl_int = ( \case
  [] -> return ()
  i : is -> case elem i is of
    True -> throwE $ duplicate_int_case_err i
    False -> check_no_dupl_int is
  ) :: [ Int ] -> Stateful ()

-- 

check_is_func_type = ( \case
  FuncType func_type -> return func_type
  other_t -> throwE $ cases_expr_not_func_t_err other_t
  ) :: ValType -> Stateful FuncType

check_in_t_is_type_name = ( \(InAndOutTs in_t out_t) -> case in_t of
  TypeApp (ConsAndTIns type_name []) -> return (type_name, out_t)
  other_t -> throwE $ cases_expr_wrong_in_t_err other_t
  ) :: FuncType -> Stateful (TypeName, ValType)

-- 

cases_type_inference_g = (
  undefined
  ) :: CasesExpr -> Stateful (Haskell, ValType)

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
  CasesExpr cases -> cases_type_inference_g $ remove_pos cases
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

--

append_to_err = ( \str (b, e_t, err_msg) -> throwE (b, e_t, err_msg ++ str) )
  :: String -> Error -> Stateful a

case_to_lovns_exprs = ( \case
  [] -> ([], [])
  Case lovn expr : cases ->
    case_to_lovns_exprs cases ==> \(lovns, exprs) ->
    (lovn : lovns, expr : exprs)
  ) :: [ Case ] -> ( [ LitOrValName ], [ ValueExpression ] )

