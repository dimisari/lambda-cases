module Generation.Final.Values where

import Data.Functor ((<&>))
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

import Generation.State.TypesAndOperations

import Generation.Helpers.ErrorMessages
import Generation.Helpers.TypeChecking (equiv_types)
import Generation.Helpers.Helpers
import Generation.Helpers.CheckCases

import Generation.Final.LowLevel
import Generation.Final.OperatorValues

-- Generate
-- DefaultCase, Maybe, CasesExpr, Cases,

data Indent a = Indent a 

instance Generate a => Generate (Indent a) where
  generate = \(Indent a) val_type -> 
    indentation +++ generate a val_type

instance Generate DefaultCase where
  generate = \(DefaultCase value_expression) output_t -> 
    ("_ -> " ++) <$> generate value_expression output_t 

instance Generate a => Generate (Maybe a) where
  generate = \case
    Just a -> generate a
    Nothing -> \_ -> return ""

instance Generate CasesExpr where
  generate = \(CasesAndMaybeDefault case1 cases maybe_def_case) val_type -> 
    modify_ind_lev (+ 1) >>
    ("\\case\n" ++) <$> generate (Cases (case1 : cases) maybe_def_case) val_type
    <* modify_ind_lev ((-) 1)

data Cases = 
  Cases [ Case ] (Maybe DefaultCase)

instance Generate Cases where
  generate = \(Cases cases maybe_def_case) val_type ->
    check_is_func_type val_type >>= \func_t ->
    check_in_t_is_type_name func_t >>= \(type_name, out_t) ->
    let
    has_default = case maybe_def_case of
      Just _ -> True
      Nothing -> False
      :: Bool
    in
    check_and_gen_cases cases has_default func_t type_name out_t >>= \cases_hs ->
    generate (Indent maybe_def_case) out_t >>= \maybe_def_case_hs ->
    return $ cases_hs ++ maybe_def_case_hs

check_and_gen_cases = ( \cases has_default func_t type_name out_t ->
  case_to_lovns_exprs cases ==> \(lovns, exprs) ->
  type_map_get type_name >>= \case
    IntType -> check_and_gen_int_cases lovns has_default exprs out_t
    OrType _ or_cases -> 
      check_and_gen_or_type_cases
        lovns type_name (map get_c_name or_cases) has_default exprs func_t
    _ -> undefined
  ) :: [ Case ] -> Bool -> FuncType -> TypeName -> ValType -> Stateful Haskell

check_and_gen_int_cases = ( \lovns has_default exprs out_t ->
  check_lovns_int lovns has_default >>= \(ints, int_or_val_name) ->
  generate (IntCases ints int_or_val_name exprs) out_t
  ) ::
  [ LitOrValName ] -> Bool -> [ ValueExpression ] -> ValType -> Stateful Haskell

check_and_gen_or_type_cases = ( \lovns t_name cases_names has_def exprs func_t ->
  check_lovns_or_type lovns t_name cases_names has_def >>= \(vns, last_vn) ->
  generate_func_type (OrTypeCases vns last_vn exprs) func_t
  ) ::
  [ LitOrValName ] -> TypeName -> [ ValueName ] -> Bool -> [ ValueExpression ] ->
  FuncType -> Stateful Haskell

data IntCases = 
  IntCases [ Int ] IntOrValName [ ValueExpression ]

instance Generate IntCases where
  generate = \(IntCases ints int_or_val_name exprs) out_t ->
    ( concat <$>
    mapM (flip generate out_t) (map Indent $ zipWith IntCase ints $ init exprs)
    ) +++
    generate (Indent $ LastIntCase int_or_val_name (last exprs)) out_t

-- GenerateFuncType 

class GenerateFuncType a where
  generate_func_type :: a -> FuncType -> Stateful Haskell

instance GenerateFuncType a => GenerateFuncType (Indent a) where
  generate_func_type = \(Indent a) val_type -> 
    indentation +++ generate_func_type a val_type

data OrTypeCases = 
  OrTypeCases [ ValueName ] SpecificOrDefaultCaseVN [ ValueExpression ]

instance GenerateFuncType OrTypeCases where
  generate_func_type = \(OrTypeCases val_names last_val_name exprs) func_t ->
    ( concat <$>
      mapM
        (flip generate_func_type func_t)
        (map Indent $ zipWith OrTypeCase val_names $ init exprs)
    ) +++
    generate_func_type (Indent $ LastOrTypeCase last_val_name (last exprs)) func_t

data OrTypeCase = 
  OrTypeCase ValueName ValueExpression

instance GenerateFuncType OrTypeCase where
  generate_func_type = \(OrTypeCase val_name val_expr) (InAndOutTs in_t out_t) ->
    or_type_vn_g val_name in_t >>= \(val_name_hs, inserted) ->
    (((val_name_hs ++ " -> ") ++) <$> generate val_expr out_t <&> (++ "\n"))
    <* mapM_ value_map_remove inserted

data LastOrTypeCase = 
  LastOrTypeCase SpecificOrDefaultCaseVN ValueExpression

instance GenerateFuncType LastOrTypeCase where
  generate_func_type = \(LastOrTypeCase last_val_name val_expr) ->
    case last_val_name of
      SpecificValName val_name ->
        generate_func_type (OrTypeCase val_name val_expr)
      DefaultValName val_name ->
        generate_func_type (DefaultOrTypeCase val_name val_expr)

data DefaultOrTypeCase = 
  DefaultOrTypeCase ValueName ValueExpression

instance GenerateFuncType DefaultOrTypeCase where
  generate_func_type =
    \(DefaultOrTypeCase val_name val_expr) (InAndOutTs in_t out_t) ->
    value_map_insert val_name in_t >>
    generate val_expr out_t >>= \val_expr_hs ->
    value_map_remove val_name >>
    return (show val_name ++ " -> " ++ val_expr_hs ++ "\n")

-- Int cases

data IntCase = 
  IntCase Int ValueExpression

instance Generate IntCase where
  generate = \(IntCase i val_expr) out_t -> 
    generate val_expr out_t >>= \val_expr_hs ->
    return $ show i ++ " -> " ++ val_expr_hs ++ "\n"

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
    value_map_insert val_name int >>
    generate val_expr out_t >>= \val_expr_hs ->
    value_map_remove val_name >>
    return (show val_name ++ " -> " ++ val_expr_hs ++ "\n")

-- Where

instance Generate Where where
  generate = \(ValueExpressionWhereValues val_expr values) val_type ->
    get_ind_lev >>= \ind_lev ->
    set_ind_lev (ind_lev + 1) >>

    insert_values_to_map values >>

    concat <$> mapM values_g values >>= \values_hs ->
    generate val_expr val_type >>= \val_expr_hs ->
    
    remove_values_from_map values >>

    set_ind_lev ind_lev >>
    return
      ( "\n" ++
        indent (ind_lev + 1) ++ "let" ++
        indent (ind_lev + 1) ++ values_hs ++
        indent (ind_lev + 1) ++ "in" ++
        "\n" ++
        indent (ind_lev + 1) ++ val_expr_hs
      )

-- 
 
instance Generate CasesOrWhere where
  generate =  \case
    CasesExpr cases_expr -> generate (Indent cases_expr)
    Where where_ -> generate where_

instance Generate InputCasesOrWhere where
  generate = \(InputAndCasesOrWhere input cases_or_where) ->
    input_g input >=> \(input_hs, output_type, inserted) ->
    generate cases_or_where output_type >>= \cases_or_where_hs ->
    mapM_ value_map_remove inserted >>
    return (input_hs ++ cases_or_where_hs)

instance Generate ValueExpression where
  generate = \case
    InputCasesOrWhere input_cow -> generate input_cow
    CasesOrWhere cases_or_where -> generate cases_or_where 
    OpExpr expr -> generate expr

--

or_type_vn_g = ( \val_name val_type ->
  maybe_value_g val_name val_type >>= \(maybe_value_hs, inserted) ->
  return ("C" ++ show val_name ++ maybe_value_hs, inserted)
  ) :: ValueName -> ValType -> Stateful (Haskell, [ ValueName ])

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
  val_type = to_val_type value_type
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
  value_map_insert value_name $ to_val_type value_type
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

case_to_lovns_exprs = ( \case
  [] -> ([], [])
  Case lovn expr : cases ->
    case_to_lovns_exprs cases ==> \(lovns, exprs) ->
    (lovn : lovns, expr : exprs)
  ) :: [ Case ] -> ( [ LitOrValName ], [ ValueExpression ] )
