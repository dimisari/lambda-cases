{-# language LambdaCase #-}

module CodeGenerators.Values where

import Data.List
  ( intercalate, splitAt )
import Control.Monad
  ( foldM )
import Control.Monad.State
  ( (>=>) )

import Helpers
  ( Haskell, (==>), (.>), indent )

import HaskellTypes.LowLevel
  ( ValueName(..), Abstraction(..) )
import HaskellTypes.Types
  ( TypeName(..), ValueType(..) )
  
import HaskellTypes.Values
import HaskellTypes.AfterParsing
import HaskellTypes.Generation

import CodeGenerators.ErrorMessages
import CodeGenerators.LowLevel
import CodeGenerators.TypeChecking ( types_are_equivalent )
import CodeGenerators.Types ( value_type_g )


-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- FunctionApplicationChain,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, InputOpExpression, InputOpExprOrOpExpr
-- SpecificCase, Cases,
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, ValueExpression

-- Parenthesis: parenthesis_g, paren_type_inference_g

parenthesis_g = ( \(InnerExpression expr) val_type ->
  input_op_expr_or_op_expr_g expr val_type >>= \hs -> return $ "(" ++ hs ++ ")"
  ) :: Parenthesis -> ValType -> Stateful Haskell

paren_type_inference_g = ( \(InnerExpression expr) ->
  input_op_expr_or_op_expr_type_inf_g expr >>= \(hs, val_type) ->
  return ("(" ++ hs ++ ")", val_type)
  ) :: Parenthesis -> Stateful (Haskell, ValType)

-- Tuple:
-- tuple_g, type_name_tuple_values_g, tuple_types_and_values_g,
-- correct_type_name_tuple_values_g, tuple_type_inference_g

tuple_g = ( \(Values v1 v2 vs) val_type -> case val_type of 
  FuncType _ _ -> undefined
  NamedType tn -> type_name_tuple_values_g tn (v1 : v2 : vs)
  ProdType vt1 vt2 vts -> tuple_types_and_values_g (vt1 : vt2 : vts) (v1 : v2 : vs)
  ) :: Tuple -> ValType -> Stateful Haskell

type_name_tuple_values_g = ( \tn vs ->
  type_map_get tn "type_name_tuple_values_g" >>= \case
  FieldAndValTypeList fatl -> case length vs == length fatl of 
    False -> error values_fields_lengths_dont_match_err
    True -> correct_type_name_tuple_values_g tn (map get_f_valtype fatl) vs
  CaseAndMaybeValTypeList _ -> undefined
  ) :: TypeName -> [ InputOpExprOrOpExpr ] -> Stateful Haskell

tuple_types_and_values_g = ( \vts vs -> case length vts == length vs of
  False -> error tuple_values_types_lengths_dont_match_err
  True -> 
    zipWith input_op_expr_or_op_expr_g vs vts==>sequence >>= \vs_hs ->
    return $ "(" ++ intercalate ", " vs_hs ++ ")"
  ) :: [ ValType ] -> [ InputOpExprOrOpExpr ] -> Stateful Haskell

correct_type_name_tuple_values_g = ( \tn vts vs ->
  get_indent_level >>= \indent_level ->
  zipWith input_op_expr_or_op_expr_g vs vts==>sequence >>= \vs_hs -> 
  return $
    "\n" ++ indent (indent_level + 1) ++ show tn ++ "C" ++
    concatMap (\v_hs ->  " (" ++ v_hs ++ ")") vs_hs
  ) :: TypeName -> [ ValType ] -> [ InputOpExprOrOpExpr ] ->
       Stateful Haskell

tuple_type_inference_g = ( \(Values v1 v2 vs) ->
  mapM input_op_expr_or_op_expr_type_inf_g (v1 : v2 : vs) >>=
  unzip .> \(vs_hs, vts) -> case vts of
    vt1 : vt2 : vts ->
      return ("(" ++ intercalate ", " vs_hs ++ ")", ProdType vt1 vt2 vts)
    _ -> undefined
  ) :: Tuple -> Stateful (Haskell, ValType)

-- MathApplication: math_application_g, math_app_type_inference_g

math_application_g =
  math_app_to_app_tree .> application_tree_g
  :: MathApplication -> ValType -> Stateful Haskell

math_app_type_inference_g =  
  math_app_to_app_tree .> app_tree_type_inference_g
  :: MathApplication -> Stateful (Haskell, ValType)

-- BaseValue: base_value_g, base_value_type_inference_g

base_value_g = ( \case
  Parenthesis parenthesis -> parenthesis_g parenthesis
  Tuple tuple -> tuple_g tuple
  Literal literal -> literal_g literal
  ValueName value_name -> value_name_g value_name
  MathApplication math_application -> math_application_g math_application
  ) :: BaseValue -> ValType -> Stateful Haskell

base_value_type_inference_g = ( \case
  Parenthesis parenthesis -> paren_type_inference_g parenthesis
  Tuple tuple -> tuple_type_inference_g tuple
  Literal literal -> literal_type_inference_g literal
  ValueName value_name -> value_name_type_inference_g value_name
  MathApplication math_application -> math_app_type_inference_g math_application
  ) :: BaseValue -> Stateful (Haskell, ValType)

-- FunctionApplicationChain:
-- func_app_chain_g, application_tree_g, app_tree_type_inference_g

func_app_chain_g = 
  func_app_chain_to_app_tree .> application_tree_g
  :: FunctionApplicationChain -> ValType -> Stateful Haskell

application_tree_g = ( \case 
  BaseValueLeaf base_value -> base_value_g base_value
  app_tree -> \val_type ->
    app_tree_type_inference_g app_tree >>= \(app_tree_hs, app_tree_t) ->
    types_are_equivalent val_type app_tree_t >>= \case 
      True -> return app_tree_hs
      False -> undefined
  ) :: ApplicationTree -> ValType -> Stateful Haskell

app_tree_type_inference_g = ( \case 
  Application app_tree1 app_tree2 ->
    app_tree_type_inference_g app_tree1 >>= \(app_tree1_hs, app_tree1_t) ->
    case app_tree1_t of 
      FuncType input_t output_t -> 
        application_tree_g app_tree2 input_t >>= \app_tree2_hs ->
        let 
        app_tree2_hs_ = case app_tree2 of 
          Application _ _ -> "(" ++ app_tree2_hs ++ ")"
          _ -> app_tree2_hs
        in
        return (app_tree1_hs ++ " " ++ app_tree2_hs_, output_t) 
      _ -> undefined
  BaseValueLeaf base_value -> base_value_type_inference_g base_value
  ) :: ApplicationTree -> Stateful (Haskell, ValType)

-- MultiplicationFactor: multiplication_factor_g

multiplication_factor_g = ( \case
  FuncAppChain func_app_chain -> func_app_chain_g func_app_chain 
  BaseValue base_value -> base_value_g base_value 
  ) :: MultiplicationFactor -> ValType -> Stateful Haskell

-- Multiplication: multiplication_g

multiplication_g = ( \(MulFactors mul_fac1 mul_fac2 mul_facs) val_type -> 
  mapM (flip multiplication_factor_g val_type) (mul_fac1 : mul_fac2 : mul_facs) >>=
  intercalate " * " .> return
  ) :: Multiplication -> ValType -> Stateful Haskell

-- SubtractionFactor: subtraction_factor_g

subtraction_factor_g = ( \case
  Multiplication multiplication -> multiplication_g multiplication
  MultiplicationFactor mul_fac -> multiplication_factor_g mul_fac
  ) :: SubtractionFactor -> ValType -> Stateful Haskell

-- Subtraction: subtraction_g

subtraction_g = ( \(SubFactors subtraction_factor1 subtraction_factor2) val_type ->
  subtraction_factor_g subtraction_factor1 val_type >>= \subtraction_factor1_hs ->
  subtraction_factor_g subtraction_factor2 val_type >>= \subtraction_factor2_hs ->
  return $ subtraction_factor1_hs ++ " - " ++ subtraction_factor2_hs
  ) :: Subtraction -> ValType -> Stateful Haskell

-- EqualityFactor: equality_factor_g

equality_factor_g = ( \case
  Subtraction subtraction -> subtraction_g subtraction
  SubtractionFactor subtraction_factor -> subtraction_factor_g subtraction_factor
  ) :: EqualityFactor -> ValType -> Stateful Haskell

-- Equality: equality_g

equality_g = ( \(EqualityFactors equality_factor1 equality_factor2) -> \case 
  (NamedType (TN "Bool")) ->
    let int_t = (NamedType (TN "Int")) in
    equality_factor_g equality_factor1 int_t >>= \equality_factor1_hs ->
    equality_factor_g equality_factor2 int_t >>= \equality_factor2_hs ->
    return $ equality_factor1_hs ++ " == " ++ equality_factor2_hs
  _ -> undefined
  ) :: Equality -> ValType -> Stateful Haskell

-- OperatorExpression: operator_expression_g, op_expr_type_inference_g

operator_expression_g = ( \case
  EqualityFactor equality_factor -> equality_factor_g equality_factor
  Equality equality -> equality_g equality
  ) :: OperatorExpression -> ValType -> Stateful Haskell

op_expr_type_inference_g = ( \operator_expr ->
  let
  pair = case operator_expr of
    Equality equality -> (equality_g equality val_type, val_type) where
      val_type = NamedType $ TN "Bool"
    EqualityFactor equ_fac -> (equality_factor_g equ_fac val_type, val_type) where
      val_type = NamedType $ TN "Int"
    :: (Stateful Haskell, ValType)
  in
  pair ==> \(g, t) -> g >>= \hs -> return (hs, t)
  ) :: OperatorExpression -> Stateful (Haskell, ValType)

-- InputOpExpression: input_op_expression_g, input_op_expr_type_inference_g

input_op_expression_g = ( \(InputAndOpResult input operator_expr) ->
  input_g input >=> \(output_type, input_hs) ->
  operator_expression_g operator_expr output_type >>= \op_expr_hs ->
  return $ input_hs ++ op_expr_hs
  ) :: InputOpExpression -> ValType -> Stateful Haskell

abs_op_expr_type_inference_g = ( \(InputAndOpResult as opval) ->
  undefined
  ) :: InputOpExpression -> Stateful (Haskell, ValType)

-- InputOpExprOrOpExpr: input_op_expression_g, input_op_expr_type_inference_g

input_op_expr_or_op_expr_g = ( \case
  InputOpExpression input_op_expr -> input_op_expression_g input_op_expr
  OperatorExpression operator_expr -> operator_expression_g operator_expr
  ) :: InputOpExprOrOpExpr -> ValType -> Stateful Haskell

input_op_expr_or_op_expr_type_inf_g = ( \case
  InputOpExpression input_op_expr -> abs_op_expr_type_inference_g input_op_expr
  OperatorExpression operator_expr -> op_expr_type_inference_g operator_expr
  ) :: InputOpExprOrOpExpr -> Stateful (Haskell, ValType)

-- LiteralOrValueName: literal_or_value_name_g, lit_or_val_name_type_inference_g

literal_or_value_name_g = ( \case
  Lit literal -> literal_g literal
  ValName value_name -> val_name_insert_and_return value_name
  ) :: LiteralOrValueName -> ValType -> Stateful Haskell

lit_or_val_name_type_inference_g = ( 
  undefined
  ) :: LiteralOrValueName -> Stateful (Haskell, ValType)

-- SpecificCase: specific_case_g, abs_g, specific_case_type_inference_g

specific_case_g = ( \(SpecificCase lit_or_val_name value_expression) -> \case
  FuncType input_t output_t ->
    get_indent_level >>= \ind_lev ->
    literal_or_value_name_g lit_or_val_name input_t >>= \lit_or_val_name_hs ->
    value_expression_g output_t value_expression >>= \value_expr_hs ->
    return $ indent ind_lev ++ abs_g lit_or_val_name_hs ++ " -> " ++ value_expr_hs
  _ -> undefined
  ) :: SpecificCase -> ValType -> Stateful Haskell

abs_g = \case 
  "true" -> "True"
  "false" -> "False"
  g -> g
  :: String -> Haskell

specific_case_type_inference_g = ( \(SpecificCase lit_or_val_name val_expr) ->
  get_indent_level >>= \ind_lev ->
  lit_or_val_name_type_inference_g lit_or_val_name >>= \(lovn_g, lovn_vt) ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, v_vt) ->
  return (indent ind_lev ++ lovn_g ++ " -> " ++ val_expr_hs, FuncType lovn_vt v_vt)
  ) :: SpecificCase -> Stateful (Haskell, ValType)

-- DefaultCase: specific_case_g, specific_case_type_inference_g

default_case_g = ( \(DefaultCase value_expression) -> \case
  FuncType input_t output_t -> 
    get_indent_level >>= \ind_lev ->
    value_map_insert (VN "value") input_t >>
    value_expression_g output_t value_expression >>= \value_expression_hs ->
    return $ indent ind_lev ++ "value -> " ++ value_expression_hs
  _ -> undefined
  ) :: DefaultCase -> ValType -> Stateful Haskell

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
  ) :: Cases -> ValType -> Stateful Haskell

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
  ) :: Cases -> Stateful (Haskell, ValType)

-- NameTypeAndValue: name_type_and_value_g

name_type_and_value_g = ( \(NTAV value_name value_type value_expr) -> 
  get_indent_level >>= \ind_lev ->
  value_expression_g (value_type_to_val_type value_type) value_expr >>=
    \val_expr_hs ->
  return $
    "\n" ++ indent ind_lev ++ show value_name ++ " :: " ++ value_type_g value_type
    ++ "\n" ++ indent ind_lev ++ show value_name ++ " = " ++ val_expr_hs ++ "\n"
  ) :: NameTypeAndValue -> Stateful Haskell

ntav_map_insert = ( \(NTAV value_name value_type value_expr) ->
  value_map_insert value_name $ value_type_to_val_type value_type
  ) :: NameTypeAndValue -> Stateful ()

-- NameTypeAndValueLists: name_type_and_value_lists_g

name_type_and_value_lists_g =  
  ntav_lists_to_list_of_ntavs .> mapM name_type_and_value_g >=> concat .> return
  :: NameTypeAndValueLists -> Stateful Haskell

ntav_lists_to_list_of_ntavs = ( \ntav_lists -> case ntav_lists of
  NTAVLists (val_name : val_names) (val_type : val_types) (val_expr : val_exprs) ->
    NTAV val_name val_type val_expr :
    ntav_lists_to_list_of_ntavs (NTAVLists val_names val_types val_exprs)
  NTAVLists [] [] [] -> []
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: NameTypeAndValueLists -> [ NameTypeAndValue ]

-- NTAVOrNTAVLists: ntav_or_ntav_lists_g

ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue name_type_and_value -> name_type_and_value_g name_type_and_value
  NameTypeAndValueLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

ntav_or_ntav_lists_to_list_of_ntavs = ( \case 
  NameTypeAndValue name_type_and_value -> [ name_type_and_value ]
  NameTypeAndValueLists ntav_lists -> ntav_lists_to_list_of_ntavs ntav_lists
  ) :: NTAVOrNTAVLists -> [ NameTypeAndValue ]

-- NamesTypesAndValues: names_types_and_values_g

names_types_and_values_g =
  ns_ts_and_vs_to_list_of_ntavs .> list_of_ntavs_g
  :: NamesTypesAndValues -> Stateful Haskell

ns_ts_and_vs_to_list_of_ntavs = ( \(NTAVs ntavs) ->
  concatMap ntav_or_ntav_lists_to_list_of_ntavs ntavs
  ) :: NamesTypesAndValues -> [ NameTypeAndValue ]

list_of_ntavs_g = ( \list_of_ntavs ->
  mapM_ ntav_map_insert list_of_ntavs >>
  list_of_ntavs==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ NameTypeAndValue ] -> Stateful Haskell

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueWhereNTAVs val_expr ntavs) val_type ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_hs ->
  value_expression_g val_type val_expr >>= \val_expr_hs ->
  update_indent_level ind_lev >>
  return ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs)
  ) :: Where -> ValType -> Stateful Haskell

where_type_inference_g = ( \(ValueWhereNTAVs val_expr ntavs) ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_hs ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, val_type) ->
  update_indent_level ind_lev >>
  return
    ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs, val_type)
  ) :: Where -> Stateful (Haskell, ValType)

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

abstraction_cases_or_where_g = ( 
  \val_type (InputAndCOWResult input cases_or_where) ->
  input_g input val_type >>= \(output_type, input_hs) ->
  cases_or_where_g cases_or_where output_type >>= \cases_or_where_hs ->
  return $ input_hs ++ cases_or_where_hs
  ) :: ValType -> InputCasesOrWhere -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValType)

-- ValueExpression: value_expression_g, value_expression_type_inference_g

value_expression_g = ( \val_type -> \case
  InputCasesOrWhere abs_cow -> abstraction_cases_or_where_g val_type abs_cow
  CasesOrWhere cases_or_where -> cases_or_where_g cases_or_where val_type
  InputOpExprOrOpExpr expr -> input_op_expr_or_op_expr_g expr val_type
  ) :: ValType -> ValueExpression -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere abs_cow -> abstraction_cow_type_inference_g abs_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  InputOpExprOrOpExpr expr -> input_op_expr_or_op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValType)

