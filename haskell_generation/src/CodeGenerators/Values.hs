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
  ( ValType(..), ApplicationTree(..), fac_to_app_tree, math_app_to_app_tree
  , ValFieldsOrCases(..), FieldAndValType(..), value_type_to_val_type )
import HaskellTypes.Generation
  ( Stateful, get_indent_level, update_indent_level, value_map_get
  , value_map_insert, type_map_get )

import CodeGenerators.ErrorMessages
import CodeGenerators.LowLevel
  ( literal_g, literal_type_inference_g, value_name_g, value_name_type_inference_g
  , abstraction_g )
import CodeGenerators.TypeChecking
  ( ts_are_equivalent )
import CodeGenerators.Types
  ( value_type_g )


-- All:
-- ParenthesisValue, TupleValue, MathApplication, BaseValue
-- FunctionApplicationChain,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, AbstractionOpExpression, AbsOpOrOpExpression
-- SpecificCase, Cases,
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, ValueExpression

-- ParenthesisValue: parenthesis_value_g, parenthesis_value_type_inference_g

parenthesis_value_g = ( \vt (Parenthesis expr) ->
  abs_op_expr_or_op_expr_g vt expr >>= \hs -> return $ "(" ++ hs ++ ")"
  ) :: ValType -> ParenthesisValue -> Stateful Haskell

parenthesis_value_type_inference_g = ( \(Parenthesis expr) ->
  abs_op_expr_or_op_expr_type_inference_g expr >>= \(vt, hs) ->
  return (vt, "(" ++ hs ++ ")")
  ) :: ParenthesisValue -> Stateful (ValType, Haskell)

-- TupleValue:
-- tuple_value_g, type_name_tuple_values_g, tuple_types_and_values_g,
-- correct_type_name_tuple_values_g, tuple_value_type_inference_g

tuple_value_g = ( \vt (Values v1 v2 vs) -> case vt of 
  FuncType _ _ -> undefined
  NamedType tn -> type_name_tuple_values_g tn (v1 : v2 : vs)
  TupleValType vt1 vt2 vts ->
    tuple_types_and_values_g (vt1 : vt2 : vts) (v1 : v2 : vs)
  ) :: ValType -> TupleValue -> Stateful Haskell

type_name_tuple_values_g = ( \tn vs ->
  type_map_get tn "type_name_tuple_values_g" >>= \case
  FieldAndValTypeList fatl -> case length vs == length fatl of 
    False -> error values_fields_lengths_dont_match_err
    True -> correct_type_name_tuple_values_g tn (map get_f_valtype fatl) vs
  CaseAndMaybeValTypeList _ -> undefined
  ) :: TypeName -> [ AbsOpOrOpExpression ] -> Stateful Haskell

tuple_types_and_values_g = ( \vts vs -> case length vts == length vs of
  False -> error tuple_values_types_lengths_dont_match_err
  True -> 
    zipWith abs_op_expr_or_op_expr_g vts vs==>sequence >>= \vs_g ->
    return $ "(" ++ intercalate ", " vs_g ++ ")"
  ) :: [ ValType ] -> [ AbsOpOrOpExpression ] -> Stateful Haskell

correct_type_name_tuple_values_g = ( \tn vts vs ->
  get_indent_level >>= \il ->
  zipWith abs_op_expr_or_op_expr_g vts vs==>sequence >>= \vs_g -> 
  return $
    "\n" ++ indent (il + 1) ++ show tn ++ "C" ++
    concatMap (\v_g ->  " (" ++ v_g ++ ")") vs_g
  ) :: TypeName -> [ ValType ] -> [ AbsOpOrOpExpression ] ->
       Stateful Haskell

tuple_value_type_inference_g = ( \(Values v1 v2 vs) ->
  mapM abs_op_expr_or_op_expr_type_inference_g (v1 : v2 : vs) >>=
  unzip .> \(vts, vs_g) -> case vts of
    vt1 : vt2 : vts ->
      return (TupleValType vt1 vt2 vts, "(" ++ intercalate ", " vs_g ++ ")")
    _ -> undefined
  ) :: TupleValue -> Stateful (ValType, Haskell)

-- MathApplication:
-- math_application_g, math_application_type_inference_g

math_application_g = ( \vt -> 
  math_app_to_app_tree .> application_tree_g vt
  ) :: ValType -> MathApplication -> Stateful Haskell

math_application_type_inference_g = ( 
  math_app_to_app_tree .> application_tree_type_inference_g
  ) :: MathApplication -> Stateful (ValType, Haskell)

-- BaseValue: base_value_g, base_value_type_inference_g

base_value_g = ( \vt -> \case
  ParenthesisValue parenthesis_value -> parenthesis_value_g vt parenthesis_value
  TupleValue tuple_value -> tuple_value_g vt tuple_value
  Literal literal -> literal_g vt literal
  ValueName value_name -> value_name_g vt value_name
  MathApplication math_app -> math_application_g vt math_app
  ) :: ValType -> BaseValue -> Stateful Haskell

base_value_type_inference_g = ( \case
  ParenthesisValue par_val -> parenthesis_value_type_inference_g par_val
  TupleValue tuple_value -> tuple_value_type_inference_g tuple_value
  Literal literal -> literal_type_inference_g literal
  ValueName value_name -> value_name_type_inference_g value_name
  MathApplication math_app -> math_application_type_inference_g math_app
  ) :: BaseValue -> Stateful (ValType, Haskell)

-- FunctionApplicationChain:
-- function_application_chain_g, application_tree_g,
-- application_tree_type_inference_g

function_application_chain_g = ( \vt ->
  fac_to_app_tree .> application_tree_g vt
  ) :: ValType -> FunctionApplicationChain -> Stateful Haskell

application_tree_g = ( \vt -> \case 
  BaseValueLeaf bv -> base_value_g vt bv
  at ->
    application_tree_type_inference_g at >>= \(at_vt, at_hs) ->
    ts_are_equivalent vt at_vt >>= \case 
      True -> return at_hs
      False ->
        value_map_get (VN "gcd") >>= \t ->
        error $
        "\ngiven type: " ++ show vt ++
        "\ninferred type: " ++ show at_vt ++
        "\napp tree: " ++ show at
  ) :: ValType -> ApplicationTree -> Stateful Haskell

application_tree_type_inference_g = ( \case 
  Application at1 at2 -> application_tree_type_inference_g at1 >>=
    \(vt1, hs1) -> case vt1 of 
      FuncType input_t output_t -> 
        application_tree_g input_t at2 >>= \hs2 ->
        let 
        hs2_ = case at2 of 
          Application _ _ -> "(" ++ hs2 ++ ")"
          _ -> hs2
        in
        return (output_t, hs1 ++ " " ++ hs2_) 
      _ ->
        error $ "\n" ++ show at1 ++ "\n" ++ show at2 ++ "\n" ++ show vt1 ++ "\n"
  BaseValueLeaf bv -> base_value_type_inference_g bv
  ) :: ApplicationTree -> Stateful (ValType, Haskell)

-- MultiplicationFactor: multiplication_factor_g

multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> function_application_chain_g vt oaas
  BaseValueMF bv -> base_value_g vt bv
  ) :: ValType -> MultiplicationFactor -> Stateful Haskell

-- Multiplication: multiplication_g

multiplication_g = ( \vt (Mul mf1 mf2 mfs) -> 
  mapM (multiplication_factor_g vt) (mf1 : mf2 : mfs) >>=
  intercalate " * " .> return
  ) :: ValType -> Multiplication -> Stateful Haskell

-- SubtractionFactor: subtraction_factor_g

subtraction_factor_g = ( \vt -> \case
  MulSF m -> multiplication_g vt m
  MFSF f -> multiplication_factor_g vt f
  ) :: ValType -> SubtractionFactor -> Stateful Haskell

-- Subtraction: subtraction_g

subtraction_g = ( \vt (Sub sf1 sf2) ->
  subtraction_factor_g vt sf1 >>= \sf1_g ->
  subtraction_factor_g vt sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: ValType -> Subtraction -> Stateful Haskell

-- EqualityFactor: equality_factor_g

equality_factor_g = ( \vt -> \case
  SubEF s -> subtraction_g vt s
  SFEF f -> subtraction_factor_g vt f
  ) :: ValType -> EqualityFactor -> Stateful Haskell

-- Equality: equality_g

equality_g = ( \case 
  (NamedType (TN "Bool")) -> \(Equ ef1 ef2) ->
    let int_vt = (NamedType (TN "Int")) in
    equality_factor_g int_vt ef1 >>= \ef1_g ->
    equality_factor_g int_vt ef2 >>= \ef2_g ->
    return $ ef1_g ++ " == " ++ ef2_g
  _ -> undefined
  ) :: ValType -> Equality -> Stateful Haskell

-- OperatorExpression:
-- operator_expression_g, operator_expression_type_inference_g

operator_expression_g = ( \vt -> \case
  Equality equ -> equality_g vt equ
  EquF f -> equality_factor_g vt f
  ) :: ValType -> OperatorExpression -> Stateful Haskell

operator_expression_type_inference_g = ( \case
  Equality equ -> equality_g vt equ >>= \hs -> return (vt, hs) where
    vt = NamedType $ TN "Bool"
      :: ValType

  EquF f -> equality_factor_g vt f >>= \hs -> return (vt, hs) where
    vt = NamedType $ TN "Int"
      :: ValType
  ) :: OperatorExpression -> Stateful (ValType, Haskell)

-- AbstractionOpExpression:
-- abstraction_operator_expr_g, abstraction_op_expr_type_inference_g

abstraction_operator_expr_g = ( \case
  FuncType input_t output_t ->
    \(AbstractionAndOpResult abstraction op_expr) ->
      undefined >>
      return $ "\\" ++ undefined ++ " -> " ++ undefined
  _ -> undefined
  ) :: ValType -> AbstractionOpExpression -> Stateful Haskell

abs_op_expr_type_inference_g = ( \(AbstractionAndOpResult as opval) ->
  undefined
  ) :: AbstractionOpExpression -> Stateful (ValType, Haskell)

-- AbsOpOrOpExpression:
-- abstraction_operator_expr_g, abstraction_op_expr_type_inference_g

abs_op_expr_or_op_expr_g = ( \vt -> \case
  AbstractionOpExpression abs_op_expr -> abstraction_operator_expr_g vt abs_op_expr
  OperatorExpression op_expr -> operator_expression_g vt op_expr
  ) :: ValType -> AbsOpOrOpExpression -> Stateful Haskell

abs_op_expr_or_op_expr_type_inference_g = ( \case
  AbstractionOpExpression abs_op_expr -> abs_op_expr_type_inference_g abs_op_expr
  OperatorExpression op_expr -> operator_expression_type_inference_g op_expr
  ) :: AbsOpOrOpExpression -> Stateful (ValType, Haskell)

-- LiteralOrValueName:
-- literal_or_value_name_g, lit_or_val_name_type_inference_g

literal_or_value_name_g = ( \vt -> \case
  Lit literal -> literal_g vt literal
  ValName value_name -> value_name_g vt value_name
  ) :: ValType -> LiteralOrValueName -> Stateful Haskell

lit_or_val_name_type_inference_g = ( \case
  Lit literal -> literal_type_inference_g literal
  ValName value_name -> value_name_type_inference_g value_name
  ) :: LiteralOrValueName -> Stateful (ValType, Haskell)

-- SpecificCase: specific_case_g, abs_g, specific_case_type_inference_g
specific_case_g = ( \vt sc@(SC lovn v) -> case vt of 
  FuncType input_t output_t -> case lovn of 
    Lit l -> literal_g input_t l >>= add_value_g 
    ValName vn -> value_map_insert vn input_t >> add_value_g (show vn)
    where
    add_value_g = ( \g ->
      value_expression_g output_t v >>= \v_g ->
      get_indent_level >>= \i ->
      return $ indent i ++ abs_g g ++ " -> " ++ v_g
      ) :: Haskell -> Stateful Haskell

  _ -> undefined
  ) :: ValType -> SpecificCase -> Stateful Haskell

abs_g = \case 
  "true" -> "True"
  "false" -> "False"
  g -> g
  :: String -> Haskell

specific_case_type_inference_g = ( \sc@(SC lovn v) ->
  lit_or_val_name_type_inference_g lovn >>= \(lovn_vt, lovn_g) ->
  value_expression_type_inference_g v >>= \(v_vt, v_g) ->
  get_indent_level >>= \i ->
  return (FuncType lovn_vt v_vt, indent i ++ lovn_g ++ " -> " ++ v_g)
  ) :: SpecificCase -> Stateful (ValType, Haskell)

-- DefaultCase: specific_case_g, specific_case_type_inference_g
default_case_g = ( \vt dc@(DC v) -> case vt of 
  FuncType input_t output_t -> 
    value_map_insert (VN "value") input_t >>
    value_expression_g output_t v >>= \v_g ->
    get_indent_level >>= \i ->
    return $ indent i ++ "value -> " ++ v_g
  _ -> undefined
  ) :: ValType -> DefaultCase -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g
cases_g = ( \vt -> \case
  OneAndDefault sc dc ->
    get_indent_level >>= \i ->
    update_indent_level (i + 1) >>
    specific_case_g vt sc >>= \sc_g ->
    default_case_g vt dc >>= \dc_g ->
    update_indent_level i >>
    return ("\\case\n" ++ sc_g ++ "\n" ++ dc_g)

  Many sc1 sc2 scs mdc ->
    get_indent_level >>= \i ->
    update_indent_level (i + 1) >>
    mapM (specific_case_g vt) (sc1 : sc2 : scs) >>= \scs_g ->
    update_indent_level i >>
    return ("\\case\n" ++ intercalate "\n" scs_g)
  ) :: ValType -> Cases -> Stateful Haskell

cases_type_inference_g = ( \case
  OneAndDefault sc dc -> undefined
  Many sc1 sc2 scs mdc ->
    get_indent_level >>= \i ->
    update_indent_level (i + 1) >>
    specific_case_type_inference_g sc1 >>= \(vt, c_g) ->
    mapM (specific_case_g vt) (sc2 : scs) >>= \scs_g ->
    update_indent_level i >>
    return (vt, "\\case\n" ++ c_g ++ "\n" ++ intercalate "\n" scs_g)
  ) :: Cases -> Stateful (ValType, Haskell)

-- NameTypeAndValue: name_type_and_value_g

name_type_and_value_g = ( \(NTAV vn vt v) -> 
  value_expression_g (value_type_to_val_type vt) v >>= \v_g ->
  get_indent_level >>= \i ->
  return $
    "\n" ++ indent i ++ show vn ++ " :: " ++ value_type_g vt ++ "\n" ++
    indent i ++ show vn ++ " = " ++ v_g ++ "\n"
  ) :: NameTypeAndValue -> Stateful Haskell

ntav_map_insert = ( \(NTAV vn vt v) ->
  value_map_insert vn $ value_type_to_val_type vt
  ) :: NameTypeAndValue -> Stateful ()

-- NameTypeAndValueLists: name_type_and_value_lists_g

name_type_and_value_lists_g =  
  ntav_lists_to_list_of_ntavs .> mapM name_type_and_value_g >=> concat .> return
  :: NameTypeAndValueLists -> Stateful Haskell

ntav_lists_to_list_of_ntavs = ( \ntav_lists -> case ntav_lists of
  NTAVLists (vn : vns) (vt : vts) (v : vs) ->
    NTAV vn vt v : ntav_lists_to_list_of_ntavs (NTAVLists vns vts vs)
  NTAVLists [] [] [] -> []
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: NameTypeAndValueLists -> [ NameTypeAndValue ]

-- NTAVOrNTAVLists: ntav_or_ntav_lists_g

ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue ntav -> name_type_and_value_g ntav
  NameTypeAndValueLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

ntav_or_ntav_lists_to_list_of_ntavs = ( \case 
  NameTypeAndValue ntav -> [ ntav ]
  NameTypeAndValueLists ntav_lists -> ntav_lists_to_list_of_ntavs ntav_lists
  ) :: NTAVOrNTAVLists -> [ NameTypeAndValue ]

-- NamesTypesAndValues: names_types_and_values_g

names_types_and_values_g = ( ns_ts_and_vs_to_list_of_ntavs .> list_of_ntavs_g)
  :: NamesTypesAndValues -> Stateful Haskell

ns_ts_and_vs_to_list_of_ntavs = ( \(NTAVs ntavs) ->
  concatMap ntav_or_ntav_lists_to_list_of_ntavs ntavs
  ) :: NamesTypesAndValues -> [ NameTypeAndValue ]

list_of_ntavs_g = ( \ntavs_l ->
  mapM_ ntav_map_insert ntavs_l >>
  ntavs_l ==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ NameTypeAndValue ] -> Stateful Haskell

-- Where: where_g, where_type_inference_g 

where_g = ( \vt (Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_expression_g vt v >>= \v_g ->
  return ("\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: ValType -> Where -> Stateful Haskell

where_type_inference_g = ( \(Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_expression_type_inference_g v >>= \(vt, v_g) ->
  return (vt, "\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: Where -> Stateful (ValType, Haskell)

-- CasesOrWhere: cases_or_where_g, cases_or_where_type_inference_g

cases_or_where_g = ( \vt -> \case
  Cases cases -> cases_g vt cases
  Where where_ -> where_g vt where_
  ) :: ValType -> CasesOrWhere -> Stateful Haskell

cases_or_where_type_inference_g = ( \case
  Cases cases -> cases_type_inference_g cases
  Where where_ -> where_type_inference_g where_
  ) :: CasesOrWhere -> Stateful (ValType, Haskell)

-- AbstractionCasesOrWhere: abstraction_cases_or_where_g

abstraction_cases_or_where_g = ( \case
  FuncType input_t output_t ->
    \(AbstractionAndCOWResult abstraction cases_or_where) ->
    abstraction_g input_t abstraction >>= \abs_g ->
    cases_or_where_g output_t cases_or_where >>= \cow_g ->
    return $ "\\" ++ abs_g ++ " -> " ++ cow_g
  _ -> undefined
  ) :: ValType -> AbstractionCasesOrWhere -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: AbstractionCasesOrWhere -> Stateful (ValType, Haskell)

-- ValueExpression: value_expression_g, value_expression_type_inference_g
value_expression_g = ( \vt -> \case
  AbstractionCasesOrWhere abs_cow -> abstraction_cases_or_where_g vt abs_cow
  CasesOrWhere cases_or_where -> cases_or_where_g vt cases_or_where
  AbsOpOrOpExpression expr -> abs_op_expr_or_op_expr_g vt expr
  ) :: ValType -> ValueExpression -> Stateful Haskell

value_expression_type_inference_g = ( \case
  AbstractionCasesOrWhere abs_cow -> abstraction_cow_type_inference_g abs_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  AbsOpOrOpExpression expr -> abs_op_expr_or_op_expr_type_inference_g expr
  ) :: ValueExpression -> Stateful (ValType, Haskell)

