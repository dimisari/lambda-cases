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
  ( ValType(..), ApplicationTree(..), to_application_tree, ValFieldsOrCases(..)
  , FieldAndValType(..), value_type_to_val_type )
import HaskellTypes.Generation
  ( Stateful, get_indent_level, update_indent_level, value_map_get
  , value_map_insert, type_map_get )

import CodeGenerators.ErrorMessages
import CodeGenerators.LowLevel
  ( literal_g, abstraction_g )
import CodeGenerators.TypeChecking
  ( ts_are_equivalent )
import CodeGenerators.Types
  ( value_type_g )


-- All:
-- ParenthesisValue, TupleValue, MathApplication, BaseValue
-- FunctionApplicationChain,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, AbstractionOperatorExpression
-- operator_expression_g, abstraction_operator_expression_g, ManyArgsApplication,
-- SpecificCase, Cases,
-- name_type_and_value_g, name_type_and_value_lists_g,
-- ntav_or_ntav_lists_g, names_types_and_values_g, Where,
-- CasesOrWhere, ValueExpression

-- ParenthesisValue: parenthesis_value_g, parenthesis_value_type_inference_g

parenthesis_value_g = ( \vt (Parenthesis v) ->
  value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"
  ) :: ValType -> ParenthesisValue -> Stateful Haskell

parenthesis_value_type_inference_g = ( \(Parenthesis v) ->
  value_type_inference_g v >>= \(vt, hs) -> return (vt, "(" ++ hs ++ ")")
  ) :: ParenthesisValue -> Stateful (ValType, Haskell)

-- TupleValue:
-- tuple_value_g, type_name_tuple_values_g, tuple_types_and_values_g,
-- correct_type_name_tuple_values_g, tuple_value_type_inference_g

tuple_value_g = ( \vt (Values v1 v2 vs) -> case vt of 
  FunctionType _ _ -> undefined
  NamedType tn -> type_name_tuple_values_g tn
  TupleValType vt1 vt2 vts ->
    tuple_types_and_values_g (vt1 : vt2 : vts) (v1 : v2 : vs)
  ) :: ValType -> TupleValue -> Stateful Haskell

type_name_tuple_values_g = ( \tn vs -> type_map_get tn >>= \case
  FieldAndValTypeList fatl -> case length vs == length fatl of 
    False -> error values_fields_lengths_dont_match_err
    True -> correct_type_name_tuple_values_g tn (map get_f_valtype fatl) vs
  CaseAndMaybeValTypeList _ -> undefined
  ) :: TypeName -> [ AbstractionOperatorExpression ] -> Stateful Haskell

tuple_types_and_values_g = ( \vts vs -> case length vts == length vs of
  False -> error tuple_values_types_lengths_dont_match_err
  True -> 
    zipWith abstraction_operator_expression_g vts vs==>sequence >>= \vs_g ->
    return $ "(" ++ intercalate ", " vs_g ++ ")"
  ) :: [ ValType ] -> [ AbstractionOperatorExpression ] -> Stateful Haskell

correct_type_name_tuple_values_g = ( \tn vts vs ->
  get_indent_level >>= \il ->
  zipWith abstraction_operator_expression_g vts vs==>sequence >>= \vs_g -> 
  return $
    "\n" ++ indent (il + 1) ++ show tn ++ "C" ++
    concatMap (\v_g ->  " (" ++ v_g ++ ")") vs_g
  ) :: TypeName -> [ ValType ] -> [ AbstractionOperatorExpression ] ->
       Stateful Haskell

tuple_value_type_inference_g = ( \(Values v1 v2 vs) ->
  mapM abstraction_op_expr_type_inference_g (v1 : v2 : vs) >>=
  unzip .> \(vts, vs_g) -> case vts of
    vt1 : vt2 : vts ->
      return (TupleValType vt1 vt2 vts, "(" ++ intercalate ", " vs_g ++ ")")
    _ -> undefined
  ) :: TupleValue -> Stateful (ValType, Haskell)

-- MathApplication:
-- math_application_g, math_application_type_inference_g

math_application_g = ( \vt (MathApp vn lov1 lovs) -> case lovs of 
  [] -> undefined
  lov2 : rest ->
    many_args_application_g vt (MAA lov1 lov2 rest vn) >>= \hs ->
    return $ "(" ++ hs ++ ")"
  ) :: ValType -> MathApplication -> Stateful Haskell

math_application_type_inference_g = ( \(MathApp vn lov1 lovs) -> case lovs of 
  [] -> undefined
  lov2 : rest ->
    many_args_application_type_inference_g (MAA lov1 lov2 rest vn) >>=
    \(vt, hs) -> return (vt, "(" ++ hs ++ ")")
  ) :: MathApplication -> Stateful (ValType, Haskell)

-- BaseValue: base_value_g, base_value_type_inference_g

base_value_g = ( \vt -> \case
  ParenthesisValue parenthesis_value -> parenthesis_value_g vt parenthesis_value
  Literal literal -> literal_g vt literal
  ValueName value_name -> value_name_g vt value_name
  MathApplication ma -> math_application_g vt ma
  ) :: ValType -> BaseValue -> Stateful Haskell

base_value_type_inference_g = ( \case
  ParenthesisValue pv -> parenthesis_value_type_inference_g pv
  Literal literal -> literal_type_inference_g vt literal
  ValueName value_name -> literal_or_value_name_type_inference_g lovn
  MathApplication ma -> math_application_type_inference_g ma
  ) :: BaseValue -> Stateful (ValType, Haskell)

-- FunctionApplicationChain:
-- function_application_chain_g, application_tree_g,
-- application_tree_type_inference_g

function_application_chain_g = ( \vt ->
  to_application_tree .> application_tree_g vt
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
      FunctionType in_vt out_vt -> 
        application_tree_g in_vt at2 >>= \hs2 ->
        let 
        hs2_ = case at2 of 
          Application _ _ -> "(" ++ hs2 ++ ")"
          _ -> hs2
        in
        return (out_vt, hs1 ++ " " ++ hs2_) 
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

-- AbstractionOperatorExpression:
-- abstraction_operator_expression_g, abstraction_op_expr_type_inference_g

abstraction_operator_expression_g = ( \vt (AbstractionAndOpResult as opval) ->
  case as of 
    a1 : other_as -> case vt of 
      FunctionType in_vt out_vt ->
        abstraction_g in_vt a1 >>= \a1_g ->
        abstraction_operator_expression_g
          out_vt (AbstractionAndResult other_as opval) >>= \other_g ->
        return $ "\\" ++ a1_g ++ " -> " ++ other_g
      _ -> undefined
    [] -> operator_expression_g vt opval
  ) :: ValType -> AbstractionOperatorExpression -> Stateful Haskell

abstraction_op_expr_type_inference_g = ( \(AbstractionAndOpResult as opval) ->
  undefined
  ) :: AbstractionOperatorExpression -> Stateful (ValType, Haskell)

-- SpecificCase: specific_case_g, specific_case_type_inference_g
specific_case_g = ( \vt sc@(SC lovn v) -> case vt of 
  FunctionType in_vt out_vt -> case lovn of 
    Literal l -> literal_g in_vt l >>= add_value_g 
    ValueName vn -> value_map_insert vn in_vt >> add_value_g (show vn)
    where
    add_value_g = ( \g ->
      value_g out_vt v >>= \v_g ->
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
  literal_or_value_name_type_inference_g lovn >>= \(lovn_vt, lovn_g) ->
  value_type_inference_g v >>= \(v_vt, v_g) ->
  get_indent_level >>= \i ->
  return (FunctionType lovn_vt v_vt, indent i ++ lovn_g ++ " -> " ++ v_g)
  ) :: SpecificCase -> Stateful (ValType, Haskell)

-- DefaultCase: specific_case_g, specific_case_type_inference_g
default_case_g = ( \vt dc@(DC v) -> case vt of 
  FunctionType in_vt out_vt -> 
    value_map_insert (VN "value") in_vt >>
    value_g out_vt v >>= \v_g ->
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
-- Cases end

name_type_and_value_g = ( \(NTAV vn vt v) -> 
  let val_t = value_type_to_val_type vt in
  value_map_insert vn val_t >> value_g val_t v >>= \v_g ->
  get_indent_level >>= \i ->
  return $
  "\n" ++ indent i  ++ show vn ++ " :: " ++ value_type_g vt ++ "\n" ++
  indent i  ++ show vn ++ " = " ++ v_g ++ "\n"
  ) :: NameTypeAndValue -> Stateful Haskell

name_type_and_value_lists_g = ( \ntavls@(NTAVLists vns vts vs) -> 
  let
  zip3 = ( \case
    (vn : vns, vt : vts, v : vs) -> NTAV vn vt v : zip3 (vns, vts, vs)
    ([], [], []) -> []
    _ -> error $ name_type_and_value_lists_err ntavls
    ) :: ([ ValueName ], [ ValueType ], [ ValueExpression ]) ->
         [ NameTypeAndValue ]
  in
  zip3 (vns, vts, vs)==>mapM name_type_and_value_g >>= concat .> return
  ) :: NameTypeAndValueLists -> Stateful Haskell

ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue ntav -> name_type_and_value_g ntav
  NameTypeAndValueLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

names_types_and_values_g = ( \(NTAVs ntavs) ->
  ntavs==>mapM ntav_or_ntav_lists_g >>= concat .> return
  ) :: NamesTypesAndValues -> Stateful Haskell

-- Where: where_g, where_type_inference_g 
where_g = ( \vt (Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_g vt v >>= \v_g ->
  return ("\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: ValType -> Where -> Stateful Haskell

where_type_inference_g = ( \(Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_type_inference_g v >>= \(vt, v_g) ->
  return (vt, "\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: Where -> Stateful (ValType, Haskell)

-- CasesOrWhere: cases_or_where_g, cases_or_where_type_inference_g
cases_or_where_g = ( \vt -> \case
  Cases cs -> cases_g vt cs
  Where w -> where_g vt w
  ) :: ValType -> CasesOrWhere -> Stateful Haskell

cases_or_where_type_inference_g = ( \case
  OperatorExpression opval -> operator_expression_type_inference_g opval
  Cases cs -> cases_type_inference_g cs
  ) :: CasesOrWhere -> Stateful (ValType, Haskell)

-- ValueExpression: value_g, value_type_inference_g
value_g = ( \vt lov -> case (vt, lov) of
  (FunctionType in_vt out_vt, AbstractionAndCasesOrWhere (a:as) outval) ->
    abstraction_g in_vt a >>= \a_g ->
    value_g out_vt (AbstractionAndCasesOrWhere as outval) >>= \other_g ->
    return $ "\\" ++ a_g ++ " -> " ++ other_g
  (_, AbstractionAndCasesOrWhere [] outval) -> cases_or_where_g vt outval
  _ -> undefined
  ) :: ValType -> ValueExpression -> Stateful Haskell

value_type_inference_g = ( \case
  (AbstractionAndCasesOrWhere [] outval) -> cases_or_where_type_inference_g outval
  _ -> undefined
  ) :: ValueExpression -> Stateful (ValType, Haskell)

