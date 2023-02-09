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
  ( LiteralOrValueName(..), ValueName(..), Abstraction(..) )
import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..)
  , FieldsOrCases(..), bt_to_vt, vt_to_bt )
import HaskellTypes.Values
import HaskellTypes.AfterParsing
  ( ValType(..), ApplicationTree(..), to_application_tree, to_application_tree
  , ValFieldsOrCases(..), FieldAndValType(..), value_type_to_val_type )
import HaskellTypes.Generation
  ( Stateful, get_indent_level, update_indent_level, type_map_get
  , val_type_map_get, value_map_get, val_map_get, value_map_insert
  , val_map_insert )

import CodeGenerators.ErrorMessages
import CodeGenerators.LowLevel
  ( literal_g, val_literal_g, literal_type_inference_g, literal_or_value_name_g
  , literal_or_value_name_type_inference_g, abstractions_g, val_abstraction_g
  , vts_are_equivalent, val_ts_are_equivalent, val_literal_or_value_name_g
  , val_literal_or_value_name_type_inference_g )
import CodeGenerators.Types
  ( value_type_g )


-- All:
-- ParenthesisValue, MathApplication, BaseValue, OneArgApplications,
-- multiplication_factor_g, multiplication_g, subtraction_factor_g, subtraction_g,
-- equality_factor_g, equality_g
-- operator_value_g, lambda_operator_value_g, ManyArgsApplication,
-- UseFields, SpecificCase, Cases,
-- name_type_and_value_g, name_type_and_value_lists_g,
-- ntav_or_ntav_lists_g, names_types_and_values_g, Where,
-- OutputValue, LambdaOutputValue

-- ParenthesisValue:
-- parenthesis_value_g, value_type_tuple_values_g, base_type_tuple_values_g, 
-- value_types_tuple_values_g, type_name_tuple_values_g,
-- correct_type_name_tuple_values_g   
-- parenthesis_value_type_inference_g, tuple_values_type_inference_g
parenthesis_value_g = ( \vt -> \case
  Parenthesis v -> value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"
  Tuple vs -> value_type_tuple_values_g vt vs
  ) :: ValueType -> ParenthesisValue -> Stateful Haskell

value_type_tuple_values_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> \vs -> error $ tuple_fun_type_err vs vt
  AbsTypesAndResType [] bt -> base_type_tuple_values_g bt
  ) :: ValueType -> [ LambdaOperatorValue ] -> Stateful Haskell

base_type_tuple_values_g = ( \case
  TypeName tn -> type_name_tuple_values_g tn
  ParenType vt -> value_type_tuple_values_g vt
  TupleType vt1 vt2 vts -> value_types_tuple_values_g $ vt1 : vt2 : vts
  ) :: BaseType -> [ LambdaOperatorValue ] -> Stateful Haskell

value_types_tuple_values_g = ( \vts vs -> case length vts == length vs of
  False -> error tuple_values_types_lengths_dont_match_err
  True -> 
    zipWith lambda_operator_value_g vts vs==>sequence >>= \vs_g ->
    return $ "(" ++ intercalate ", " vs_g ++ ")"
  ) :: [ ValueType ] -> [ LambdaOperatorValue ] -> Stateful Haskell

type_name_tuple_values_g = ( \tn vs -> type_map_get tn >>= \case
  FieldAndTypeList fatl -> case length vs == length fatl of 
    False -> error values_fields_lengths_dont_match_err
    True -> correct_type_name_tuple_values_g tn (map get_field_type fatl) vs
  CaseAndMaybeTypeList camtl -> undefined camtl
  ) :: TypeName -> [ LambdaOperatorValue ] -> Stateful Haskell

correct_type_name_tuple_values_g = ( \tn vts vs ->
  get_indent_level >>= \il ->
  zipWith lambda_operator_value_g vts vs==>sequence >>= \vs_g -> 
  return $
    "\n" ++ indent (il + 1) ++ show tn ++ "C" ++
    concatMap (\v_g ->  " (" ++ v_g ++ ")") vs_g
  ) :: TypeName -> [ ValueType ] -> [ LambdaOperatorValue ] -> Stateful Haskell

parenthesis_value_type_inference_g = ( \case
  Parenthesis v ->
    value_type_inference_g v >>= \(vt, hs) -> return (vt, "(" ++ hs ++ ")")
  Tuple vs ->
    tuple_values_type_inference_g vs
  ) :: ParenthesisValue -> Stateful (ValueType, Haskell)

tuple_values_type_inference_g = ( \vs ->
  mapM lambda_operator_value_type_inference_g vs >>=
  unzip .> \(vts, vs_g) -> case vts of
    vt1 : vt2 : vts ->
      return
      ( AbsTypesAndResType [] $ TupleType vt1 vt2 vts
      , "(" ++ intercalate ", " vs_g ++ ")"
      )
    _ -> undefined
  ) :: [ LambdaOperatorValue ] -> Stateful (ValueType, Haskell)

-- (ValType) ParenthesisValue:
-- val_parenthesis_value_g, val_value_type_tuple_values_g, 
-- val_value_types_tuple_values_g, val_type_name_tuple_values_g,
-- val_correct_type_name_tuple_values_g   
-- val_parenthesis_value_type_inference_g, val_tuple_values_type_inference_g
val_parenthesis_value_g = ( \vt -> \case
  Parenthesis v -> val_value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"
  Tuple vs -> val_value_type_tuple_values_g vt vs
  ) :: ValType -> ParenthesisValue -> Stateful Haskell

val_value_type_tuple_values_g = ( \case 
  FunctionType _ _ -> undefined
  NamedType tn -> val_type_name_tuple_values_g tn
  TupleValType vt1 vt2 vts -> val_value_types_tuple_values_g $ vt1 : vt2 : vts
  ) :: ValType -> [ LambdaOperatorValue ] -> Stateful Haskell

val_value_types_tuple_values_g = ( \vts vs -> case length vts == length vs of
  False -> error tuple_values_types_lengths_dont_match_err
  True -> 
    zipWith val_lambda_operator_value_g vts vs==>sequence >>= \vs_g ->
    return $ "(" ++ intercalate ", " vs_g ++ ")"
  ) :: [ ValType ] -> [ LambdaOperatorValue ] -> Stateful Haskell

val_type_name_tuple_values_g = ( \tn vs -> val_type_map_get tn >>= \case
  FieldAndValTypeList fatl -> case length vs == length fatl of 
    False -> error values_fields_lengths_dont_match_err
    True -> val_correct_type_name_tuple_values_g tn (map get_f_valtype fatl) vs
  CaseAndMaybeValTypeList camtl -> undefined camtl
  ) :: TypeName -> [ LambdaOperatorValue ] -> Stateful Haskell

val_correct_type_name_tuple_values_g = ( \tn vts vs ->
  get_indent_level >>= \il ->
  zipWith val_lambda_operator_value_g vts vs==>sequence >>= \vs_g -> 
  return $
    "\n" ++ indent (il + 1) ++ show tn ++ "C" ++
    concatMap (\v_g ->  " (" ++ v_g ++ ")") vs_g
  ) :: TypeName -> [ ValType ] -> [ LambdaOperatorValue ] -> Stateful Haskell

val_parenthesis_value_type_inference_g = ( \case
  Parenthesis v ->
    val_value_type_inference_g v >>= \(vt, hs) -> return (vt, "(" ++ hs ++ ")")
  Tuple vs ->
    val_tuple_values_type_inference_g vs
  ) :: ParenthesisValue -> Stateful (ValType, Haskell)

val_tuple_values_type_inference_g = ( \vs ->
  mapM val_lambda_operator_value_type_inference_g vs >>=
  unzip .> \(vts, vs_g) -> case vts of
    vt1 : vt2 : vts ->
      return (TupleValType vt1 vt2 vts, "(" ++ intercalate ", " vs_g ++ ")")
    _ -> undefined
  ) :: [ LambdaOperatorValue ] -> Stateful (ValType, Haskell)

-- MathApplication: math_application_g, math_application_type_inference_g
math_application_g = ( \vt (MathApp vn lov1 lovs) ->
  many_args_application_g vt (MAA (lov1 : lovs) vn) >>= \hs ->
  return $ "(" ++ hs ++ ")"
  ) :: ValueType -> MathApplication -> Stateful Haskell

math_application_type_inference_g = ( \(MathApp vn lov1 lovs) ->
  many_args_application_type_inference_g (MAA (lov1 : lovs) vn) >>= \(vt, hs) ->
  return (vt, "(" ++ hs ++ ")")
  ) :: MathApplication -> Stateful (ValueType, Haskell)

-- (ValType) MathApplication:
-- val_math_application_g, val_math_application_type_inference_g
val_math_application_g = ( \vt (MathApp vn lov1 lovs) ->
  val_many_args_application_g vt (MAA (lov1 : lovs) vn) >>= \hs ->
  return $ "(" ++ hs ++ ")"
  ) :: ValType -> MathApplication -> Stateful Haskell

val_math_application_type_inference_g = ( \(MathApp vn lov1 lovs) ->
  val_many_args_application_type_inference_g (MAA (lov1 : lovs) vn) >>=
  \(vt, hs) -> return (vt, "(" ++ hs ++ ")")
  ) :: MathApplication -> Stateful (ValType, Haskell)

-- BaseValue: base_value_g, base_value_type_inference_g
base_value_g = ( \vt -> \case
  ParenthesisValue pv -> parenthesis_value_g vt pv
  LiteralOrValueName lovn -> literal_or_value_name_g vt lovn
  MathApplication ma -> math_application_g vt ma
  ) :: ValueType -> BaseValue -> Stateful Haskell

base_value_type_inference_g = ( \case
  ParenthesisValue pv -> parenthesis_value_type_inference_g pv
  LiteralOrValueName lovn -> literal_or_value_name_type_inference_g lovn
  MathApplication ma -> math_application_type_inference_g ma
  ) :: BaseValue -> Stateful (ValueType, Haskell)

-- (ValType) BaseValue: val_base_value_g, val_base_value_type_inference_g
val_base_value_g = ( \vt -> \case
  ParenthesisValue pv -> val_parenthesis_value_g vt pv
  LiteralOrValueName lovn -> val_literal_or_value_name_g vt lovn
  MathApplication ma -> val_math_application_g vt ma
  ) :: ValType -> BaseValue -> Stateful Haskell

val_base_value_type_inference_g = ( \case
  ParenthesisValue pv -> val_parenthesis_value_type_inference_g pv
  LiteralOrValueName lovn -> val_literal_or_value_name_type_inference_g lovn
  MathApplication ma -> val_math_application_type_inference_g ma
  ) :: BaseValue -> Stateful (ValType, Haskell)

-- OneArgApplications:
-- one_arg_applications_g, application_tree_g, application_tree_type_inference_g
one_arg_applications_g = ( \vt ->
  to_application_tree .> application_tree_g vt
  ) :: ValueType -> OneArgApplications -> Stateful Haskell

application_tree_g = ( \vt -> \case 
  BaseValueLeaf bv -> base_value_g vt bv
  at ->
    application_tree_type_inference_g at >>= \(at_vt, at_hs) ->
    vts_are_equivalent vt at_vt >>= \case 
      True -> return at_hs
      False -> error $ "\n" ++ show vt ++ "\n" ++ show at_vt ++ "\n" ++ show at
  ) :: ValueType -> ApplicationTree -> Stateful Haskell

application_tree_type_inference_g = ( \case 
  Application at1 at2 -> application_tree_type_inference_g at1 >>=
    \(vt1@(AbsTypesAndResType abs_ts res_t), hs1) -> case abs_ts of 
      [] ->
        error $ "\n" ++ show at1 ++ "\n" ++ show at2 ++ "\n" ++ show vt1 ++ "\n"
      abs_t:rest -> 
        application_tree_g (bt_to_vt abs_t) at2 >>= \hs2 ->
        let 
        hs2_ = case at2 of 
          Application _ _ -> "(" ++ hs2 ++ ")"
          _ -> hs2
        in
        return (AbsTypesAndResType rest res_t, hs1 ++ " " ++ hs2_) 
  BaseValueLeaf bv -> base_value_type_inference_g bv
  ) :: ApplicationTree -> Stateful (ValueType, Haskell)

-- (ValType) OneArgApplications:
-- val_one_arg_applications_g, val_application_tree_g,
-- val_application_tree_type_inference_g
val_one_arg_applications_g = ( \vt ->
  to_application_tree .> val_application_tree_g vt
  ) :: ValType -> OneArgApplications -> Stateful Haskell

val_application_tree_g = ( \vt -> \case 
  BaseValueLeaf bv -> val_base_value_g vt bv
  at ->
    val_application_tree_type_inference_g at >>= \(at_vt, at_hs) ->
    val_ts_are_equivalent vt at_vt >>= \case 
      True -> return at_hs
      False -> error $ "\n" ++ show vt ++ "\n" ++ show at_vt ++ "\n" ++ show at
  ) :: ValType -> ApplicationTree -> Stateful Haskell

val_application_tree_type_inference_g = ( \case 
  Application at1 at2 -> val_application_tree_type_inference_g at1 >>=
    \(vt1, hs1) -> case vt1 of 
      FunctionType in_vt out_vt -> 
        val_application_tree_g in_vt at2 >>= \hs2 ->
        let 
        hs2_ = case at2 of 
          Application _ _ -> "(" ++ hs2 ++ ")"
          _ -> hs2
        in
        return (out_vt, hs1 ++ " " ++ hs2_) 
      _ ->
        error $ "\n" ++ show at1 ++ "\n" ++ show at2 ++ "\n" ++ show vt1 ++ "\n"
  BaseValueLeaf bv -> val_base_value_type_inference_g bv
  ) :: ApplicationTree -> Stateful (ValType, Haskell)
-- OneArgApplications end

multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> one_arg_applications_g vt oaas
  BaseValueMF bv -> base_value_g vt bv
  ) :: ValueType -> MultiplicationFactor -> Stateful Haskell

multiplication_g = ( \vt (Mul mf1 mf2 mfs) -> 
  mapM (multiplication_factor_g vt) (mf1 : mf2 : mfs) >>=
  intercalate " * " .> return
  ) :: ValueType -> Multiplication -> Stateful Haskell

subtraction_factor_g = ( \vt -> \case
  MulSF m -> multiplication_g vt m
  MFSF f -> multiplication_factor_g vt f
  ) :: ValueType -> SubtractionFactor -> Stateful Haskell

subtraction_g = ( \vt (Sub sf1 sf2) ->
  subtraction_factor_g vt sf1 >>= \sf1_g ->
  subtraction_factor_g vt sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: ValueType -> Subtraction -> Stateful Haskell

equality_factor_g = ( \vt -> \case
  SubEF s -> subtraction_g vt s
  SFEF f -> subtraction_factor_g vt f
  ) :: ValueType -> EqualityFactor -> Stateful Haskell

equality_g = ( \case 
  (AbsTypesAndResType [] (TypeName (TN "Bool"))) -> \(Equ ef1 ef2) ->
    let int_vt = (AbsTypesAndResType [] (TypeName (TN "Int"))) in
    equality_factor_g int_vt ef1 >>= \ef1_g ->
    equality_factor_g int_vt ef2 >>= \ef2_g ->
    return $ ef1_g ++ " == " ++ ef2_g
  _ -> undefined
  ) :: ValueType -> Equality -> Stateful Haskell

val_multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> val_one_arg_applications_g vt oaas
  BaseValueMF bv -> val_base_value_g vt bv
  ) :: ValType -> MultiplicationFactor -> Stateful Haskell

val_multiplication_g = ( \vt (Mul mf1 mf2 mfs) -> 
  mapM (val_multiplication_factor_g vt) (mf1 : mf2 : mfs) >>=
  intercalate " * " .> return
  ) :: ValType -> Multiplication -> Stateful Haskell

val_subtraction_factor_g = ( \vt -> \case
  MulSF m -> val_multiplication_g vt m
  MFSF f -> val_multiplication_factor_g vt f
  ) :: ValType -> SubtractionFactor -> Stateful Haskell

val_subtraction_g = ( \vt (Sub sf1 sf2) ->
  val_subtraction_factor_g vt sf1 >>= \sf1_g ->
  val_subtraction_factor_g vt sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: ValType -> Subtraction -> Stateful Haskell

val_equality_factor_g = ( \vt -> \case
  SubEF s -> val_subtraction_g vt s
  SFEF f -> val_subtraction_factor_g vt f
  ) :: ValType -> EqualityFactor -> Stateful Haskell

val_equality_g = ( \case 
  (NamedType (TN "Bool")) -> \(Equ ef1 ef2) ->
    let int_vt = (NamedType (TN "Int")) in
    val_equality_factor_g int_vt ef1 >>= \ef1_g ->
    val_equality_factor_g int_vt ef2 >>= \ef2_g ->
    return $ ef1_g ++ " == " ++ ef2_g
  _ -> undefined
  ) :: ValType -> Equality -> Stateful Haskell

-- OperatorValue: operator_value_g, operator_value_type_inference_g
operator_value_g = ( \vt -> \case
  Equality equ -> equality_g vt equ
  EquF f -> equality_factor_g vt f
  ) :: ValueType -> OperatorValue -> Stateful Haskell

operator_value_type_inference_g = ( \case
  Equality equ -> equality_g vt equ >>= \hs -> return (vt, hs) where
    vt = (AbsTypesAndResType [] (TypeName (TN "Bool")))
      :: ValueType

  EquF f -> equality_factor_g vt f >>= \hs -> return (vt, hs) where
    vt = (AbsTypesAndResType [] (TypeName (TN "Int")))
      :: ValueType
  ) :: OperatorValue -> Stateful (ValueType, Haskell)

-- (ValType) OperatorValue:
-- val_operator_value_g, val_operator_value_type_inference_g
val_operator_value_g = ( \vt -> \case
  Equality equ -> val_equality_g vt equ
  EquF f -> val_equality_factor_g vt f
  ) :: ValType -> OperatorValue -> Stateful Haskell

val_operator_value_type_inference_g = ( \case
  Equality equ -> val_equality_g vt equ >>= \hs -> return (vt, hs) where
    vt = NamedType $ TN "Bool"
      :: ValType

  EquF f -> val_equality_factor_g vt f >>= \hs -> return (vt, hs) where
    vt = NamedType $ TN "Int"
      :: ValType
  ) :: OperatorValue -> Stateful (ValType, Haskell)

-- LambdaOperatorValue:
-- lambda_operator_value_g, lambda_operator_value_type_inference_g
lambda_operator_value_g = (
  \(AbsTypesAndResType bts bt) (LOV as opval) -> case length as > length bts of 
  True -> error $ too_many_abstractions_err as bts
  False -> 
    abstractions_g bts1 as >>= \as_g ->
    operator_value_g (AbsTypesAndResType bts2 bt) opval >>= \opval_g ->
    return $ as_g ++ opval_g
    where
    (bts1, bts2) = splitAt (length as) bts
      :: ([ BaseType ], [ BaseType ])
  ) :: ValueType -> LambdaOperatorValue -> Stateful Haskell

lambda_operator_value_type_inference_g = ( \(LOV as opval) ->
  undefined
  ) :: LambdaOperatorValue -> Stateful (ValueType, Haskell)

-- (ValType) LambdaOperatorValue:
-- val_lambda_operator_value_g, val_lambda_operator_value_type_inference_g
val_lambda_operator_value_g = ( \vt (LOV as opval) -> case as of 
  a1 : other_as -> case vt of 
    FunctionType in_vt out_vt ->
      val_abstraction_g in_vt a1 >>= \a1_g ->
      val_lambda_operator_value_g out_vt (LOV other_as opval) >>= \other_g ->
      return $ "\\" ++ a1_g ++ " -> " ++ other_g
    _ -> undefined
  [] -> val_operator_value_g vt opval
  ) :: ValType -> LambdaOperatorValue -> Stateful Haskell

val_lambda_operator_value_type_inference_g = ( \(LOV as opval) ->
  undefined
  ) :: LambdaOperatorValue -> Stateful (ValType, Haskell)

-- ManyArgsApplication:
-- many_args_application_g, bts_lovs_vn_g, bt_lov_g,
-- many_args_application_type_inference_g
many_args_application_g = ( \vt (MAA lovs vn) -> value_map_get vn >>=
  \(AbsTypesAndResType abs_bts res_bt) ->
  let
  (bts1, bts2) = splitAt (length lovs) abs_bts
    :: ([ BaseType ], [ BaseType ])
  in
  vts_are_equivalent vt (AbsTypesAndResType bts2 res_bt) >>= \case
    False -> error $
      many_args_types_dont_match_err vt (AbsTypesAndResType bts2 res_bt)
    True -> bts_lovs_vn_g bts1 lovs vn
  ) :: ValueType -> ManyArgsApplication -> Stateful Haskell

val_many_args_application_g = ( \vt maa -> 
  val_many_args_application_type_inference_g maa >>= \(inference_vt, hs) ->
  val_ts_are_equivalent vt inference_vt >>= \case
    True -> return hs 
    False -> error $ "\n\n" ++ show vt ++ "\n\n" ++ show inference_vt
  ) :: ValType -> ManyArgsApplication -> Stateful Haskell

bts_lovs_vn_g = ( \bts lovs vn ->
  zipWith bt_lov_g bts lovs==>sequence >>= \maavs_g ->
  return $ show vn ++ concatMap (" " ++) maavs_g
  ) :: [ BaseType ] -> [ LambdaOperatorValue ] -> ValueName -> Stateful Haskell

bt_lov_g = ( \bt lov@(LOV as opval) -> case (as, opval) of
  ([], EquF(SFEF(MFSF(BaseValueMF bv)))) ->
    base_value_g (bt==>bt_to_vt) bv
  (_, _) -> 
    lambda_operator_value_g (bt==>bt_to_vt) lov >>= \maav_g ->
    return $ "(" ++ maav_g ++ ")"
  ) :: BaseType -> LambdaOperatorValue -> Stateful Haskell

many_args_application_type_inference_g = ( \(MAA lovs vn) ->
  value_map_get vn >>= \(AbsTypesAndResType abs_bts res_bt) ->
  let
  (bts1, bts2) = splitAt (length lovs) abs_bts
    :: ([ BaseType ], [ BaseType ])
  in
  bts_lovs_vn_g bts1 lovs vn >>= \hs ->
  return (AbsTypesAndResType bts2 res_bt, hs)
  ) :: ManyArgsApplication -> Stateful (ValueType, Haskell)

val_many_args_application_type_inference_g = ( \(MAA lovs vn) ->
  val_map_get vn >>= \vt -> 
  n_in_types_plus_out_type (length lovs) vt ==> \(in_types, out_type) ->
  zipWith val_lambda_operator_value_g in_types lovs ==> sequence >>= \lovs_g ->
  return (out_type, show vn ++ concatMap (\lov_g -> " (" ++ lov_g ++ ")") lovs_g)
  ) :: ManyArgsApplication -> Stateful (ValType, Haskell)

n_in_types_plus_out_type = ( \case 
  0 -> \vt -> ([], vt)
  n -> \case
    FunctionType in_vt out_vt ->
      n_in_types_plus_out_type (n - 1) out_vt ==> \(in_vts, final_out_vt) ->
      (in_vt : in_vts, final_out_vt)
    _ -> undefined
  ) :: Int -> ValType -> ([ ValType ], ValType)

-- UseFields: use_fields_g, correct_use_fields_g, insert_to_value_map_ret_vn
use_fields_g = ( \vt@(AbsTypesAndResType bts bt) (UF v) -> case bts of 
  [] -> error $ use_fields_not_fun_err vt
  bt1:bts_tail -> case bt1 of
    ParenType _ -> error $ must_be_tuple_type_err bt1 -- maybe something here?
    TypeName tn -> type_map_get tn >>= \case
      FieldAndTypeList fatl ->
        correct_use_fields_g tn fatl (AbsTypesAndResType bts_tail bt) v
      _ -> undefined
  ) :: ValueType -> UseFields -> Stateful Haskell

correct_use_fields_g = ( \tn fatl vt v ->
  get_indent_level >>= \il ->
  value_map_insert (VN "value") (AbsTypesAndResType [] (TypeName tn)) >>
  mapM insert_to_value_map_ret_vn fatl >>= \vns ->
  value_g vt v >>= \v_g ->
  return $
  "\\value@(" ++ show tn ++ "C" ++ concatMap (show .> (" " ++)) vns ++ ") -> " ++
  v_g
  ) :: TypeName -> [ FieldAndType ] -> ValueType -> LambdaOutputValue ->
       Stateful Haskell

insert_to_value_map_ret_vn = ( \(FT vn vt) -> value_map_insert vn vt >> return vn )
  :: FieldAndType -> Stateful ValueName

-- (ValType) UseFields:
-- val_use_fields_g, val_correct_use_fields_g, insert_to_val_map_ret_vn
val_use_fields_g = ( \case
  FunctionType in_vt out_vt -> case in_vt of
    NamedType tn -> \(UF v) -> val_type_map_get tn >>= \case
      FieldAndValTypeList fatl -> val_correct_use_fields_g tn fatl out_vt v
      _ -> undefined
    _ -> undefined
  _ -> undefined
  ) :: ValType -> UseFields -> Stateful Haskell

val_correct_use_fields_g = ( \tn fatl vt v ->
  get_indent_level >>= \il ->
  val_map_insert (VN "value") (NamedType tn) >>
  mapM insert_to_val_map_ret_vn fatl >>= \vns ->
  val_value_g vt v >>= \v_g ->
  return $
  "\\value@(" ++ show tn ++ "C" ++ concatMap (show .> (" " ++)) vns ++ ") -> " ++
  v_g
  ) :: TypeName -> [ FieldAndValType ] -> ValType -> LambdaOutputValue ->
       Stateful Haskell

insert_to_val_map_ret_vn = ( \(FVT vn vt) -> val_map_insert vn vt >> return vn )
  :: FieldAndValType -> Stateful ValueName

-- SpecificCase: specific_case_g, specific_case_type_inference_g
specific_case_g = ( \vt@(AbsTypesAndResType bts bt) sc@(SC lovn v) -> case bts of 
  [] -> error $ specific_case_not_abstraction_err vt sc
  b:bs -> case lovn of 
    Literal l -> literal_g bt_vt l >>= add_value_g 
    ValueName vn -> value_map_insert vn bt_vt >> add_value_g (show vn)
    where
    bt_vt = bt_to_vt b
      :: ValueType
    add_value_g = ( \g ->
      value_g (AbsTypesAndResType bs bt) v >>= \v_g ->
      get_indent_level >>= \i ->
      return $ indent i ++ abs_g g ++ " -> " ++ v_g
      ) :: Haskell -> Stateful Haskell
  ) :: ValueType -> SpecificCase -> Stateful Haskell

abs_g = \case 
  "true" -> "True"
  "false" -> "False"
  g -> g
  :: String -> Haskell

specific_case_type_inference_g = ( \sc@(SC lovn v) ->
  literal_or_value_name_type_inference_g lovn >>= \(lovn_vt, lovn_g) ->
  value_type_inference_g v >>= \(AbsTypesAndResType abs res, v_g) ->
  get_indent_level >>= \i ->
  let
  lovn_bt = case lovn_vt of 
    AbsTypesAndResType [] bt -> bt
    _ -> ParenType lovn_vt
  in
  return
  (AbsTypesAndResType (lovn_bt : abs) res, indent i ++ lovn_g ++ " -> " ++ v_g)
  ) :: SpecificCase -> Stateful (ValueType, Haskell)

-- (ValType) SpecificCase: val_specific_case_g, val_specific_case_type_inference_g
val_specific_case_g = ( \vt sc@(SC lovn v) -> case vt of 
  FunctionType in_vt out_vt -> case lovn of 
    Literal l -> val_literal_g in_vt l >>= add_value_g 
    ValueName vn -> val_map_insert vn in_vt >> add_value_g (show vn)
    where
    add_value_g = ( \g ->
      val_value_g out_vt v >>= \v_g ->
      get_indent_level >>= \i ->
      return $ indent i ++ abs_g g ++ " -> " ++ v_g
      ) :: Haskell -> Stateful Haskell

  _ -> undefined
  ) :: ValType -> SpecificCase -> Stateful Haskell

val_specific_case_type_inference_g = ( \sc@(SC lovn v) ->
  val_literal_or_value_name_type_inference_g lovn >>= \(lovn_vt, lovn_g) ->
  val_value_type_inference_g v >>= \(v_vt, v_g) ->
  get_indent_level >>= \i ->
  return (FunctionType lovn_vt v_vt, indent i ++ lovn_g ++ " -> " ++ v_g)
  ) :: SpecificCase -> Stateful (ValType, Haskell)

-- Cases: cases_g, cases_type_inference_g
cases_g = ( \vt (Cs cs) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> mapM (specific_case_g vt) cs >>= \cs_g ->
  update_indent_level i >> return ("\\case\n" ++ intercalate "\n" cs_g)
  ) :: ValueType -> Cases -> Stateful Haskell

cases_type_inference_g = ( \(Cs cs) -> case cs of
  [] -> undefined
  sc:scs ->
    get_indent_level >>= \i -> update_indent_level (i + 1) >>
    specific_case_type_inference_g sc >>= \(vt, sc_g) ->
    mapM (specific_case_g vt) scs >>= \scs_g ->
    update_indent_level i >>
    return (vt, "\\case\n" ++ sc_g ++ "\n" ++ intercalate "\n" scs_g)
  ) :: Cases -> Stateful (ValueType, Haskell)

-- (ValType) Cases: val_cases_g, val_cases_type_inference_g
val_cases_g = ( \vt (Cs cs) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> mapM (val_specific_case_g vt) cs >>= \cs_g ->
  update_indent_level i >> return ("\\case\n" ++ intercalate "\n" cs_g)
  ) :: ValType -> Cases -> Stateful Haskell

val_cases_type_inference_g = ( \(Cs cs) -> case cs of
  [] -> undefined
  sc:scs ->
    get_indent_level >>= \i ->
    update_indent_level (i + 1) >>
    val_specific_case_type_inference_g sc >>= \(vt, sc_g) ->
    mapM (val_specific_case_g vt) scs >>= \scs_g ->
    update_indent_level i >>
    return (vt, "\\case\n" ++ sc_g ++ "\n" ++ intercalate "\n" scs_g)
  ) :: Cases -> Stateful (ValType, Haskell)
-- Cases end

name_type_and_value_g = ( \(NTAV vn vt v) -> 
  value_map_insert vn vt >> value_g vt v >>= \v_g ->
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
    ) :: ([ ValueName ], [ ValueType ], [ LambdaOutputValue ]) ->
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

val_name_type_and_value_g = ( \(NTAV vn vt v) -> 
  let val_vt = value_type_to_val_type vt in
  val_map_insert vn val_vt >> val_value_g val_vt v >>= \v_g ->
  get_indent_level >>= \i ->
  return $
  "\n" ++ indent i  ++ show vn ++ " :: " ++ value_type_g vt ++ "\n" ++
  indent i  ++ show vn ++ " = " ++ v_g ++ "\n"
  ) :: NameTypeAndValue -> Stateful Haskell

val_name_type_and_value_lists_g = ( \ntavls@(NTAVLists vns vts vs) -> 
  let
  zip3 = ( \case
    (vn : vns, vt : vts, v : vs) -> NTAV vn vt v : zip3 (vns, vts, vs)
    ([], [], []) -> []
    _ -> error $ name_type_and_value_lists_err ntavls
    ) :: ([ ValueName ], [ ValueType ], [ LambdaOutputValue ]) ->
         [ NameTypeAndValue ]
  in
  zip3 (vns, vts, vs)==>mapM val_name_type_and_value_g >>= concat .> return
  ) :: NameTypeAndValueLists -> Stateful Haskell

val_ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue ntav -> val_name_type_and_value_g ntav
  NameTypeAndValueLists ntav_lists -> val_name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

val_names_types_and_values_g = ( \(NTAVs ntavs) ->
  ntavs==>mapM val_ntav_or_ntav_lists_g >>= concat .> return
  ) :: NamesTypesAndValues -> Stateful Haskell

-- Where: where_g, where_type_inference_g 
where_g = ( \vt (Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_g vt v >>= \v_g ->
  return ("\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: ValueType -> Where -> Stateful Haskell

where_type_inference_g = ( \(Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_type_inference_g v >>= \(vt, v_g) ->
  return (vt, "\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: Where -> Stateful (ValueType, Haskell)

-- (ValType) Where: val_where_g, val_where_type_inference_g 
val_where_g = ( \vt (Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  val_names_types_and_values_g ntavs >>= \ntavs_g ->
  val_value_g vt v >>= \v_g ->
  return ("\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: ValType -> Where -> Stateful Haskell

val_where_type_inference_g = ( \(Where_ v ntavs) ->
  get_indent_level >>= \i -> update_indent_level (i + 1) >>
  val_names_types_and_values_g ntavs >>= \ntavs_g ->
  val_value_type_inference_g v >>= \(vt, v_g) ->
  return (vt, "\n" ++ indent (i + 1) ++ v_g ++ " where" ++ ntavs_g)
  <* update_indent_level i
  ) :: Where -> Stateful (ValType, Haskell)

-- OutputValue: output_value_g, output_value_type_inference_g
output_value_g = ( \vt -> \case
  ManyArgsApplication maa -> many_args_application_g vt maa
  UseFields uf -> use_fields_g vt uf
  OperatorValue opval -> operator_value_g vt opval
  Cases cs -> cases_g vt cs
  Where w -> where_g vt w
  ) :: ValueType -> OutputValue -> Stateful Haskell

output_value_type_inference_g = ( \case
  ManyArgsApplication maa -> many_args_application_type_inference_g maa

  -- cannot infer unless we try to lookup potential fields in the output value 
  -- and infer backwards
  UseFields uf -> undefined

  OperatorValue opval -> operator_value_type_inference_g opval

  -- could infer by looking up cases that are not "..." (need new map?)
  Cases cs -> cases_type_inference_g cs
  Where w -> where_type_inference_g w 
  ) :: OutputValue -> Stateful (ValueType, Haskell)

-- (ValType) OutputValue: val_output_value_g, val_output_value_type_inference_g
val_output_value_g = ( \vt -> \case
  ManyArgsApplication maa -> val_many_args_application_g vt maa
  UseFields uf -> val_use_fields_g vt uf
  OperatorValue opval -> val_operator_value_g vt opval
  Cases cs -> val_cases_g vt cs
  Where w -> val_where_g vt w
  ) :: ValType -> OutputValue -> Stateful Haskell

val_output_value_type_inference_g = ( \case
  ManyArgsApplication maa -> val_many_args_application_type_inference_g maa
  UseFields uf -> undefined
  OperatorValue opval -> val_operator_value_type_inference_g opval
  Cases cs -> val_cases_type_inference_g cs
  Where w -> val_where_type_inference_g w 
  ) :: OutputValue -> Stateful (ValType, Haskell)

-- LambdaOutputValue: value_g, value_type_inference_g
value_g = ( \(AbsTypesAndResType bts bt) (LV as outval) ->
  let
  (bts1, bts2) = splitAt (length as) bts
    :: ([ BaseType ], [ BaseType ])
  in
  abstractions_g bts1 as >>= \as_g ->
  output_value_g (AbsTypesAndResType bts2 bt) outval >>= \outval_g ->
  return $ as_g ++ outval_g
  ) :: ValueType -> LambdaOutputValue -> Stateful Haskell

value_type_inference_g = ( \case
  (LV [] outval) -> output_value_type_inference_g outval
  _ -> undefined
  ) :: LambdaOutputValue -> Stateful (ValueType, Haskell)

-- (ValType) LambdaOutputValue: val_value_g, val_value_type_inference_g
val_value_g = ( \vt lov -> case (vt, lov) of
  (FunctionType in_vt out_vt, LV (a:as) outval) ->
    val_abstraction_g in_vt a >>= \a_g ->
    val_value_g out_vt (LV as outval) >>= \other_g ->
    return $ "\\" ++ a_g ++ " -> " ++ other_g
  (_, LV [] outval) -> val_output_value_g vt outval
  _ -> undefined
  ) :: ValType -> LambdaOutputValue -> Stateful Haskell

val_value_type_inference_g = ( \case
  (LV [] outval) -> val_output_value_type_inference_g outval
  _ -> undefined
  ) :: LambdaOutputValue -> Stateful (ValType, Haskell)

