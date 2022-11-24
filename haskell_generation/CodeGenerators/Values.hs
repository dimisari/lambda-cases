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
  ( LiteralOrValueName(..), ApplicationDirection(..), ValueName(..), Abstractions(..) )
import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..), vt_bt_are_equivalent
  , vt_shortest_equivalent )
import HaskellTypes.Generation
  ( Stateful, get_indent_level, update_indent_level, tuple_type_map_lookup
  , value_map_insert, value_map_lookup )

import CodeGenerators.LowLevel
  ( lit_g, literal_g, value_name_g, literal_or_value_name_g, abstractions_g )
import CodeGenerators.Types
  ( value_type_g )

import HaskellTypes.Values
import CodeGenerators.ErrorMessages

{- 
  All:
  ParenthesisValue, BaseValue, OneArgApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsArgValue, ManyArgsApplication,
  UseFields, SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue
parenthesis_value_g = ( \vt -> \case
  Parenthesis v -> value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"

  Tuple vs -> vt_values_g vt vs
  ) :: ValueType -> ParenthesisValue -> Stateful Haskell

vt_values_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> \vs -> error $ vt_values_err_msg vs vt

  AbsTypesAndResType [] bt -> bt_values_g bt
  ) :: ValueType -> [ Value ] -> Stateful Haskell

bt_values_g = ( \case
  TupleType vts -> vts_values_g vts
  ParenthesisType vt -> vt_values_g vt
  TypeName tn -> tn_values_g tn
  ) :: BaseType -> [ Value ] -> Stateful Haskell

vts_values_g = ( \vts vs -> case length vts == length vs of
  False -> error vts_values_err_msg

  True -> 
    zipWith value_g vts vs==>sequence >>= \vs_g ->
    return $ "( " ++ init vs_g==>concatMap (++ ", ") ++ vs_g==>last ++ " )"
  ) :: [ ValueType ] -> [ Value ] -> Stateful Haskell

tn_values_g = ( \tn vs -> tuple_type_map_lookup tn >>= \case
  Nothing -> error $ type_not_found_err_msg tn

  Just fatl -> case length vs == length fatl of 
    False -> error tn_values_err_msg

    True -> correct_tn_values_g tn (map get_vt fatl) vs
  ) :: TypeName -> [ Value ] -> Stateful Haskell

correct_tn_values_g = ( \tn vts vs ->
  zipWith value_g vts vs==>sequence >>= \vs_g -> 
  return $ show tn ++ "C (" ++ init vs_g==>concatMap (++ ") (") ++ vs_g==>last ++ ")"
  ) :: TypeName -> [ ValueType ] -> [ Value ] -> Stateful Haskell

-- BaseValue
base_value_g = ( \vt -> \case
  ParenthesisValue pv -> parenthesis_value_g vt pv
  LiteralOrValueName lovn -> literal_or_value_name_g vt lovn
  ) :: ValueType -> BaseValue -> Stateful Haskell

-- OneArgApplications
one_arg_applications_g = ( \vt oaa@(OAA bv_ad_s bv) -> case bv_ad_s of
  [] -> error one_arg_applications_err_msg1

  ( init_bv, init_ad ):bv_ad_s_tail ->

    bv_type_inference_g init_bv >>= \( bv_vt, bv_hs ) ->

    foldM add_next_application_g ( bv_vt, bv_hs, init_ad ) bv_ad_s_tail
      >>= \( final_vt, final_hs, final_ad ) -> 
    bv_type_inference_g bv >>= \bv_g ->

    ( case final_ad of
      LeftApplication -> one_arg_application_g ( final_vt, final_hs ) bv_g
      RightApplication -> one_arg_application_g bv_g ( final_vt, final_hs ) )==>
    \( inferred_vt, hs ) ->

    case vt == inferred_vt of 
      False -> error $ one_arg_applications_err_msg2 oaa

      True -> return hs
  ) :: ValueType -> OneArgApplications -> Stateful Haskell

bv_type_inference_g = ( \case
  ParenthesisValue pv -> error $ bv_type_inference_err_msg pv

  LiteralOrValueName lovn -> lovn==> \case
    Literal l -> return $ ( AbsTypesAndResType [] (TypeName (TN "Int")), lit_g l)

    ValueName vn -> value_map_lookup vn >>= \vt -> return ( vt, value_name_g vn )
  ) :: BaseValue -> Stateful ( ValueType, Haskell )

add_next_application_g = ( \( sf_vt, sf_hs, ad ) ( next_bv, next_ad ) ->
  bv_type_inference_g next_bv >>= \bv_g ->
  let 
  ( next_vt, next_hs ) = case ad of
    LeftApplication -> one_arg_application_g ( sf_vt, sf_hs ) bv_g
    RightApplication -> one_arg_application_g bv_g ( sf_vt, sf_hs )
  in
  return ( next_vt, next_hs, next_ad )
  ) :: ( ValueType, Haskell, ApplicationDirection ) ->
       ( BaseValue, ApplicationDirection ) ->
       Stateful ( ValueType, Haskell, ApplicationDirection )

one_arg_application_g = ( \( vt_left, hs_left ) ( vt_right, hs_right ) -> case vt_left of 
  AbsTypesAndResType [] _ -> error $ one_arg_application_err_msg_1 vt_left

  AbsTypesAndResType (abs_bt : abs_bts) bt -> 
    vt_bt_are_equivalent ( vt_shortest_equivalent vt_right, abs_bt )==> \case
      False -> error $ one_arg_application_err_msg_2 vt_right abs_bt hs_left hs_right

      True -> ( AbsTypesAndResType abs_bts bt, hs_left ++ " " ++ hs_right )
  ) :: ( ValueType, Haskell ) -> ( ValueType, Haskell ) -> ( ValueType, Haskell )

-- MultiplicationFactor
multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> one_arg_applications_g vt oaas
  BaseValueMF bv -> base_value_g vt bv
  ) :: ValueType -> MultiplicationFactor -> Stateful Haskell

-- Multiplication
multiplication_g = ( \vt (Mul mfs) -> 
  mapM (multiplication_factor_g vt) mfs >>= intercalate " * " .> return
  ) :: ValueType -> Multiplication -> Stateful Haskell

-- SubtractionFactor
subtraction_factor_g = ( \vt -> \case
  MulSF m -> multiplication_g vt m
  OneArgAppSF oaas -> one_arg_applications_g vt oaas
  BaseValueSF bv -> base_value_g vt bv
  ) :: ValueType -> SubtractionFactor -> Stateful Haskell

-- Subtraction
subtraction_g = ( \vt (Sub sf1 sf2) ->
  subtraction_factor_g vt sf1 >>= \sf1_g ->
  subtraction_factor_g vt sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: ValueType -> Subtraction -> Stateful Haskell

-- EqualityFactor
equality_factor_g = ( \vt -> \case
  SubEF s -> subtraction_g vt s
  MulEF m -> multiplication_g vt m
  OAAEF oaas -> one_arg_applications_g vt oaas
  BaseValueEF bv -> base_value_g vt bv
  ) :: ValueType -> EqualityFactor -> Stateful Haskell

-- Equality
equality_g = ( \vt (Equ ef1 ef2) ->
  equality_factor_g vt ef1 >>= \ef1_g ->
  equality_factor_g vt ef2 >>= \ef2_g ->
  return $ ef1_g ++ " == " ++ ef2_g
  ) :: ValueType -> Equality -> Stateful Haskell

-- NoAbstractionsValue1
no_abstractions_value_1_g = ( \vt -> \case
  Equality equ -> equality_g vt equ
  Subtraction sub -> subtraction_g vt sub
  Multiplication mul -> multiplication_g vt mul
  OneArgApps oaas -> one_arg_applications_g vt oaas
  BaseValue bv -> base_value_g vt bv
  ) :: ValueType -> NoAbstractionsValue1 -> Stateful Haskell

-- ManyArgsArgValue
many_args_arg_value_g = (
  \(AbsTypesAndResType bts bt) (MAAV (As as) nav1) -> case length as > length bts of 
  True -> error $ many_args_arg_value_err_msg (As as) bts

  False -> 
    let
    ( bts1, bts2 ) = splitAt (length as) bts
      :: ( [ BaseType ], [ BaseType ] )
    in
    abstractions_g bts1 (As as) >>= \as_g ->
    no_abstractions_value_1_g (AbsTypesAndResType bts2 bt) nav1 >>= \nav1_g ->
    return $ as_g ++ nav1_g
  ) :: ValueType -> ManyArgsArgValue -> Stateful Haskell

-- ManyArgsApplication
many_args_application_g = ( \vt (MAA maavs vn) -> value_map_lookup vn >>=
  \(AbsTypesAndResType abs_bts res_bt) ->
  let
  ( bts1, bts2 ) = splitAt (length maavs) abs_bts
    :: ( [ BaseType ], [ BaseType ] )
  in
  case vt == AbsTypesAndResType bts2 res_bt of 
    False -> error $ many_args_application_err_msg vt (AbsTypesAndResType bts2 res_bt)

    True -> bts_maavs_vn_g bts1 maavs vn
  ) :: ValueType -> ManyArgsApplication -> Stateful Haskell

bts_maavs_vn_g = ( \bts maavs vn ->
  zipWith bt_maav_g bts maavs==>sequence >>= \maavs_g ->
  return $ value_name_g vn ++ concatMap (" " ++) maavs_g
  ) :: [ BaseType ] -> [ ManyArgsArgValue ] -> ValueName -> Stateful Haskell

bt_maav_g = ( \bt maav ->
  let
  maav_vt = case bt of
    ParenthesisType vt -> vt
    _ -> (AbsTypesAndResType [] bt)
    :: ValueType
  in
  case maav of
    MAAV (As []) (BaseValue bv) -> base_value_g maav_vt bv

    _ -> many_args_arg_value_g maav_vt maav >>= \maav_g -> return $ "(" ++ maav_g ++ ")"
  ) :: BaseType -> ManyArgsArgValue -> Stateful Haskell

-- UseFields
use_fields_g = ( \(AbsTypesAndResType bts bt) (UF v) -> case bts of 
  [] -> error use_fields_err_msg1

  b:bs -> case b of
    TupleType _ -> error use_fields_err_msg2

    ParenthesisType _ -> error use_fields_err_msg2

    TypeName tn -> tuple_type_map_lookup tn >>= \case 
      Nothing -> error $ type_not_found_err_msg tn

      Just fatl -> correct_use_fields_g tn fatl (AbsTypesAndResType bs bt) v

  ) :: ValueType -> UseFields -> Stateful Haskell

correct_use_fields_g = ( \tn fatl vt v ->
  get_indent_level >>= \il ->
  mapM insert_to_value_map_ret_vn fatl >>= \vns ->
  value_g vt v >>= \v_g ->
  return $
    "(\\(" ++ show tn ++ "C" ++ concatMap ( value_name_g .> (" " ++) ) vns ++
    ") ->\n" ++ indent (il + 1) ++ v_g ++ " )"
  ) :: TypeName -> [ FieldAndType ] -> ValueType -> Value -> Stateful Haskell

insert_to_value_map_ret_vn = ( \(FT vn vt) -> value_map_insert vn vt >> return vn )
  :: FieldAndType -> Stateful ValueName

-- SpecificCase
specific_case_g = ( \vt@(AbsTypesAndResType bts bt) sc@(SC lovn v) -> case bts of 
  [] -> error $ specific_case_err_msg vt sc

  b:bs ->
    let
    generate = ( \g ->
      value_g (AbsTypesAndResType bs bt) v >>= \v_g ->
      get_indent_level >>= \i ->
      return $ indent i ++ g ++ " -> " ++ v_g
      ) :: Haskell -> Stateful Haskell
    in
    case lovn of 
      Literal l -> literal_g (AbsTypesAndResType [] b) l==>generate 

      ValueName vn ->
        value_map_insert vn (vt_shortest_equivalent $ AbsTypesAndResType [] b) >>
        generate (value_name_g vn)
  ) :: ValueType -> SpecificCase -> Stateful Haskell

-- Cases
cases_g = ( \vt (Cs cs) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> mapM (specific_case_g vt) cs >>= \cs_g ->
  update_indent_level i >>
  ("\\case\n" ++ init cs_g==>concatMap (++ "\n") ++ last cs_g)==>return
  ) :: ValueType -> Cases -> Stateful Haskell

-- NameTypeAndValue
name_type_and_value_g = ( \(NTAV vn vt v) -> 
  value_map_insert vn vt >> value_g vt v >>= \v_g ->
  get_indent_level >>= \i ->
  let
  combine = ( \value_begin -> \value_end ->
    indent i  ++ value_name_g vn ++ " = " ++
    value_begin ++ v_g ++ value_end ++ "\n" ++
    indent (i + 1) ++ ":: " ++ value_type_g vt ++ "\n"
    ) :: String -> String -> Haskell
  in
  return $ case v of
    (Value (As []) _) -> combine "" ""
    _ -> combine "( " " )"
  ) :: NameTypeAndValue -> Stateful Haskell

-- NameTypeAndValueLists
name_type_and_value_lists_g = ( \(NTAVLists vns vts vs) -> 
  let
  zip3 = ( \case
    ( vn : vns, vt : vts, v : vs ) -> NTAV vn vt v : zip3 ( vns, vts, vs )
    ( [], [], [] ) -> []
    _ -> error name_type_and_value_lists_err_msg
    ) :: ( [ ValueName ], [ ValueType ], [ Value ] ) -> [ NameTypeAndValue ]
  in
  zip3 ( vns, vts, vs )==>mapM name_type_and_value_g >>= concat .> return
  ) :: NameTypeAndValueLists -> Stateful Haskell

-- NTAVOrNTAVLists
ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue ntav -> name_type_and_value_g ntav
  NameTypeAndValueLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

-- NamesTypesAndValues
names_types_and_values_g = ( \(NTAVs ntavs) ->
  ntavs==>mapM ntav_or_ntav_lists_g >>= concat .> return
  ) :: NamesTypesAndValues -> Stateful Haskell

-- IntermediatesOutput
intermediates_output_g = ( \vt (IntermediatesOutput_ ntavs v) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> names_types_and_values_g ntavs >>= \ntavs_g ->
  value_g vt v >>= \v_g ->
  let
  hs_source =
    "\n" ++
    indent (i + 1) ++ "let\n" ++ ntavs_g ++
    indent (i + 1) ++ "in\n" ++
    indent (i + 1) ++ v_g
    :: Haskell
  in
  update_indent_level i >> return hs_source
  ) :: ValueType -> IntermediatesOutput -> Stateful Haskell

-- NoAbstractionsValue
no_abstractions_value_g = ( \vt -> \case
  ManyArgsApplication maa -> many_args_application_g vt maa
  UseFields uf -> use_fields_g vt uf
  NoAbstractionsValue1 nav1 -> no_abstractions_value_1_g vt nav1
  Cases cs -> cases_g vt cs
  IntermediatesOutput io -> intermediates_output_g vt io
  ) :: ValueType -> NoAbstractionsValue -> Stateful Haskell

-- Value
value_g = ( \(AbsTypesAndResType bts bt) (Value (As as) nav) ->
  let
  ( bts1, bts2 ) = splitAt (length as) bts
    :: ( [ BaseType ], [ BaseType ] )
  in
  abstractions_g bts1 (As as) >>= \as_g ->
  no_abstractions_value_g ( AbsTypesAndResType bts2 bt ) nav >>= \nav_g ->
  return $ as_g ++ nav_g
  ) :: ValueType -> Value -> Stateful Haskell
