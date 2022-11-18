{-# language LambdaCase #-}

module CodeGenerators.Values where

import Prelude
  ( Int, String, Maybe(..), Bool(..), (>>=), (>>), (==), (+), (*), (>), (++), ($), concat
  , concatMap, return, error, mapM, init, last, show, map, sequence, zip, drop, length
  , undefined, foldl )
import Data.List ( intercalate, splitAt )
import qualified Data.Map as M ( lookup, insert )
import Control.Monad.State ( (>=>) )

import Helpers ( Haskell, (-->), (.>), indent )

import HaskellTypes.LowLevel
  ( LiteralOrValueName(..), ApplicationDirection(..), ValueName(..), Abstractions(..) )
import CodeGenerators.LowLevel
  ( lit_g, literal_g, value_name_g, literal_or_value_name_g, abstractions_g )

import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..), vt_bt_are_equivalent )
import CodeGenerators.Types ( value_type_g )

import HaskellTypes.Values
  ( ParenthesisValue(..), ParenLitOrName(..), OneArgApplications(..)
  , MultiplicationFactor(..), Multiplication(..), SubtractionFactor(..), Subtraction(..)
  , NoAbstractionsValue1(..), ManyArgsArgValue(..), ManyArgsApplication(..)
  , UseFields(..), SpecificCase(..), Cases(..)
  , NameTypeAndValue(..), NameTypeAndValueLists(..)
  , NTAVOrNTAVLists(..), NamesTypesAndValues(..), IntermediatesOutput(..)
  , NoAbstractionsValue(..), Value(..) )

import HaskellTypes.Generation
  ( Stateful, get_indent_level, update_indent_level, tuple_type_map_lookup
  , value_map_insert, value_map_lookup )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsArgValue, ManyArgsApplication,
  UseFields, SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue
bt_values_g = ( \case
  TupleType vts -> \vs -> case length vts == length vs of
    False -> error "length of tuple values and tuple types must be the same"

    True -> 
      zip vts vs-->map ( \( vt, v ) -> value_g vt v )-->sequence >>= \vs_g ->
      return $ "( " ++ init vs_g-->concatMap (++ ", ") ++ vs_g-->last ++ " )"

  ParenthesisType vt -> vt_values_g vt

  TypeName tn -> \vs -> tuple_type_map_lookup tn >>= \case
    Nothing -> error $ "Did not find a definition for tuple_type: " ++ show tn
    Just fatl ->
      zip (map get_vt fatl) vs-->mapM (\( vt, v ) -> value_g vt v) >>= \vs_g -> 
      return $ show tn ++ "C (" ++ init vs_g-->concatMap (++ ") (") ++ vs_g-->last ++ ")"
  ) :: BaseType -> [ Value ] -> Stateful Haskell

vt_values_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> \vs ->
    error $ show (Tuple vs) ++ " has type: " ++ show vt

  AbsTypesAndResType [] bt -> bt_values_g bt
  ) :: ValueType -> [ Value ] -> Stateful Haskell

parenthesis_value_g = ( \vt -> \case
  Parenthesis v -> value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"

  Tuple vs -> vt_values_g vt vs
  ) :: ValueType -> ParenthesisValue -> Stateful Haskell

-- ParenLitOrName
paren_lit_or_name_g = ( \vt -> \case
  ParenthesisValue pv -> parenthesis_value_g vt pv

  LiteralOrValueName lovn -> literal_or_value_name_g vt lovn
  ) :: ValueType -> ParenLitOrName -> Stateful Haskell

-- OneArgApplications
plon_type_inference_g = ( \case
  ParenthesisValue pv ->
    error $
      "Cannot infer types for values inside parenthesis in one argument application"
      ++
      ", please define the following as an intermediate value: " ++ show pv

  LiteralOrValueName lovn -> lovn --> \case
    Literal l -> return $ ( AbsTypesAndResType [] (TypeName (TN "Int")), lit_g l)

    ValueName vn -> value_map_lookup vn >>= \case
      Nothing -> error $ "Could not find value: " ++ value_name_g vn 

      Just vt -> return ( vt, value_name_g vn )
  ) :: ParenLitOrName -> Stateful ( ValueType, Haskell )

one_arg_application_g = ( \( vt_left, hs_left ) ( vt_right, hs_right ) ->
  case vt_left of 
    AbsTypesAndResType [] _ -> 
      error $
        "Cannot apply argument to something that does not have a function type. \n"
        ++
        "Type : " ++ show vt_left

    AbsTypesAndResType (abs_bt : abs_bts) bt -> 
      vt_bt_are_equivalent ( vt_right, abs_bt ) --> \case
        False -> 
          error $
            "types don't match for one argument function application. " ++
            "types involved:\n  " ++ show vt_right ++ "\n  " ++ show abs_bt 
        True -> ( AbsTypesAndResType abs_bts bt, hs_left ++ " " ++ hs_right )
  ) :: ( ValueType, Haskell ) -> ( ValueType, Haskell ) -> ( ValueType, Haskell )

add_next_application_g = ( \so_far_with_type ( ad, plon ) ->
  so_far_with_type >>= \sfwt_g ->
  plon_type_inference_g plon >>= \plon_g ->
  return $ case ad of
    LeftApplication -> one_arg_application_g sfwt_g plon_g
    RightApplication -> one_arg_application_g plon_g sfwt_g
  ) :: Stateful ( ValueType, Haskell ) -> ( ApplicationDirection, ParenLitOrName ) ->
       Stateful ( ValueType, Haskell )

one_arg_applications_g = ( \vt oaa@(OAA init_plon ad_plon_s) -> case ad_plon_s of
  [] -> error "application expression should have at least one application operator"

  _ ->
    foldl add_next_application_g (plon_type_inference_g init_plon) ad_plon_s >>=
    \( inferred_vt, hs ) -> case vt == inferred_vt of 
      False -> 
        error $ "type inference for one argument applications failed in: " ++ show oaa
           
      True -> return hs
  ) :: ValueType -> OneArgApplications -> Stateful Haskell

-- MultiplicationFactor
multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> one_arg_applications_g vt oaas

  ParenLitOrNameMF plon -> paren_lit_or_name_g vt plon
  ) :: ValueType -> MultiplicationFactor -> Stateful Haskell

-- Multiplication
multiplication_g = ( \vt (Mul mfs) -> 
  mapM (multiplication_factor_g vt) mfs >>= intercalate " * " .> return
  ) :: ValueType -> Multiplication -> Stateful Haskell

-- SubtractionFactor
subtraction_factor_g = ( \vt -> \case
  MulSF m -> multiplication_g vt m
  OAASF oaas -> one_arg_applications_g vt oaas
  ParenLitOrNameSF plon -> paren_lit_or_name_g vt plon
  ) :: ValueType -> SubtractionFactor -> Stateful Haskell

-- Subtraction
subtraction_g = ( \vt (Sub sf1 sf2) ->
  subtraction_factor_g vt sf1 >>= \sf1_g ->
  subtraction_factor_g vt sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: ValueType -> Subtraction -> Stateful Haskell

-- NoAbstractionsValue1
no_abstractions_value_1_g = ( \vt -> \case
  Subtraction sub -> subtraction_g vt sub
  Multiplication mul -> multiplication_g vt mul
  OneArgApps oaas -> one_arg_applications_g vt oaas
  PLON plon -> paren_lit_or_name_g vt plon
  ) :: ValueType -> NoAbstractionsValue1 -> Stateful Haskell

-- ManyArgsArgValue
many_args_arg_value_g = (
  \(AbsTypesAndResType bts bt) (MAAV (As as) nav1) -> 
  case length as > length bts of 
    True ->
      error $
        "More abstractions than abstraction types." ++
        "\n  Abstractions:  " ++ show (As as) ++
        "\n  Types:  " ++ show bts

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
bts_maavs_vn_g = ( \bts maaavs vn ->
  let
  bt_maav_g = ( \( bt, maaav ) ->
    let
    maav_vt = case bt of
      ParenthesisType vt -> vt
      _ -> (AbsTypesAndResType [] bt)
      :: ValueType
    in
    case maaav of
      MAAV (As []) (PLON plon) -> paren_lit_or_name_g maav_vt plon

      _ ->
        many_args_arg_value_g maav_vt maaav >>= \maaav_g ->
        return $ "(" ++ maaav_g ++ ")"
      ) :: ( BaseType, ManyArgsArgValue )-> Stateful Haskell
  in
  zip bts maaavs-->mapM bt_maav_g >>= \maaavs_g ->
  return $ value_name_g vn ++ maaavs_g-->concatMap (" " ++)
  ) :: [ BaseType ] -> [ ManyArgsArgValue ] -> ValueName -> Stateful Haskell

many_args_application_g = ( \vt1 (MAA maaavs vn) -> 
  value_map_lookup vn >>= \case
    Nothing -> error $ "Could not find value: " ++ value_name_g vn 

    Just (AbsTypesAndResType bts final_bt) ->
      let
      ( bts1, bts2 ) = splitAt (length maaavs) bts
        :: ( [ BaseType ], [ BaseType ] )
      in
      case vt1 == AbsTypesAndResType bts2 final_bt of 
        False -> 
          error $
            "types don't match for many arguments function application. " ++
            "types involved:\n  " ++ show vt1 ++
            "\n  " ++ show (AbsTypesAndResType bts2 final_bt)

        True -> bts_maavs_vn_g bts2 maaavs vn
  ) :: ValueType -> ManyArgsApplication -> Stateful Haskell

-- UseFields

insert_to_value_map_ret_vn = ( \(FT vn vt) -> value_map_insert ( vn, vt ) >> return vn )
  :: FieldAndType -> Stateful ValueName

err_msg = "use_fields abstraction should have tuple_type type"
use_fields_g = ( \(AbsTypesAndResType bts bt) (UF v) -> case bts of 
  [] -> error "use_fields should have abstaction type"

  b:bs -> case b of
    TupleType _ -> error err_msg
    ParenthesisType _ -> error err_msg

    TypeName tn -> tuple_type_map_lookup tn >>= \case 
      Nothing -> error "tuple_type does not exist"

      Just fatl ->
        get_indent_level >>= \il ->
        mapM insert_to_value_map_ret_vn fatl >>= \vns ->
        value_g (AbsTypesAndResType bs bt) v >>= \v_g ->
        return $
          "(\\(" ++ show tn ++ "C" ++ concatMap ( value_name_g .> (" " ++) ) vns ++
          ") ->\n" ++ indent (il + 1) ++ v_g ++ " )"

  ) :: ValueType -> UseFields -> Stateful Haskell

-- SpecificCase
specific_case_g = ( \vt@(AbsTypesAndResType bts bt) sc@(SC lovn v) ->
  case bts of 
    [] -> error $ "case should have abstaction type" ++ show vt ++ show sc
    b:bs -> case lovn of 
      Literal l ->  return $ literal_g (AbsTypesAndResType [] b) l

      ValueName vn ->
        value_map_insert (vn, AbsTypesAndResType [] b) >>
        value_g (AbsTypesAndResType bs bt) v >>= \v_g ->
        get_indent_level >>= \i ->
        return $ indent i ++ value_name_g vn ++ " -> " ++ v_g
  ) :: ValueType -> SpecificCase -> Stateful Haskell

-- Cases
cases_g = ( \vt (Cs cs) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> mapM (specific_case_g vt) cs >>= \cs_g ->
  update_indent_level i >>
  ("\\case\n" ++ init cs_g-->concatMap (++ "\n") ++ last cs_g)-->return
  ) :: ValueType -> Cases -> Stateful Haskell

-- NameTypeAndValue
name_type_and_value_g = ( \(NTAV vn vt v) -> 
  value_map_insert ( vn, vt ) >>
  value_g vt v >>= \v_g ->
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
    _ -> error "name_type_and_value_lists_g: lists must have the same length"
    ) :: ( [ ValueName ], [ ValueType ], [ Value ] ) -> [ NameTypeAndValue ]
  in
  zip3 ( vns, vts, vs )-->mapM name_type_and_value_g >>= concat .> return
  ) :: NameTypeAndValueLists -> Stateful Haskell

-- NTAVOrNTAVLists
ntav_or_ntav_lists_g = ( \case 
  NameTypeAndValue ntav -> name_type_and_value_g ntav
  NameTypeAndValueLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> Stateful Haskell

-- NamesTypesAndValues
names_types_and_values_g = ( \(NTAVs ntavs) ->
  ntavs-->mapM ntav_or_ntav_lists_g >>= concat .> return
  ) :: NamesTypesAndValues -> Stateful Haskell

-- IntermediatesOutput
intermediates_output_g = ( \vt (IntermediatesOutput_ ntavs v) ->
  get_indent_level >>= \i ->
  update_indent_level (i + 1) >> names_types_and_values_g ntavs >>= \ntavs_g ->
  value_g vt v >>= \v_g ->
  let
  hs_source =
    "\n" ++ indent (i + 1) ++ "let\n" ++ ntavs_g ++
    indent (i + 1) ++ "in\n" ++ indent (i + 1) ++ v_g
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
  ( ( bts1, bts2 ), vt ) =
    ( splitAt (length as) bts, AbsTypesAndResType bts2 bt )
    :: ( ( [ BaseType ], [ BaseType ] ), ValueType )
  in
  abstractions_g bts1 (As as) >>= \as_g ->
  no_abstractions_value_g vt nav >>= \nav_g ->
  return $ as_g ++ nav_g
  ) :: ValueType -> Value -> Stateful Haskell
