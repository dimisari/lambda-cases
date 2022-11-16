{-# language LambdaCase #-}

module CodeGenerators.Values where

import Prelude
  ( Int, String, Maybe(..), Bool(..), (>>=), (>>), (==), (+), (*), (++), ($), concat
  , concatMap, return, error, mapM, init, last, show, map, sequence, zip, drop, length
  , undefined )
import Data.List ( intercalate, splitAt )
import qualified Data.Map as M ( lookup, insert )
import Control.Monad.State ( (>=>) )

import Helpers ( Haskell, (-->), (.>), indent )

import HaskellTypes.LowLevel
  ( ApplicationDirection(..), ValueName(..), Abstractions(..) )
import CodeGenerators.LowLevel
  ( value_name_g, literal_or_value_name_g, abstractions_g )

import HaskellTypes.Types ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..) )
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
  , value_map_insert )

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
bt_values_g = ( \bt vs -> case bt of 
  TupleType vts -> case length vs == length vts of
    False -> error "length of tuple values and tuple types must be the same"

    True -> 
      zip vts vs-->map ( \( vt, v ) -> value_g vt v )-->sequence >>= \vs_g ->
      return $ "( " ++ init vs_g-->concatMap (++ ", ") ++ vs_g-->last ++ " )"

  ParenthesisType vt -> parenthesis_value_g vt $ Tuple vs

  TypeName (TN tn) -> 
    -- pseudocode: tuple type map lookup for types + zip + mapM value_g with the types
    mapM (value_g (AbsTypesAndResType [] bt)) vs >>= \vs_g -> 
    return $ tn ++ "C (" ++ init vs_g-->concatMap (++ ") (") ++ vs_g-->last ++ ")"
  ) :: BaseType -> [ Value ] -> Stateful Haskell

vt_values_g = ( \vt vs -> case vt of 
  AbsTypesAndResType (_:_) _ ->
    error $ show (Tuple vs) ++ " has type: " ++ show vt

  AbsTypesAndResType [] bt -> bt_values_g bt vs
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
one_arg_function_applications_g = ( \vt (OAA init_plon ad_plon_s) -> case ad_plon_s of
  [] -> error "application expression should have at least one application direction"
  _ -> 
    let
    recursive_g = ( \vt generate_so_far -> \case 
      ( ad, plon ):rest ->
        generate_so_far >>= \sf ->
        paren_lit_or_name_g vt plon >>= \plon_g ->
        let
        combine_g = return $ case ad of
          LeftApplication -> sf ++ " " ++ plon_g
          RightApplication -> plon_g ++ " " ++ sf
          :: Stateful Haskell
        in
        recursive_g vt combine_g rest
      [] -> generate_so_far
      ) :: ValueType ->
           Stateful Haskell ->
           [ ( ApplicationDirection, ParenLitOrName) ] ->
           Stateful Haskell
    in
    recursive_g vt (paren_lit_or_name_g vt init_plon) ad_plon_s
  ) :: ValueType -> OneArgApplications -> Stateful Haskell

-- MultiplicationFactor
multiplication_factor_g = ( \vt -> \case
  OneArgAppMF oaas -> one_arg_function_applications_g vt oaas
  ParenLitOrNameMF plon -> paren_lit_or_name_g vt plon
  ) :: ValueType -> MultiplicationFactor -> Stateful Haskell

-- Multiplication
multiplication_g = ( \vt (Mul mfs) -> 
  mapM (multiplication_factor_g vt) mfs >>= intercalate " * " .> return
  ) :: ValueType -> Multiplication -> Stateful Haskell

-- SubtractionFactor
subtraction_factor_g = ( \vt -> \case
  MulSF m -> multiplication_g vt m
  OAASF oaas -> one_arg_function_applications_g vt oaas
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
  OneArgApps oaas -> one_arg_function_applications_g vt oaas
  PLON plon -> paren_lit_or_name_g vt plon
  ) :: ValueType -> NoAbstractionsValue1 -> Stateful Haskell

-- ManyArgsArgValue
many_args_app_arg_value_g = (
  \(AbsTypesAndResType bts bt) (MAAV (As as) nav1) -> 
  let
  ( ( bts1, bts2 ), vt ) =
    ( splitAt (length as) bts, AbsTypesAndResType bts2 bt )
    :: ( ( [ BaseType ], [ BaseType ] ), ValueType )
  in
  abstractions_g bts2 (As as) >>= \as_g ->
  no_abstractions_value_1_g vt nav1 >>= \nav1_g ->
  return $ as_g ++ nav1_g
  ) :: ValueType -> ManyArgsArgValue -> Stateful Haskell

-- ManyArgsApplication
many_args_application_g = ( \vt (MAA maaavs vn) -> 
  let
  help_g = \case 
    (MAAV (As []) (PLON plon)) -> paren_lit_or_name_g vt plon
    maaav ->
      many_args_app_arg_value_g vt maaav >>= \maaav_g -> return $ "(" ++ maaav_g ++ ")"
    :: ManyArgsArgValue -> Stateful Haskell
  in
  mapM help_g maaavs >>= \maaavs_g ->
  return $ value_name_g vn ++ maaavs_g-->concatMap (" " ++)
  ) :: ValueType -> ManyArgsApplication -> Stateful Haskell

-- UseFields
err_msg = "use_fields abstraction should have tuple_type type"

insert_to_value_map_ret_vn = ( \(FT vn vt) -> value_map_insert ( vn, vt ) >> return vn )
  :: FieldAndType -> Stateful ValueName

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
          "(\\(" ++ show tn ++ "C" ++
          concatMap ( \vn -> " " ++ value_name_g vn ) vns ++ ") ->\n" ++
          indent (il + 1) ++ v_g ++ " )"

  ) :: ValueType -> UseFields -> Stateful Haskell

-- SpecificCase
specific_case_g = ( \(AbsTypesAndResType bts bt) (SC lovn v) ->
  case bts of 
    [] -> error "case should have abstaction type"
    b:bs ->
      literal_or_value_name_g (AbsTypesAndResType [] bt) lovn >>= \lovn_g ->
      value_g (AbsTypesAndResType bs bt) v >>= \v_g ->
      get_indent_level >>= \i ->
      return $ indent i ++ lovn_g ++ " -> " ++ v_g
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
    ( vn:vns, vt:vts, v:vs) -> NTAV vn vt v:zip3 ( vns, vts, vs )
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
