{-# language LambdaCase #-}

module CodeGenerators.Values where

import Prelude
  ( Int, String, Maybe(..), (>>=), (>>), (+), (*), (++), ($), concat, concatMap, return
  , error, mapM, init, last, show, map, sequence, zip, drop, length, undefined )
import Data.List ( intercalate, splitAt )
import qualified Data.Map as M ( lookup, insert )
import Control.Monad.State ( (>=>) )

import Helpers ( Haskell, (-->), (.>), indent )

import HaskellTypes.LowLevel
  ( ApplicationDirection(..), ValueName(..), Abstractions(..) )
import CodeGenerators.LowLevel
  ( value_name_g, literal_or_value_name_g, abstractions_g )

import HaskellTypes.Types ( TypeName(..), BaseType(..), ValueType(..) )
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
  ( Stateful, get_indent_level, update_indent_level, get_tuple_type_map )

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

parenthesis_value_g = ( \vt -> \case
  Parenthesis v -> value_g vt v >>= \v_g -> return $ "(" ++ v_g ++ ")"
  Tuple vs -> 
    case vt of 
      AbstractionTypesAndResultType [] bt -> case bt of 
        TupleType vts ->
          map value_g vts --> \value_g_fs ->
          zip value_g_fs vs-->map ( \( f, v ) -> f v )-->sequence >>= \vs_g ->
          return $ "( " ++ init vs_g-->concatMap (++ ", ") ++ vs_g-->last ++ " )"
        ParenthesisType vt -> parenthesis_value_g vt $ Tuple vs
        TypeName (TN tn) -> 
          -- obviously wrong: need to fix with state of user defined types
          mapM (value_g vt) vs >>= \vs_g -> 
          return $ tn ++ "C (" ++ init vs_g-->concatMap (++ ") (") ++ vs_g-->last ++ ")"
      _ -> error $ show (Tuple vs) ++ " has type: " ++ show vt
  ) :: ValueType -> ParenthesisValue -> Stateful Haskell

-- ParenLitOrName

paren_lit_or_name_g = ( \vt -> \case
  ParenthesisValue pv -> parenthesis_value_g vt pv
  LiteralOrValueName lovn -> return $ literal_or_value_name_g vt lovn
  ) :: ValueType -> ParenLitOrName -> Stateful Haskell

-- OneArgApplications

one_arg_function_applications_g = ( \vt (OAA init_plon ad_plon_s) ->
  case ad_plon_s of
    [] -> error "application expression should have at least one application direction"
    _ -> 
      let
      recursive_g = ( \generate_so_far -> \case 
        ( ad, plon ):rest ->
          generate_so_far >>= \sf ->
          paren_lit_or_name_g vt plon >>= \plon_g ->
          let
            combine_g =
              return $ case ad of
                LeftApplication -> sf ++ " " ++ plon_g
                RightApplication -> plon_g ++ " " ++ sf
              :: Stateful Haskell
          in
          recursive_g combine_g rest
        [] -> generate_so_far
        ) :: Stateful Haskell -> [ ( ApplicationDirection, ParenLitOrName) ] ->
             Stateful Haskell
      in
      recursive_g (paren_lit_or_name_g vt init_plon) ad_plon_s
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
  \(AbstractionTypesAndResultType bts bt) (MAAV (As as) nav1) -> 
  let
  ( ( bts1, bts2 ), vt ) =
    ( splitAt (length as) bts, AbstractionTypesAndResultType bts2 bt )
    :: ( ( [ BaseType ], [ BaseType ] ), ValueType )
  in
  no_abstractions_value_1_g vt nav1 >>= \nav1_g ->
  return $ abstractions_g bts2 (As as) ++ nav1_g
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
  return $ value_name_g vt vn ++ maaavs_g-->concatMap (" " ++)
  ) :: ValueType -> ManyArgsApplication -> Stateful Haskell

-- UseFields

use_fields_g = ( \(AbstractionTypesAndResultType bts bt) (UF v) ->
  case bts of 
    [] -> error "use_fields should have abstaction type"
    b:bs -> case b of 
      TypeName tn ->
        get_tuple_type_map >>= \ttm ->
        get_indent_level >>= \il ->
        case M.lookup tn ttm of 
          Just vns ->
            value_g (AbstractionTypesAndResultType bs bt) v >>= \v_g ->
            return $
              "(\\(" ++ tn-->(\(TN n) -> n) ++ "C" ++
              concatMap (\(VN n) -> " " ++ n) vns ++ ") ->\n" ++
              indent (il + 1) ++ v_g ++ " )"
          Nothing -> 
            error "tuple_type does not exist"
      _ -> error "use_fields should have tuple_type type"
  ) :: ValueType -> UseFields -> Stateful Haskell

-- SpecificCase

specific_case_g = ( \(AbstractionTypesAndResultType bts bt) (SC lovn v) ->
  case bts of 
    [] -> error "case should have abstaction type"
    b:bs ->
      value_g (AbstractionTypesAndResultType bs bt) v >>= \v_g ->
      get_indent_level >>= \i ->
      return $
        indent i ++ literal_or_value_name_g (AbstractionTypesAndResultType [] bt) lovn ++
        " -> " ++ v_g
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
    indent i  ++ value_name_g vt vn ++ " = " ++
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

value_g = ( \(AbstractionTypesAndResultType bts bt) (Value (As as) nav) ->
  let
  ( ( bts1, bts2 ), vt ) =
    ( splitAt (length as) bts, AbstractionTypesAndResultType bts2 bt )
    :: ( ( [ BaseType ], [ BaseType ] ), ValueType )
  in
  no_abstractions_value_g vt nav >>= \nav_g ->
  return $ abstractions_g bts1 (As as) ++ nav_g
  ) :: ValueType -> Value -> Stateful Haskell
