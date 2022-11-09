{-# LANGUAGE LambdaCase #-}

module CodeGenerators.Values where

import Prelude
  ( Int, (>>=), (>>), (+), (*), (++), ($), concat, concatMap, return, error, mapM, init
  , last )
import Data.List ( intercalate, replicate )
import Control.Monad.State ( (>=>), State, get, put )

import Helpers ( Haskell, (-->), (.>) )

import HaskellTypes.LowLevel ( ApplicationDirection(..), ValueName, Abstractions(..) )
import CodeGenerators.LowLevel
  ( value_name_g, literal_or_value_name_g, abstractions_g )

import HaskellTypes.Types ( TypeName, ValueType )
import CodeGenerators.Types ( value_type_g )

import HaskellTypes.Values
  ( ParenthesisValue(..), ParenLitOrName(..), OneArgFunctionApplications(..)
  , MultiplicationFactor(..), Multiplication(..), SubtractionFactor(..), Subtraction(..)
  , NoAbstractionsValue1(..), ManyArgsAppArgValue(..), ManyArgsApplication(..)
  , SpecificCase(..), Cases(..)
  , NameTypeAndValue(..), NameTypeAndValueLists(..)
  , NTAVOrNTAVLists(..), NamesTypesAndValues(..), IntermediatesOutput(..)
  , NoAbstractionsValue(..), Value(..) )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgFunctionApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsAppArgValue, ManyArgsApplication,
  SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues, IntermediatesOutput,
  NoAbstractionsValue, Value
-}

data GState = IndentAndTypeNames Int [ TypeName ]
type Stateful = State GState

type IndentState = State Int

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell

-- ParenthesisValue

parenthesis_value_g = ( ( \case
  Parenthesis v -> value_g v
  Tuple vs -> 
    mapM value_g vs >>= \vs_g ->
    return $ " " ++ init vs_g-->concatMap (++ ", ") ++ vs_g-->last ++ " "
  ) >=> ("(" ++) .> (++ ")") .> return
  ) :: ParenthesisValue -> IndentState Haskell

-- ParenLitOrName

paren_lit_or_name_g = ( \case
  ParenthesisValue pv -> parenthesis_value_g pv
  LiteralOrValueName lovn -> return $ literal_or_value_name_g lovn
  ) :: ParenLitOrName -> IndentState Haskell

-- OneArgFunctionApplications

one_arg_function_applications_g = ( \(OneArgFunctionApplications init_plon ad_plon_s) ->
  case ad_plon_s of
    [] -> error "application expression should have at least one application direction"
    _ -> 
      let
      recursive_g = ( \generate_so_far -> \case 
        ( ad, plon ):rest ->
          generate_so_far >>= \sf ->
          paren_lit_or_name_g plon >>= \plon_g ->
          let
            combine_g =
              return $ case ad of
                LeftApplication -> sf ++ " " ++ plon_g
                RightApplication -> plon_g ++ " " ++ sf
              :: IndentState Haskell
          in
          recursive_g combine_g rest
        [] -> generate_so_far
        ) :: IndentState Haskell -> [ ( ApplicationDirection, ParenLitOrName) ] ->
             IndentState Haskell
      in
      recursive_g (paren_lit_or_name_g init_plon) ad_plon_s
  ) :: OneArgFunctionApplications -> IndentState Haskell

-- MultiplicationFactor

multiplication_factor_g = ( \case
  OneArgApplicationsMF oaas -> one_arg_function_applications_g oaas
  ParenLitOrNameMF plon -> paren_lit_or_name_g plon
  ) :: MultiplicationFactor -> IndentState Haskell

-- Multiplication

multiplication_g = ( \(Multiplication mfs) -> 
  mapM multiplication_factor_g mfs >>= intercalate " * " .> return
  ) :: Multiplication -> IndentState Haskell

-- SubtractionFactor

subtraction_factor_g = ( \case
  MultiplicationSF m -> multiplication_g m
  OneArgApplicationsSF oaas -> one_arg_function_applications_g oaas
  ParenLitOrNameSF plon -> paren_lit_or_name_g plon
  ) :: SubtractionFactor -> IndentState Haskell

-- Subtraction

subtraction_expression_g = ( \(Subtraction sf1 sf2) ->
  subtraction_factor_g sf1 >>= \sf1_g ->
  subtraction_factor_g sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: Subtraction -> IndentState Haskell

-- NoAbstractionsValue1

no_abstractions_value_1_g = ( \case
  Sub sub -> subtraction_expression_g sub
  Mul mul -> multiplication_g mul
  OneArgApps oaas -> one_arg_function_applications_g oaas
  PLON plon -> paren_lit_or_name_g plon
  ) :: NoAbstractionsValue1 -> IndentState Haskell

-- ManyArgsAppArgValue

many_args_app_arg_value_g = ( \(ManyArgsAppArgValue as nav1) -> 
  no_abstractions_value_1_g nav1 >>= \nav1_g ->
  return $ abstractions_g as ++ nav1_g
  ) :: ManyArgsAppArgValue -> IndentState Haskell

-- ManyArgsApplication

many_args_application_g = ( \(ManyArgsApplication maaavs vn) -> 
  let
  help_g = \case 
    (ManyArgsAppArgValue (Abstractions []) (PLON plon)) -> paren_lit_or_name_g plon
    maaav ->
      many_args_app_arg_value_g maaav >>= \maaav_g -> return $ "(" ++ maaav_g ++ ")"
    :: ManyArgsAppArgValue -> IndentState Haskell
  in
  mapM help_g maaavs >>= \maaavs_g ->
  return $ value_name_g vn ++ maaavs_g-->concatMap (" " ++)
  ) :: ManyArgsApplication -> IndentState Haskell

-- SpecificCase

specific_case_g = ( \(SpecificCase lovn v) ->
  value_g v >>= \v_g ->
  get >>= \i ->
  return $ indent i ++ literal_or_value_name_g lovn ++ " -> " ++ v_g
  ) :: SpecificCase -> IndentState Haskell

-- Cases

cases_g = ( \(Cs cs) ->
  get >>= \i ->
  put (i + 1) >> mapM specific_case_g cs >>= \cs_g ->
  put i >> ("\\case\n" ++ init cs_g-->concatMap (++ "\n") ++ last cs_g)-->return
  ) :: Cases -> IndentState Haskell

-- NameTypeAndValue

name_type_and_value_g = ( \(NameTypeAndValue vn vt v) -> 
  value_g v >>= \v_g ->
  get >>= \i ->
  let
  combine value_begin value_end =
    indent i  ++ value_name_g vn ++ " = " ++
    value_begin ++ v_g ++ value_end ++ "\n" ++
    indent (i + 1) ++ ":: " ++ value_type_g vt ++ "\n"
    :: Haskell
  in
  return $ case v of
    (Value (Abstractions []) nae) -> combine "" ""
    _ -> combine "( " " )"
  ) :: NameTypeAndValue -> IndentState Haskell

-- NameTypeAndValueLists

name_type_and_value_lists_g = ( \(NameTypeAndValueLists vns vts vs) -> 
  let
  zip3 = ( \case
    ( vn:vns, vt:vts, v:vs) -> NameTypeAndValue vn vt v:zip3 ( vns, vts, vs )
    ( [], [], [] ) -> []
    _ -> error "name_type_and_value_lists_g: lists must have the same length"
    ) :: ( [ ValueName ], [ ValueType ], [ Value ] ) -> [ NameTypeAndValue ]
  in
  zip3 ( vns, vts, vs )-->mapM name_type_and_value_g >>= concat .> return
  ) :: NameTypeAndValueLists -> IndentState Haskell

-- NTAVOrNTAVLists

ntav_or_ntav_lists_g = ( \case 
  NTAV ntav -> name_type_and_value_g ntav
  NTAVLists ntav_lists -> name_type_and_value_lists_g ntav_lists
  ) :: NTAVOrNTAVLists -> IndentState Haskell

-- NamesTypesAndValues

names_types_and_values_g = ( \(NamesTypesAndValues ntavs) ->
  ntavs-->mapM ntav_or_ntav_lists_g >>= concat .> return
  ) :: NamesTypesAndValues -> IndentState Haskell

-- IntermediatesOutput

intermediates_output_g = ( \(IntermediatesOutput_ ntavs v) ->
  get >>= \i ->
  put (i + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_g ->
  value_g v >>= \v_g ->
  let
  hs_source =
    "\n" ++ indent (i + 1) ++ "let\n" ++ ntavs_g ++
    indent (i + 1) ++ "in\n" ++ indent (i + 1) ++ v_g
    :: Haskell
  in
  put i >> return hs_source
  ) :: IntermediatesOutput -> IndentState Haskell

-- NoAbstractionsValue

no_abstractions_value_g = ( \case
  ManyArgsApp maa -> many_args_application_g maa
  NoAbstractionsValue1 nav1 -> no_abstractions_value_1_g nav1
  Cases cs -> cases_g cs
  IntermediatesOutput io -> intermediates_output_g io
  ) :: NoAbstractionsValue -> IndentState Haskell

-- Value

value_g = ( \(Value as nav) ->
  no_abstractions_value_g nav >>= \nav_g ->
  return $ abstractions_g as ++ nav_g
  ) :: Value -> IndentState Haskell
