module CodeGenerators.TypeDefinitions where

import Data.List (intercalate)

import Helpers (Haskell, (==>))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..))
import ParsingTypes.TypeDefinitions 

import AfterParsing.Types (ValueType'(..), FunctionType'(..), FieldsOrCases(..))
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations

-- All: Field, TupleTypeDefinition, OrTypeCase, OrTypeDefinition, TypeDefinition

-- Field: field_g

field_g = ( \(NameAndType field_name field_type) type_name ->
  let
  field_type' = value_type_conversion field_type
  in
  value_map_insert
    (VN $ "get_" ++ show field_name)
    (FunctionType' $ InAndOutType (TypeName' type_name) field_type') >>
  return ("get_" ++ show field_name ++ " :: " ++ show field_type')
  ) :: Field -> TypeName -> Stateful Haskell

-- TupleTypeDefinition: tuple_type_definition_g, fields_g

tuple_type_definition_g = ( \(NameAndFields type_name fields) ->
  type_map_insert type_name (FieldList $ map field_conversion fields) >>
  fields_g fields type_name >>= \fields_hs ->
  return $
    "\ndata " ++ show type_name ++ " =\n  " ++ fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDefinition -> Stateful Haskell

fields_g = ( \fields type_name ->
  fields==>mapM (flip field_g type_name) >>= \fields_hs ->
  return $ show type_name ++ "C { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field ] -> TypeName -> Stateful Haskell

-- OrTypeCase: or_type_case_g

or_type_case_g = ( \(OrTypeCase case_name maybe_input_type) type_name ->
  let 
  case_type = case maybe_input_type of
    Nothing -> TypeName' type_name
    Just input_type ->
      FunctionType' $
        InAndOutType (value_type_conversion input_type) $ TypeName' type_name 
    :: ValueType'

  input_type_g = case maybe_input_type of 
    Nothing -> ""
    Just input_type  -> " " ++ show input_type
    :: Haskell
  in
  value_map_insert case_name case_type >>
  return (show case_name ++ "C" ++ input_type_g)
  ) :: OrTypeCase -> TypeName -> Stateful Haskell

-- OrTypeDefinition: or_type_def_g, or_type_cases_g, or_type_case_g

or_type_definition_g = (
  \(NameAndCases type_name or_t_c1 or_t_c2 or_t_cs) -> 
  type_map_insert type_name
    (OrTypeCaseList $ map or_type_case_conversion $ or_t_c1 : or_t_c2 : or_t_cs) >>
  or_type_cases_g or_t_cs type_name >>= \cases_hs ->
  return $
    "\n\ndata " ++ show type_name ++ " =\n  " ++ cases_hs ++ "\n  deriving Show"
  ) :: OrTypeDefinition -> Stateful Haskell

or_type_cases_g = ( \or_type_cases type_name ->
  mapM (flip or_type_case_g type_name) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase ] -> TypeName -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDefinition tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDefinition or_t_def -> or_type_definition_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell
