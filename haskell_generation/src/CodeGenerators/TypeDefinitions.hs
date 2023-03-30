module CodeGenerators.TypeDefinitions where

import Data.List (intercalate)

import Helpers (Haskell, (==>), (.>))

import ParsingTypes.LowLevelValues (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..), TypeApplication(..))
import ParsingTypes.TypeDefinitions 

import AfterParsing.Types 
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations

-- All:
-- Field', TupleTypeDefinition', TupleTypeDefinition,
-- OrTypeCase', OrTypeDefinition', OrTypeDefinition
-- TypeDefinition

-- Field': field_g

field_g = ( \(NameAndType' field_name field_type) type_name ->
  value_map_insert
    (VN $ "get_" ++ show field_name)
    (FunctionType' $ InputAndOutputType' (TypeName' type_name) field_type) >>
  return ("get_" ++ show field_name ++ " :: " ++ show field_type)
  ) :: Field' -> TypeName -> Stateful Haskell

-- TupleTypeDefinition': tuple_type_definition'_g, fields_g

tuple_type_definition'_g = (
  \(NameAndFields' (ConstructorAndInputs' type_name _) fields) ->
  type_map_insert type_name (FieldList fields) >>
  fields_g fields type_name >>= \fields_hs ->
  return $
    "\ndata " ++ show type_name ++ " =\n  " ++ fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDefinition' -> Stateful Haskell

fields_g = ( \fields type_name ->
  fields==>mapM (flip field_g type_name) >>= \fields_hs ->
  return $ show type_name ++ "C { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field' ] -> TypeName -> Stateful Haskell

-- TupleTypeDefinition: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conversion .> tuple_type_definition'_g
  :: TupleTypeDefinition -> Stateful Haskell

-- OrTypeCase': or_type_case'_g

or_type_case'_g = ( \(NameAndMaybeInputType' case_name maybe_input_t) type_name ->
  let 
  (case_type, input_type_g) = case maybe_input_t of
    Nothing -> (TypeName' type_name, "")
    Just input_type ->
      ( FunctionType' $ InputAndOutputType' input_type (TypeName' type_name)
      , " " ++ show input_type )
    :: (ValueType', Haskell)
  in
  value_map_insert case_name case_type >>
  return (show case_name ++ "C" ++ input_type_g)
  ) :: OrTypeCase' -> TypeName -> Stateful Haskell

-- OrTypeDefinition': or_type_definition'_g, or_type_cases'_g

or_type_definition'_g = (
  \(NameAndCases' (ConstructorAndInputs' type_name _) or_type_cases) -> 
  type_map_insert type_name (OrTypeCaseList or_type_cases) >>
  or_type_cases'_g or_type_cases type_name >>= \cases_hs ->
  return $
    "\n\ndata " ++ show type_name ++ " =\n  " ++ cases_hs ++ "\n  deriving Show"
  ) :: OrTypeDefinition' -> Stateful Haskell

or_type_cases'_g = ( \or_type_cases type_name ->
  mapM (flip or_type_case'_g type_name) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase' ] -> TypeName -> Stateful Haskell

-- OrTypeDefinition: or_type_definition_g

or_type_definition_g = 
  or_type_def_conversion .> or_type_definition'_g
  :: OrTypeDefinition -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDefinition tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDefinition or_t_def -> or_type_definition_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell
