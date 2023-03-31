module CodeGenerators.TypeDefinitions where

import Data.List (intercalate)

import Helpers (Haskell, (==>), (.>))

import ParsingTypes.LowLevelValues (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..), ConsAndTypeVars(..))
import ParsingTypes.TypeDefinitions 

import AfterParsing.Types 
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations

-- All:
-- Field', TupleTypeDefinition', TupleTypeDefinition,
-- OrTypeCase', OrTypeDefinition', OrTypeDefinition
-- TypeDefinition

-- Field': field_g

field_g = ( \(NameAndType' field_name field_type) type_application ->
  value_map_insert
    (VN $ "get_" ++ show field_name)
    (FunctionType' $
      InputAndOutputType' (ConsAndTypeVars' type_application) field_type) >>
  return ("get_" ++ show field_name ++ " :: " ++ show field_type)
  ) :: Field' -> ConsAndTypeVars' -> Stateful Haskell

-- type_g =
--   :: ValueType' -> ConsAndTypeVars' -> Stateful Haskell

-- TupleTypeDefinition': tuple_type_definition'_g, fields_g

tuple_type_definition'_g = (
  \(NameAndFields' type_application@(ConsAndTVars' type_name _) fields) ->
  type_map_insert type_name (FieldList fields) >>
  fields_g fields type_application >>= \fields_hs ->
  return $
    "\ndata " ++ show type_application ++ " =\n  " ++
    fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDefinition' -> Stateful Haskell

fields_g = ( \fields type_application@(ConsAndTVars' type_name _) ->
  fields==>mapM (flip field_g type_application) >>= \fields_hs ->
  return $ "C" ++ show type_name ++ " { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field' ] -> ConsAndTypeVars' -> Stateful Haskell

-- TupleTypeDefinition: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conversion .> tuple_type_definition'_g
  :: TupleTypeDefinition -> Stateful Haskell

-- OrTypeCase': or_type_case_g

or_type_case_g = (
  \(NameAndMaybeInputType' case_name maybe_input_t) type_application ->
  let 
  (case_type, input_type_g) = case maybe_input_t of
    Nothing -> (ConsAndTypeVars' type_application, "")
    Just input_type ->
      ( FunctionType' $
        InputAndOutputType' input_type $ ConsAndTypeVars' type_application
      , " " ++ show input_type )
    :: (ValueType', Haskell)
  in
  value_map_insert case_name case_type >>
  return ("C" ++ show case_name ++ input_type_g)
  ) :: OrTypeCase' -> ConsAndTypeVars' -> Stateful Haskell

-- OrTypeDefinition': or_type_definition'_g, or_type_cases_g

or_type_definition'_g = (
  \(NameAndCases'
    type_application@(ConsAndTVars' type_name _) or_type_cases) -> 
  type_map_insert type_name (OrTypeCaseList or_type_cases) >>
  or_type_cases_g or_type_cases type_application >>= \cases_hs ->
  return $
    "\ndata " ++ show type_application ++ " =\n  " ++
    cases_hs ++ "\n  deriving Show\n"
  ) :: OrTypeDefinition' -> Stateful Haskell

or_type_cases_g = ( \or_type_cases type_application ->
  mapM (flip or_type_case_g type_application) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase' ] -> ConsAndTypeVars' -> Stateful Haskell

-- OrTypeDefinition: or_type_definition_g

or_type_definition_g = 
  or_type_def_conversion .> or_type_definition'_g
  :: OrTypeDefinition -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDefinition tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDefinition or_t_def -> or_type_definition_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell
