module CodeGenerators.TypeDefinitions where

import Data.List (intercalate)

import Helpers (Haskell, (==>), (.>))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName(..))
import ParsingTypes.TypeDefinitions 

import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions

import Conversions.TypeDefinitions

import GenerationState.TypesAndOperations

-- All:
-- Field', TupleTypeDefinition', TupleTypeDefinition,
-- OrTypeCase', OrTypeDefinition', OrTypeDefinition
-- TypeDefinition

-- Field': field_g

field_g = ( \(NameAndType' field_name field_type) cons_and_type_vars ->
  let
  get_function_hs = "get_" ++ show field_name
    :: Haskell
  field_type_hs = type_g field_type $ get_type_vars cons_and_type_vars
    :: Haskell
  in
  value_map_insert
    (VN get_function_hs) (get_function_type cons_and_type_vars field_type) >>
  return (get_function_hs ++ " :: " ++ field_type_hs)
  ) :: Field' -> TypeConstructorAndVariables' -> Stateful Haskell

get_function_type = ( \cons_and_type_vars field_type ->
  FunctionType' $
    InputAndOutputType' (cons_and_vars_to_val_type cons_and_type_vars) field_type
  ) :: TypeConstructorAndVariables' -> ValueType' -> ValueType'

cons_and_vars_to_val_type = ( \cons_and_type_vars ->
  TypeApplication' $
    TypeConstructorAndInputs' (get_t_cons_name cons_and_type_vars) []
  ) :: TypeConstructorAndVariables' -> ValueType'

type_name_g = ( \type_name type_vars->
  lookup type_name type_vars ==> \case
    Just type_var -> type_var
    Nothing -> show type_name
  ) :: TypeName -> [ (TypeName, String) ] -> Haskell

-- TupleTypeDefinition': tuple_type_definition'_g, fields_g

tuple_type_definition'_g = (
  \(ConstructorAndFields'
    cons_and_type_vars@(TypeConstructorAndVariables' type_name type_vars)
    fields) ->
  type_map_insert type_name (TupleType (length type_vars) fields) >>
  fields_g fields cons_and_type_vars >>= \fields_hs ->
  return $
    "\ndata " ++ show cons_and_type_vars ++ " =\n  " ++
    fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDefinition' -> Stateful Haskell

fields_g = (
  \fields cons_and_type_vars@(TypeConstructorAndVariables' type_name _) ->
  fields==>mapM (flip field_g cons_and_type_vars) >>= \fields_hs ->
  return $ "C" ++ show type_name ++ " { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field' ] -> TypeConstructorAndVariables' -> Stateful Haskell

-- TupleTypeDefinition: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conversion .> tuple_type_definition'_g
  :: TupleTypeDefinition -> Stateful Haskell

-- OrTypeCase': or_type_case_g

or_type_case_g = (
  \(NameAndMaybeInputType' case_name maybe_input_t) cons_and_type_vars ->
  let 
  or_type = cons_and_vars_to_val_type cons_and_type_vars
    :: ValueType'
  in
  value_map_insert case_name (case_type maybe_input_t or_type) >>
  return
    ("C" ++ show case_name ++ (input_type_hs maybe_input_t cons_and_type_vars))
  ) :: OrTypeCase' -> TypeConstructorAndVariables' -> Stateful Haskell

case_type = ( \maybe_input_t or_type ->
  case maybe_input_t of
    Nothing -> or_type
    Just input_type -> FunctionType' $ InputAndOutputType' input_type or_type
  ) :: Maybe ValueType' -> ValueType' -> ValueType'

input_type_hs = ( \maybe_input_t cons_and_type_vars -> case maybe_input_t of
  Nothing -> ""
  Just input_type -> " " ++ type_g input_type (get_type_vars cons_and_type_vars)
  ) :: Maybe ValueType' -> TypeConstructorAndVariables' -> Haskell

-- OrTypeDefinition': or_type_definition'_g, or_type_cases_g

or_type_definition'_g = (
  \(ConstructorAndCases'
    cons_and_type_vars@(TypeConstructorAndVariables' type_name type_vars)
    or_type_cases) -> 
  type_map_insert type_name (OrType (length type_vars) or_type_cases) >>
  or_type_cases_g or_type_cases cons_and_type_vars >>= \cases_hs ->
  return $
    "\ndata " ++ show cons_and_type_vars ++ " =\n  " ++
    cases_hs ++ "\n  deriving Show\n"
  ) :: OrTypeDefinition' -> Stateful Haskell

or_type_cases_g = ( \or_type_cases cons_and_type_vars ->
  mapM (flip or_type_case_g cons_and_type_vars) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase' ] -> TypeConstructorAndVariables' -> Stateful Haskell

-- OrTypeDefinition: or_type_definition_g

or_type_definition_g = 
  or_type_def_conversion .> or_type_definition'_g
  :: OrTypeDefinition -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDefinition tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDefinition or_t_def -> or_type_definition_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell

-- Helpers: type_g

type_g = ( \value_type type_vars -> case value_type of
  TypeApplication' (TypeConstructorAndInputs' type_name type_inputs) ->
    (type_name_g type_name type_vars ++
    concatMap (flip type_g type_vars .> (" " ++)) type_inputs) ==>
      case type_inputs of
        [] -> id
        _  -> \s -> "(" ++ s ++ ")"
  other_type -> show other_type
  ) :: ValueType' -> [ (TypeName, String) ] -> Haskell

