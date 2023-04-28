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
-- TTField, TupleTDef, TupleTypeDef,
-- OrTCase, OrTDef, OrTypeDef
-- TypeDefinition

-- TTField: field_g, get_function_type, cons_and_vars_to_val_type, type_name_g

field_g = ( \(FNameAndType field_name field_type) type_cons_and_vars ->
  let
  get_function_hs = "get_" ++ show field_name
    :: Haskell
  field_type_hs = type_g field_type $ get_type_vars type_cons_and_vars
    :: Haskell
  in
  value_map_insert
    (VN get_function_hs) (get_function_type type_cons_and_vars field_type) >>
  return (get_function_hs ++ " :: " ++ field_type_hs)
  ) :: TTField -> TConsAndVars -> Stateful Haskell

get_function_type = ( \type_cons_and_vars field_type ->
  FuncType $
    InAndOutTs (cons_and_vars_to_val_type type_cons_and_vars) field_type
  ) :: TConsAndVars -> ValType -> ValType

type_name_g = ( \type_name type_vars->
  lookup type_name type_vars ==> \case
    Just type_var -> type_var
    Nothing -> show type_name
  ) :: TypeName -> [ (TypeName, String) ] -> Haskell

-- TupleTDef: tuple_type_definition'_g, fields_g

tuple_type_definition'_g = (
  \(TTConsVarsAndFields
    type_cons_and_vars@(TConsAndVars type_name type_vars)
    fields) ->
  type_map_insert type_name (TupleType (length type_vars) fields) >>
  fields_g fields type_cons_and_vars >>= \fields_hs ->
  return $
    "\ndata " ++ show type_cons_and_vars ++ " =\n  " ++
    fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTDef -> Stateful Haskell

fields_g = ( \fields type_cons_and_vars ->
  fields==>mapM (flip field_g type_cons_and_vars) >>= \fields_hs ->
  return $
    "C" ++ show (get_cons type_cons_and_vars) ++
    " { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ TTField ] -> TConsAndVars -> Stateful Haskell

-- TupleTypeDef: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conv .> tuple_type_definition'_g
  :: TupleTypeDef -> Stateful Haskell

-- OrTCase: or_type_case_g

or_type_case_g = (
  \(CNameAndMaybeInT case_name maybe_input_t) type_cons_and_vars ->
  insert_to_or_t_cs case_name >>
  value_map_insert case_name (case_type_from maybe_input_t type_cons_and_vars) >>
  return
    ( "C" ++ show case_name ++
      (input_type_hs_from maybe_input_t type_cons_and_vars)
    )
  ) :: OrTCase -> TConsAndVars -> Stateful Haskell

case_type_from = ( \maybe_input_t type_cons_and_vars -> case maybe_input_t of
  Nothing -> cons_and_vars_to_val_type type_cons_and_vars
  Just input_type -> case_type_with_input_t input_type type_cons_and_vars
  ) :: Maybe ValType -> TConsAndVars -> ValType

case_type_with_input_t = ( \input_t type_cons_and_vars -> case input_t of
  TypeApp (ConsAndInTs type_name []) ->
    type_name_to_type_var type_name type_cons_and_vars ==> \case
      Nothing -> final_type input_t type_cons_and_vars
      Just type_var -> final_type type_var type_cons_and_vars
  _ -> final_type input_t type_cons_and_vars
  ) :: ValType ->  TConsAndVars -> ValType

final_type = ( \input_t type_cons_and_vars -> 
  FuncType $
    InAndOutTs input_t $ cons_and_vars_to_val_type type_cons_and_vars
  ) :: ValType -> TConsAndVars -> ValType

input_type_hs_from = ( \maybe_input_t type_cons_and_vars -> case maybe_input_t of
  Nothing -> ""
  Just input_type -> " " ++ type_g input_type (get_type_vars type_cons_and_vars)
  ) :: Maybe ValType -> TConsAndVars -> Haskell

-- OrTDef: or_type_def'_g, or_type_cases_g

or_type_def'_g = (
  \or_type_def@(OTConsVarsAndCases type_cons_and_vars or_type_cases) -> 
  insert_or_type_to_map or_type_def >>
  mapM (flip or_type_case_g type_cons_and_vars) or_type_cases >>= \cases_hs ->
  return $
    "\ndata " ++ show type_cons_and_vars ++ " =\n  " ++
    intercalate " | " cases_hs ++ "\n  deriving Show\n"
  ) :: OrTDef -> Stateful Haskell

insert_or_type_to_map = (
  \(OTConsVarsAndCases (TConsAndVars type_name type_vars) or_type_cases) -> 
  type_map_insert type_name (OrType (length type_vars) or_type_cases)
  ) :: OrTDef -> Stateful ()

-- OrTypeDef: or_type_def_g

or_type_def_g = 
  or_type_def_conv .> or_type_def'_g
  :: OrTypeDef -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDef tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDef or_t_def -> or_type_def_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell

-- Helpers: type_g, cons_and_vars_to_val_type

type_g = ( \value_type type_vars -> case value_type of
  TypeApp (ConsAndInTs type_name type_inputs) ->
    (type_name_g type_name type_vars ++
    concatMap (flip type_g type_vars .> (" " ++)) type_inputs) ==>
      case type_inputs of
        [] -> id
        _  -> \s -> "(" ++ s ++ ")"
  other_type -> show other_type
  ) :: ValType -> [ (TypeName, String) ] -> Haskell

cons_and_vars_to_val_type = ( \(TConsAndVars type_name type_vars) ->
  let
  val_t_type_vars = map TypeVar [ 1..5 ]
    :: [ ValType ]
  in
  TypeApp $
    ConsAndInTs type_name $ take (length type_vars) val_t_type_vars 
  ) :: TConsAndVars -> ValType

type_name_to_type_var = ( \type_name type_cons_and_vars ->
  lookup type_name (get_type_vars type_cons_and_vars) ==> \case
    Just hs_t_var -> Just $ haskell_to_lcases_type_var hs_t_var
    Nothing -> Nothing
  ) :: TypeName -> TConsAndVars -> Maybe ValType

haskell_to_lcases_type_var = ( \case
  "a" -> 1
  "b" -> 2
  "c" -> 3
  "d" -> 4
  "e" -> 5
  _ -> error "more than five type variables"
  ) .> TypeVar
  :: String -> ValType
