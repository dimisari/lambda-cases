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

-- TTField: tt_field_g, cons_and_vars_conv

tt_field_g = ( \(FNameAndType f_name f_type) cons_and_vars ->
  let
  get_func_name = "get_" ++ show f_name
    :: Haskell
  get_func_type = FuncType $ InAndOutTs (cons_and_vars_conv cons_and_vars) f_type
    :: ValType
  f_type_hs = type_g f_type cons_and_vars
    :: Haskell
  in
  value_map_insert (VN get_func_name) get_func_type >>
  return (get_func_name ++ " :: " ++ f_type_hs)
  ) :: TTField -> TConsAndVars -> Stateful Haskell

-- TupleTDef: tuple_t_def_g, tt_fields_g

tuple_t_def_g = (
  \(TTConsVarsAndFields
    cons_and_vars@(TConsAndVars type_name vars)
    fields) ->
  type_map_insert type_name (TupleType (length vars) fields) >>
  tt_fields_g fields cons_and_vars >>= \fields_hs ->
  return $
    "\ndata " ++ show cons_and_vars ++ " =\n  " ++
    fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTDef -> Stateful Haskell

tt_fields_g = ( \fields cons_and_vars ->
  fields==>mapM (flip tt_field_g cons_and_vars) >>= \fields_hs ->
  return $
    "C" ++ show (get_cons cons_and_vars) ++
    " { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ TTField ] -> TConsAndVars -> Stateful Haskell

-- TupleTypeDef: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conv .> tuple_t_def_g
  :: TupleTypeDef -> Stateful Haskell

-- OrTCase: or_type_case_g

or_type_case_g = ( \(CNameAndMaybeInT case_name maybe_in_t) cons_and_vars ->
  insert_to_or_t_cs case_name >>
  value_map_insert case_name (case_type_from maybe_in_t cons_and_vars) >>
  return ("C" ++ show case_name ++ (in_t_hs_from maybe_in_t cons_and_vars))
  ) :: OrTCase -> TConsAndVars -> Stateful Haskell

case_type_from = ( \case
  Nothing -> cons_and_vars_conv
  Just in_t -> case_type_with_in_t in_t
  ) :: Maybe ValType -> TConsAndVars -> ValType

case_type_with_in_t = ( \in_t cons_and_vars -> case in_t of
  TypeApp (ConsAndInTs type_name []) ->
    type_name_to_type_var type_name cons_and_vars ==> \case
      Nothing -> final_t_from in_t cons_and_vars
      Just type_var -> final_t_from type_var cons_and_vars
  _ -> final_t_from in_t cons_and_vars
  ) :: ValType ->  TConsAndVars -> ValType

final_t_from = ( \in_t cons_and_vars -> 
  FuncType $ InAndOutTs in_t $ cons_and_vars_conv cons_and_vars
  ) :: ValType -> TConsAndVars -> ValType

in_t_hs_from = ( \maybe_in_t cons_and_vars -> case maybe_in_t of
  Nothing -> ""
  Just in_t -> " " ++ type_g in_t cons_and_vars
  ) :: Maybe ValType -> TConsAndVars -> Haskell

-- OrTDef: or_t_def_g, or_type_cases_g

or_t_def_g = (
  \or_type_def@(OTConsVarsAndCases cons_and_vars or_t_cs) -> 
  insert_or_type_to_map or_type_def >>
  mapM (flip or_type_case_g cons_and_vars) or_t_cs >>= \cases_hs ->
  return $
    "\ndata " ++ show cons_and_vars ++ " =\n  " ++
    intercalate " | " cases_hs ++ "\n  deriving Show\n"
  ) :: OrTDef -> Stateful Haskell

insert_or_type_to_map = ( \(OTConsVarsAndCases (TConsAndVars cons vars) or_t_cs) -> 
  type_map_insert cons (OrType (length vars) or_t_cs)
  ) :: OrTDef -> Stateful ()

-- OrTypeDef: or_type_def_g

or_type_def_g = 
  or_type_def_conv .> or_t_def_g
  :: OrTypeDef -> Stateful Haskell

-- TypeDefinition: type_definition_g

type_definition_g = ( \case
  TupleTypeDef tuple_t_def -> tuple_type_definition_g tuple_t_def
  OrTypeDef or_t_def -> or_type_def_g or_t_def
  ) :: TypeDefinition -> Stateful Haskell

-- Helpers: type_g, cons_and_vars_conv

type_g = ( \val_t cons_and_vars ->
  type_help_g val_t $ get_type_vars cons_and_vars
  ) :: ValType -> TConsAndVars -> Haskell

type_help_g = ( \val_t vars ->
  case val_t of
    TypeApp (ConsAndInTs cons t_ins) ->
      ( type_name_g cons vars ++
        concatMap (flip type_help_g vars .> (" " ++)) t_ins
      ) ==> case t_ins of
        [] -> id
        _  -> \s -> "(" ++ s ++ ")"
    other_type -> show other_type
  ) :: ValType -> [ (TypeName, String) ] -> Haskell

type_name_g = ( \type_name vars->
  lookup type_name vars ==> \case
    Just type_var -> type_var
    Nothing -> show type_name
  ) :: TypeName -> [ (TypeName, String) ] -> Haskell

cons_and_vars_conv = ( \(TConsAndVars type_name vars) ->
  let
  val_t_type_vars = map TypeVar [ 1..5 ]
    :: [ ValType ]
  in
  TypeApp $ ConsAndInTs type_name $ take (length vars) val_t_type_vars 
  ) :: TConsAndVars -> ValType

type_name_to_type_var = ( \type_name cons_and_vars ->
  lookup type_name (get_type_vars cons_and_vars) ==> \case
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
