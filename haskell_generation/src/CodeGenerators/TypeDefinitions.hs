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

tt_field_g = ( \(FNameAndType field_name field_type) name_expr ->
  let
  get_func_name_hs = "get_" ++ show field_name
    :: Haskell
  get_func_type =
    FuncType $ InAndOutTs (cons_and_vars_conv name_expr) field_type
    :: ValType
  field_type_hs = type_g field_type name_expr
    :: Haskell
  in
  value_map_insert (VN get_func_name_hs) get_func_type >>
  return (get_func_name_hs ++ " :: " ++ field_type_hs)
  ) :: TTField -> TNameExpr -> Stateful Haskell

-- TupleTDef: tuple_t_def_g, ins_tuple_t_to_map, tuple_t_fields_g

tuple_t_def_g = ( \tuple_t_def@(TTNameExprAndFields name_expr _) ->
  ins_tuple_t_to_map tuple_t_def >>
  tuple_t_fields_g tuple_t_def >>= \fields_hs ->
  return $
    "\ndata " ++ show name_expr ++ " =\n  " ++ fields_hs ++ "\n\n" ++
    tuple_t_show_inst_g tuple_t_def
  ) :: TupleTDef -> Stateful Haskell

ins_tuple_t_to_map = (
  \(TTNameExprAndFields (TNameExpr type_name vars) fields) ->
  type_map_insert type_name (TupleType (length vars) fields)
  ) :: TupleTDef -> Stateful ()

tuple_t_fields_g = ( \(TTNameExprAndFields name_expr fields) ->
  fields==>mapM (flip tt_field_g name_expr) >>= \fields_hs ->
  return $
    "C" ++ show (get_cons name_expr) ++ " { " ++ intercalate ", " fields_hs ++ " }"
  ) :: TupleTDef -> Stateful Haskell

tuple_t_show_inst_g = ( \(TTNameExprAndFields (TNameExpr type_name _) fields) ->
  let
  field_names = map get_name fields
    :: [ ValueName ]
  field_names_hs = concatMap (show .> (" " ++)) field_names
    :: Haskell
  field_name_show_g = ( \name ->
    "\n    \"" ++ show name ++ " = \" ++ show " ++ show name
    ) :: ValueName -> Haskell
  fields_show_hs =
    intercalate " ++ \"\\n, \" ++ " (map field_name_show_g field_names)
    :: Haskell
  in
  "instance Show " ++ show type_name ++ " where\n" ++
  "  show = \\(C" ++ show type_name ++ field_names_hs ++ ") ->\n" ++
  "    \"( \" ++ " ++ fields_show_hs ++ " ++\n    \")\"\n"
  ) :: TupleTDef -> Haskell

-- TupleTypeDef: tuple_type_definition_g

tuple_type_definition_g =
  tuple_type_def_conv .> tuple_t_def_g
  :: TupleTypeDef -> Stateful Haskell

-- OrTCase: or_type_case_g

or_type_case_g = ( \(CNameAndMaybeInT case_name maybe_in_t) name_expr ->
  insert_to_or_t_cs case_name >>
  value_map_insert case_name (case_type_from maybe_in_t name_expr) >>
  return ("C" ++ show case_name ++ (in_t_hs_from maybe_in_t name_expr))
  ) :: OrTCase -> TNameExpr -> Stateful Haskell

case_type_from = ( \case
  Nothing -> cons_and_vars_conv
  Just in_t -> case_type_with_in_t in_t
  ) :: Maybe ValType -> TNameExpr -> ValType

case_type_with_in_t = ( \in_t name_expr -> case in_t of
  TypeApp (ConsAndInTs type_name []) ->
    type_name_to_type_var type_name name_expr ==> \case
      Nothing -> final_t_from in_t name_expr
      Just type_var -> final_t_from type_var name_expr
  _ -> final_t_from in_t name_expr
  ) :: ValType ->  TNameExpr -> ValType

final_t_from = ( \in_t name_expr -> 
  FuncType $ InAndOutTs in_t $ cons_and_vars_conv name_expr
  ) :: ValType -> TNameExpr -> ValType

in_t_hs_from = ( \maybe_in_t name_expr -> case maybe_in_t of
  Nothing -> ""
  Just in_t -> " " ++ type_g in_t name_expr
  ) :: Maybe ValType -> TNameExpr -> Haskell

-- OrTDef: or_t_def_g, or_type_cases_g

or_t_def_g = (
  \or_type_def@(OTNameExprAndCases
    name_expr@(TNameExpr type_name _)
    or_t_cs) -> 
  insert_or_type_to_map or_type_def >>
  mapM (flip or_type_case_g name_expr) or_t_cs >>= \cases_hs ->
  return $
    "\ndata " ++ show name_expr ++ " =\n  " ++
    intercalate " | " cases_hs ++ "\n\n" ++ ot_show_instance_g type_name or_t_cs
  ) :: OrTDef -> Stateful Haskell

insert_or_type_to_map = ( \(OTNameExprAndCases (TNameExpr cons vars) or_t_cs) -> 
  type_map_insert cons (OrType (length vars) or_t_cs)
  ) :: OrTDef -> Stateful ()

-- ot_show_instance_g

ot_show_instance_g = ( \type_name cases ->
  "instance Show " ++ show type_name ++ " where\n" ++
  "  show = \\case\n" ++ concatMap case_show_g cases
  ) :: TypeName -> [ OrTCase ] -> Haskell

case_show_g = ( \(CNameAndMaybeInT name maybe_in_t) ->
  case maybe_in_t of
    Nothing ->
      "    C" ++ show name ++ " -> \"" ++ show name ++ "\"\n"
    Just in_t ->
      "    C" ++ show name ++ " val -> \"" ++ show name ++ "<==\\n\" ++ show val\n"
  ) :: OrTCase -> Haskell

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

type_g = ( \val_t name_expr -> type_help_g val_t $ get_type_vars name_expr )
  :: ValType -> TNameExpr -> Haskell

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

type_help_g' = ( \cons vars t_ins ->
  let
  a = type_name_g cons vars ++ concatMap (flip type_help_g vars .> (" " ++)) t_ins
  in
  case t_ins of
    [] -> a
    _  -> "(" ++ a ++ ")"
  ) :: TypeName -> ValType -> [ ValType ] -> Haskell

type_name_g = ( \type_name vars->
  lookup type_name vars ==> \case
    Just type_var -> type_var
    Nothing -> show type_name
  ) :: TypeName -> [ (TypeName, String) ] -> Haskell

cons_and_vars_conv = ( \(TNameExpr type_name vars) ->
  let
  val_t_type_vars = map TypeVar [ 1..5 ]
    :: [ ValType ]
  in
  TypeApp $ ConsAndInTs type_name $ take (length vars) val_t_type_vars 
  ) :: TNameExpr -> ValType

type_name_to_type_var = ( \type_name name_expr ->
  lookup type_name (get_type_vars name_expr) ==> \case
    Just hs_t_var -> Just $ haskell_to_lcases_type_var hs_t_var
    Nothing -> Nothing
  ) :: TypeName -> TNameExpr -> Maybe ValType

haskell_to_lcases_type_var = ( \case
  "a" -> 1
  "b" -> 2
  "c" -> 3
  "d" -> 4
  "e" -> 5
  _ -> error "more than five type variables"
  ) .> TypeVar
  :: String -> ValType
