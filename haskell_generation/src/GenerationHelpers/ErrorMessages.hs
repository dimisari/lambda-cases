module GenerationHelpers.ErrorMessages where
  
import Data.List (intercalate)
import Helpers ((.>), Error, not_caught)

import ParsingTypes.LowLevel (ValueName, Literal, Abstraction)
import ParsingTypes.OperatorValues (OpExpr)
import ParsingTypes.Types (TypeName)

import IntermediateTypes.Values
import IntermediateTypes.Types (ValType)

-- All

-- map errs

no_def_for_val_err = ( \val_name ->
  not_caught $ "No definition for value: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

type_exist_err = ( \type_name ->
  not_caught $ "Type of the same name already defined: " ++ show type_name ++ "\n"
  ) :: TypeName -> Error

no_def_for_type_err = ( \type_name ->
  not_caught $ "No definition for type: " ++ show type_name ++ "\n"
  ) :: TypeName -> Error

-- tuple errs

wrong_type_for_tuple_err = ( \op_exprs t ->
  not_caught $
  "The tuple:" ++ op_exprs_to_string op_exprs ++
  "\ncan't have the type: " ++ show t ++ "\n"
  ) :: [ OpExpr ] -> ValType -> Error

or_type_for_tuple_err = ( \op_exprs ->
  not_caught $
  "The tuple:" ++ op_exprs_to_string op_exprs ++
  "\ncan't have an or_type as its type\n"
  ) :: [ OpExpr ] -> Error

int_for_tuple_err = ( \op_exprs ->
  not_caught $ "The tuple:" ++ op_exprs_to_string op_exprs ++ "\ncan't be an Int\n"
  ) :: [ OpExpr ] -> Error

char_for_tuple_err = ( \op_exprs ->
  not_caught $ "The tuple:" ++ op_exprs_to_string op_exprs ++ "\ncan't be a Char\n"
  ) :: [ OpExpr ] -> Error

tuple_prod_t_lengths_err = ( \op_exprs val_type ->
  not_caught $
  "Length of tuple does not match length of product type:\n" ++
  op_exprs_to_string op_exprs ++ "\n" ++
  show val_type ++ "\n"
  ) :: [ OpExpr ] -> ValType -> Error

op_exprs_to_string =
  map show .> intercalate ", " .> ("(" ++) .> (++ ")")
  :: [ OpExpr ] -> String

-- equality err

equality_not_bool_err = ( \val_t ->
  not_caught $ "Equality must be of type Bool, instead it has the type: " ++
  show val_t ++ "\n"
  ) :: ValType -> Error

-- cases errs

wrong_int_case_err = ( \val_name ->
  not_caught $ "case of Int type can't be: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

diplicate_case_err = ( \val_name ->
  not_caught $ "Duplicate or_type case: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

--

lit_not_int_err = ( \val_type ->
  not_caught $ "Integer literal cannot have type: " ++ show val_type ++ "\n"
  ) :: ValType -> Error

type_check_err = ( \str val_type map_val_type -> 
  not_caught $ "Value: " ++ str ++ "\ncan't have both of these types:\n" ++
  show val_type ++ "\n" ++ show map_val_type ++ "\n"
  ) :: String -> ValType -> ValType -> Error

tuple_field_length_err =
  not_caught $
  "\nLength of tuple values and fields of the corresponding tuple_type must be " ++
  "the same\n"
  :: Error

last_int_case_err = ( \literal ->
  not_caught $ "Last case of Int type must be \"... ->\" or \"some_name ->\"" ++
  "to catch all the remaining cases.\nInstead of: \"" ++ show literal ++ " ->\"\n"
  ) :: Literal -> Error

cases_not_covered = ( \val_name val_names ->
  not_caught $ "The following cases are not covered: " ++
  show (filter (/= val_name) val_names) ++ "\n"
  ) :: ValueName -> [ ValueName ] -> Error

use_fields_err = ( \val_t ->
  not_caught $ "Can't use \"use_fields\" on the type: " ++ show val_t ++ "\n"
  ) :: ValType -> Error

not_func_t_err = ( \abs val_t ->
  not_caught $ "Have input: " ++ show abs ++ "\nBut the type: " ++ show val_t ++
  "\nis not a function type\n"
  ) :: Abstraction -> ValType -> Error

cant_apply_non_func_err2 = ( \tree1 tree2 val_t ->
  not_caught $ "Trying to apply:\n" ++ show tree1 ++ 
  "\nto:\n" ++ show tree2 ++
  "\nBut it doesn't have a function type, instead the type is:\n" ++ show val_t
  ++ "\n"
  ) :: ApplicationTree -> ApplicationTree -> ValType -> Error

not_or_type_case_err = ( \val_name val_t ->
  not_caught $ show val_name ++ " is not a case of the or_type: " ++ show val_t
  ++ "\n"
  ) :: ValueName -> ValType -> Error

lit_in_or_type_case_err = ( \lit ->
  not_caught $ "Can't have literal: " ++ show lit ++ "in case of or_type" ++ "\n"
  ) :: Literal -> Error

cases_expr_not_func_t_err = ( \t ->
  not_caught $ "cases expression has type:" ++ show t ++
  "\nwhich is not a function type\n"
  ) :: ValType -> Error

cases_expr_wrong_in_t_err = ( \t ->
  not_caught $ "cases expression has input type:" ++ show t ++
  "\ninstead of: some or_type, Int or Char\n"
  ) :: ValType -> Error

