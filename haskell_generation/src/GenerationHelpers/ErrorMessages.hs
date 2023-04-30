module GenerationHelpers.ErrorMessages where
  
import Helpers (Error)
import ParsingTypes.LowLevel (ValueName, Literal, Abstraction)
import IntermediateTypes.Values (ApplicationTree)
import IntermediateTypes.Types (ValType)

-- All

lit_not_int_err = ( \val_type ->
  "\nInteger literal cannot have type: " ++ show val_type ++ "\n"
  ) :: ValType -> Error

type_check_err = ( \str val_type map_val_type -> 
  "\nValue: " ++ str ++ "\ncan't have both of these types:\n" ++
  show val_type ++ "\n" ++ show map_val_type ++ "\n"
  ) :: String -> ValType -> ValType -> Error

tuple_field_length_err =
  "\nLength of tuple values and fields of the corresponding tuple_type must be " ++
  "the same\n"
  :: Error

last_int_case_err = ( \literal ->
  "Last case of Int type must be \"... ->\" or \"some_name ->\"" ++
  "to catch all the remaining cases.\nInstead of: \"" ++ show literal ++ " ->\""
  ) :: Literal -> Error

cases_not_covered = ( \val_name val_names ->
  "The following cases are not covered: " ++ show (filter (/= val_name) val_names)
  ) :: ValueName -> [ ValueName ] -> Error

use_fields_err = ( \val_t ->
  "Can't use \"use_fields\" on the type: " ++ show val_t
  ) :: ValType -> Error

not_func_t_err = ( \abs val_t ->
  "Have input: " ++ show abs ++ "\nBut the type: " ++ show val_t ++
  "\nis not a function type"
  ) :: Abstraction -> ValType -> Error

cant_apply_non_func_err = ( \tree1 tree2 val_t ->
  "Trying to apply:\n" ++ show tree1 ++ 
  "\nto:\n" ++ show tree2 ++
  "\nBut it doesn't have a function type, instead the type is:\n" ++ show val_t
  ) :: ApplicationTree -> ApplicationTree -> ValType -> Error

not_or_type_case_err = ( \val_name val_t ->
  show val_name ++ " is not a case of the or_type: " ++ show val_t
  ) :: ValueName -> ValType -> Error

lit_in_or_type_case_err = ( \lit ->
  "Can't have literal: " ++ show lit ++ "in case of or_type"
  ) :: Literal -> Error

cases_expr_not_func_t_err = ( \t ->
  "cases expression has type:" ++ show t ++ "\nwhich is not a function type\n"
  ) :: ValType -> Error

cases_expr_wrong_in_t_err = ( \t ->
  "cases expression has input type:" ++ show t ++
  "\ninstead of: some or_type, Int or Char \n"
  ) :: ValType -> Error
