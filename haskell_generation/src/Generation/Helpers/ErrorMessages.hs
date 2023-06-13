module Generation.Helpers.ErrorMessages where
  
import Data.List (intercalate)
import Helpers ((.>))

import Parsing.Types.LowLevel (ValueName, Literal, Abstraction)
import Parsing.Types.OperatorValues (OpExpr)
import Parsing.Types.Types (TypeName)

import IntermediateTypes.Values
import IntermediateTypes.Types (ValType)

-- All

-- map errs

no_def_for_val_err = ( \val_name ->
  not_caught $ add_dummy_err_type $ 
  "No definition for value: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

type_exist_err = ( \type_name ->
  not_caught $ add_dummy_err_type $ 
  "Type of the same name already defined: " ++ show type_name ++ "\n"
  ) :: TypeName -> Error

no_def_for_type_err = ( \type_name ->
  not_caught $ add_dummy_err_type $ 
  "No definition for type: " ++ show type_name ++ "\n"
  ) :: TypeName -> Error

-- tuple errs

wrong_type_for_tuple_err = ( \op_exprs t ->
  not_caught $ add_dummy_err_type $
  "The tuple:" ++ op_exprs_to_string op_exprs ++
  "\ncan't have the type: " ++ show t ++ "\n"
  ) :: [ OpExpr ] -> ValType -> Error

or_type_for_tuple_err = ( \op_exprs ->
  not_caught $ add_dummy_err_type $
  "The tuple:" ++ op_exprs_to_string op_exprs ++
  "\ncan't have an or_type as its type\n"
  ) :: [ OpExpr ] -> Error

int_for_tuple_err = ( \op_exprs ->
  not_caught $ add_dummy_err_type $ 
  "The tuple:" ++ op_exprs_to_string op_exprs ++ "\ncan't be an Int\n"
  ) :: [ OpExpr ] -> Error

char_for_tuple_err = ( \op_exprs ->
  not_caught $ add_dummy_err_type $ 
  "The tuple:" ++ op_exprs_to_string op_exprs ++ "\ncan't be a Char\n"
  ) :: [ OpExpr ] -> Error

tuple_prod_t_lengths_err = ( \op_exprs val_type ->
  not_caught $ add_dummy_err_type $
  "Length of tuple does not match length of product type:\n" ++
  op_exprs_to_string op_exprs ++ "\n" ++
  show val_type ++ "\n"
  ) :: [ OpExpr ] -> ValType -> Error

-- 

op_exprs_to_string =
  map show .> intercalate ", " .> ("(" ++) .> (++ ")")
  :: [ OpExpr ] -> String

-- equality err

equality_not_bool_err = ( \val_t ->
  not_caught $ add_dummy_err_type $ 
  "Equality must be of type Bool, instead it has the type: " ++
  show val_t ++ "\n"
  ) :: ValType -> Error

-- cases errs

wrong_int_case_err = ( \val_name ->
  not_caught $ add_dummy_err_type $ 
  "case of Int type can't be: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

duplicate_case_err = ( \val_name ->
  not_caught $ add_dummy_err_type $ 
  "Duplicate or_type case: " ++ show val_name ++ "\n"
  ) :: ValueName -> Error

duplicate_int_case_err = ( \i ->
  not_caught $ add_dummy_err_type $ 
  "Duplicate Int case: " ++ show i ++ "\n"
  ) :: Int -> Error

--

lit_not_int_err = ( \val_type ->
  not_caught $ add_dummy_err_type $ 
  "Integer literal cannot have type: " ++ show val_type ++ "\n"
  ) :: ValType -> Error

type_check_err = ( \str val_type map_val_type -> 
  not_caught $ add_dummy_err_type $ 
  "Value: " ++ str ++ "\ncan't have both of these types:\n" ++
  show val_type ++ "\n" ++ show map_val_type ++ "\n"
  ) :: String -> ValType -> ValType -> Error

tuple_field_length_err =
  not_caught $ add_dummy_err_type $ 
  "\nLength of tuple values and fields of the corresponding tuple_type must be " ++
  "the same\n"
  :: Error

last_int_case_err = ( \literal ->
  not_caught $ add_dummy_err_type $ 
  "Last case of Int type must be \"... ->\" or \"some_name ->\"" ++
  "to catch all the remaining cases.\nInstead of: \"" ++ show literal ++ " ->\"\n"
  ) :: Literal -> Error

cases_not_covered_err = ( \val_names ->
  not_caught $ add_dummy_err_type $ 
  "The following cases are not covered: " ++ show val_names ++ "\n"
  ) :: [ ValueName ] -> Error

cases_not_covered_err_cont = ( \type_name ->
  "From the or_type: " ++ show type_name ++ "\n"
  ) :: TypeName -> String

use_fields_err = ( \val_t ->
  not_caught $ add_dummy_err_type $ 
  "Can't use \"use_fields\" on the type: " ++ show val_t ++ "\n"
  ) :: ValType -> Error

not_func_t_err = ( \abs val_t ->
  not_caught $ add_dummy_err_type $ 
  "Have input: " ++ show abs ++ "\nBut the type: " ++ show val_t ++
  "\nis not a function type\n"
  ) :: Abstraction -> ValType -> Error

cant_apply_non_func_err2 = ( \tree1 tree2 val_t ->
  not_caught $ add_dummy_err_type $ 
  "Trying to apply:\n" ++ show tree1 ++ 
  "\nto:\n" ++ show tree2 ++
  "\nBut it doesn't have a function type, instead the type is:\n" ++ show val_t
  ++ "\n"
  ) :: ApplicationTree -> ApplicationTree -> ValType -> Error

not_or_type_case_err = ( \val_name val_t ->
  not_caught $ add_dummy_err_type $ 
  show val_name ++ " is not a case of the or_type: " ++ show val_t
  ++ "\n"
  ) :: ValueName -> ValType -> Error

not_or_type_case_err_new = ( \val_name ->
  not_caught $ add_err_type NotOrCaseErr $ 
  show val_name ++ " is not a case of the or_type: "
  ) :: ValueName -> Error

lit_in_or_type_case_err = ( \lit ->
  not_caught $ add_dummy_err_type $ 
  "Can't have literal: " ++ show lit ++ "in case of or_type" ++ "\n"
  ) :: Literal -> Error

cases_expr_not_func_t_err = ( \t ->
  not_caught $ add_dummy_err_type $ 
  "cases expression has type:" ++ show t ++
  "\nwhich is not a function type\n"
  ) :: ValType -> Error

cases_expr_wrong_in_t_err = ( \t ->
  not_caught $ add_dummy_err_type $ 
  "cases expression has input type:" ++ show t ++
  "\ninstead of: some or_type, Int or Char\n"
  ) :: ValType -> Error

-- 

data ErrorType = 
  DummyErr |
  NotOrCaseErr

type Error = (Bool, ErrorType, String)

not_caught = ( \(e_t, s) -> (False, e_t, s) )
  :: (ErrorType, String) -> Error

add_err_type = ( \e_t s -> (e_t, s) )
  :: ErrorType -> String -> (ErrorType, String)

add_dummy_err_type = add_err_type DummyErr
  :: String -> (ErrorType, String)

