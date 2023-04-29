module GenerationHelpers.ErrorMessages where
  
import Helpers (Error)
import ParsingTypes.LowLevel (ValueName, Literal)
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
  "last case of Int type must be \"... ->\" or \"some_name ->\"" ++
  "to catch all the remaining cases.\nInstead of: \"" ++ show literal ++ " ->\""
  ) :: Literal -> Error

cases_not_covered = ( \val_name val_names ->
  "the following cases are not covered: " ++ show (filter (/= val_name) val_names)
  ) :: ValueName -> [ ValueName ] -> Error
