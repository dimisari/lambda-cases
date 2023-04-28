module GenerationHelpers.ErrorMessages where
  
import Helpers (Error)
import ParsingTypes.LowLevel (ValueName)
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
