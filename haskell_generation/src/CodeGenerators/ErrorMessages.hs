module CodeGenerators.ErrorMessages where
  
import HaskellTypes.LowLevel
  ( ValueName, Abstractions )
import HaskellTypes.Types
  ( TypeName, BaseType, ValueType )
import HaskellTypes.Values

import Helpers
  ( Haskell )

type Error = String

-- All: LowLevel, Types, Values

-- LowLevel
literal_not_int_err = ( "Integer literal cannot have type: " ++) . show
  :: ValueType -> Error

type_check_err = ( \vn lookup_vt vt -> 
  "Value: " ++ show vn ++ "\nhas type: " ++ show lookup_vt ++ "\nnot: " ++ show vt
  ) :: ValueName -> ValueType -> ValueType -> Error

tuple_matching_err = ("Tuple matching TypeName: " ++) . show
  :: TypeName -> Error

tuple_function_type_err =
  ("Matching tuple but got function type: " ++) . show
  :: ValueType -> Error

tuple_less_than_2_err =
  "Should not have less than 2 in tuple matching"
  :: Error

tuple_values_types_lengths_dont_match_err =
  "Tuple values and tuple types must be of the same length"
  :: Error

abstractions_types_lengths_dont_match_err =
  "Abstractions and abstraction types must be of the same length"
  :: Error

-- Values
tuple_fun_type_err = ( \vs vt ->
  show (Tuple vs) ++ " can't have type: " ++ show vt
  ) :: [ Value ] -> ValueType -> Error

vts_tuple_values_err = "Length of values and types in tuple must be the same"
  :: Error

type_not_found_err = ("Did not find a definition for type: " ++) . show
  :: TypeName -> Error

tn_values_err =
  "Length of tuple result must be the same as the number of fields of the " ++
  "corresponding tuple_type"
  :: Error

one_arg_applications_err1 =
  "Application expression should have at least one application operator"
  :: Error

one_arg_applications_err2 = 
  ("Type inference for one argument applications failed in: " ++) . show
  :: OneArgApplications -> Error

bv_type_inference_err =
  (("Cannot infer types for values inside parenthesis in one argument" ++
    " application, please define the following as an intermediate value: ") ++)
  . show
  :: ParenthesisValue -> Error

value_not_found_err = ( "Could not find value: " ++) . show
  :: ValueName -> Error

one_arg_application_err_1 =  
  (("Cannot apply argument to something that does not have a function type." ++
    " \nType : ") ++) . show
  :: ValueType -> Error

one_arg_application_err_2 = ( \vt_right abs_bt hs_left hs_right->
  "Types don't match for one argument function application. types involved:" ++
  "\n  " ++ show vt_right ++ "\n  " ++ show abs_bt ++ "\nhaskell:\n  " ++
  hs_left ++ hs_right
  ) :: ValueType -> BaseType -> Haskell -> Haskell -> Error

many_args_arg_value_err = ( \as bts ->
  "More abstractions than abstraction types.\n  Abstractions:  " ++ show as ++
  "\n  Types: " ++ show bts
  ) :: Abstractions -> [ BaseType ] -> Error

many_args_application_err = ( \vt1 vt2 ->
  "Types don't match for many arguments function application. types" ++
  " involved:\n  " ++ show vt1  ++ "\n  " ++ show vt2
  ) :: ValueType -> ValueType -> Error

use_fields_err1 = "use_fields should have function type"
  :: Error

use_fields_err2 = "use_fields abstraction should have tuple_type type"
  :: Error

specific_case_err = ( \vt sc ->
  "case should have abstaction type" ++ show vt ++ show sc
  ) :: ValueType -> SpecificCase -> Error

name_type_and_value_lists_err =
  "name_type_and_value lists must have the same length"
  :: Error
