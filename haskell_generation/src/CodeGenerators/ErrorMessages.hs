module CodeGenerators.ErrorMessages where
  
import HaskellTypes.LowLevel
  ( ValueName(VN), Abstractions )
import HaskellTypes.Types
  ( TypeName, BaseType, ValueType )
import HaskellTypes.Values

import Helpers
  ( Haskell )

type Error = String

-- All: LowLevel, Types, Values

-- LowLevel
literal_err_msg = ( "Integer literal cannot have type: " ++) . show
  :: ValueType -> Error

type_check_value_name_err_msg = ( \vn lookup_vt vt -> 
  "Value: " ++ show vn ++ "\nhas type: " ++ show lookup_vt ++ "\nnot: " ++ show vt
  ) :: ValueName -> ValueType -> ValueType -> Error

tuple_matching_err_msg = ("tuple matching TypeName :" ++) . show
  :: TypeName -> Error

value_type_tuple_matching_err_msg = ("trying to match tuple but got type: " ++) . show
  :: ValueType -> Error

value_types_tuple_matching_err_msg1 = "should not have less than 2 in tuple matching"
  :: Error

value_types_tuple_matching_err_msg2 =
  "tuple values and tuple types must be of the same length"
  :: Error

abstractions_err_msg = "abstractions and abstraction types must be of the same length"
  :: Error

-- Types 
tuple_type_err_msg = "tuple_type of the same name already defined"
  :: Error

-- Values
vt_values_err_msg = ( \vs vt -> show (Tuple vs) ++ " can't have type: " ++ show vt)
  :: [ Value ] -> ValueType -> Error

vts_values_err_msg = "length of values and types in tuple must be the same"
  :: Error

type_not_found_err_msg = ("Did not find a definition for type: " ++) . show
  :: TypeName -> Error

tn_values_err_msg =
  "length of tuple result must be the same as the number of fields of the " ++
  "corresponding tuple_type"
  :: Error

one_arg_applications_err_msg1 =
  "application expression should have at least one application operator"
  :: Error

one_arg_applications_err_msg2 = 
  ("type inference for one argument applications failed in: " ++) . show
  :: OneArgApplications -> Error

bv_type_inference_err_msg =
  (("Cannot infer types for values inside parenthesis in one argument application" ++
    ", please define the following as an intermediate value: ") ++) . show
  :: ParenthesisValue -> Error

value_not_found_err_msg = ( "Could not find value: " ++) . show
  :: ValueName -> Error

one_arg_application_err_msg_1 =  
  ("Cannot apply argument to something that does not have a function type. \nType : "
  ++) . show
  :: ValueType -> Error

one_arg_application_err_msg_2 = ( \vt_right abs_bt hs_left hs_right->
  "types don't match for one argument function application. types involved:\n  " ++
  show vt_right ++ "\n  " ++ show abs_bt ++ "\nhaskell:\n  " ++ hs_left ++ hs_right
  ) :: ValueType -> BaseType -> Haskell -> Haskell -> Error

many_args_arg_value_err_msg = ( \as bts ->
  "More abstractions than abstraction types.\n  Abstractions:  " ++ show as ++
  "\n  Types: " ++ show bts
  ) :: Abstractions -> [ BaseType ] -> Error

many_args_application_err_msg = ( \vt1 vt2 ->
  "types don't match for many arguments function application. types involved:\n  " ++
  show vt1  ++ "\n  " ++ show vt2
  ) :: ValueType -> ValueType -> Error

use_fields_err_msg1 = "use_fields should have function type"
  :: Error

use_fields_err_msg2 = "use_fields abstraction should have tuple_type type"
  :: Error

specific_case_err_msg = ( \vt sc ->
  "case should have abstaction type" ++ show vt ++ show sc
  ) :: ValueType -> SpecificCase -> Error

name_type_and_value_lists_err_msg =
  "name_type_and_value_lists_g: lists must have the same length"
  :: Error
