module CodeGenerators.ErrorMessages where
  
import HaskellTypes.LowLevel
  ( ValueName, Abstractions )
import HaskellTypes.Types
  ( TypeName, BaseType, ValueType )
import HaskellTypes.Values

import Helpers
  ( Haskell )

type Error = String

-- All: LowLevel, Values

-- LowLevel:
-- literal_not_int_err, type_check_err, tuple_matching_err,
-- tuple_function_type_err, tuple_less_than_2_err,
-- tuple_values_types_lengths_dont_match_err,
-- abstractions_types_lengths_dont_match_err

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

-- Values:
-- tuple_fun_type_err, values_fields_lengths_dont_match_err, no_application_err
-- one_arg_applications_type_err, bv_type_inference_err, not_a_fun_err
-- argument_types_dont_match_err, too_many_abstractions_err,
-- many_args_types_dont_match_err, use_fields_not_fun_err, must_be_tuple_type_err,
-- specific_case_not_abstraction_err, name_type_and_value_lists_err 

tuple_fun_type_err = ( \vs vt ->
  show (Tuple vs) ++ " can't have type: " ++ show vt
  ) :: [ Value ] -> ValueType -> Error

values_fields_lengths_dont_match_err =
  "Length of tuple values and fields of the corresponding tuple_type must be " ++
  "the same"
  :: Error

no_application_err =
  "Application expression should have at least one application operator"
  :: Error

one_arg_applications_type_err = ( \oaa vt inferred_vt -> 
  "Could't match type: " ++ show vt ++ "\nwith type:" ++ show inferred_vt ++
  "In one argument applications expression: " ++ show oaa
  ) :: OneArgApplications -> ValueType -> ValueType -> Error

bv_type_inference_err =
  (("Cannot infer types for values inside parenthesis in one argument" ++
    " application, please define the following as an intermediate value: ") ++)
  . show
  :: ParenthesisValue -> Error

not_a_fun_err = ( \fun_vt val_vt ->
  "This is not a function type: " ++ show fun_vt ++
  "\nTrying to apply to argument of type: " ++ show val_vt
  ) :: ValueType -> ValueType -> Error

argument_types_dont_match_err = ( \vt_right abs_bt->
  "Argument types don't match for one argument function application.\n" ++
  "Types involved:\n\nFirst:\n  " ++ show vt_right ++ "Second:\n  " ++ show abs_bt
  ) :: ValueType -> BaseType -> Error

too_many_abstractions_err = ( \as bts ->
  "More abstractions than abstraction types.\n  Abstractions:  " ++ show as ++
  "\n  Types: " ++ show bts
  ) :: Abstractions -> [ BaseType ] -> Error

many_args_types_dont_match_err = ( \vt1 vt2 ->
  "Argument types don't match for many arguments function application.\n" ++
  "Types involved:\n\nFirst:\n  " ++ show vt1 ++ "Second:\n  " ++ show vt2
  ) :: ValueType -> ValueType -> Error

use_fields_not_fun_err = 
  ("use_fields should have function type but has: " ++) . show
  :: ValueType -> Error

must_be_tuple_type_err =
  ("use_fields abstraction should have tuple_type type but has:" ++) . show
  :: BaseType -> Error

specific_case_not_abstraction_err = ( \vt sc ->
  "Specific case: " ++ show sc ++ "\nShould have abstaction type but has: " ++
  show vt
  ) :: ValueType -> SpecificCase -> Error

name_type_and_value_lists_err = ( \ntavls ->
  "Name, type and value lists must have the same length but are:\n  " ++
  show ntavls
  ) :: NameTypeAndValueLists -> Error
