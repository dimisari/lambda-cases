module CodeGenerators.ErrorMessages where
  
import HaskellTypes.LowLevel
  ( ValueName, Abstraction )
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

literal_not_int_err = ( \vt ->
  "\nInteger literal cannot have type: " ++ show vt ++ "\n"
  ) :: ValueType -> Error

type_check_err = ( \vn lookup_vt vt -> 
  "\nValue: " ++ show vn ++ "\nhas type: " ++ show lookup_vt ++ "\nnot: " ++
  show vt ++ "\n"
  ) :: ValueName -> ValueType -> ValueType -> Error

tuple_matching_err = ( \tn -> "\nTuple matching TypeName: " ++ show tn ++ "\n" )
  :: TypeName -> Error

tuple_function_type_err = ( \vt ->
  "\nMatching tuple but got function type: " ++ show vt ++ "\n"
  ) :: ValueType -> Error

tuple_less_than_2_err =
  "\nShould not have less than 2 in tuple matching\n"
  :: Error

tuple_values_types_lengths_dont_match_err =
  "\nTuple values and tuple types must be of the same length\n"
  :: Error

abstractions_types_lengths_dont_match_err =
  "\nAbstractions and abstraction types must be of the same length\n"
  :: Error

-- Values:
-- tuple_fun_type_err, values_fields_lengths_dont_match_err, no_application_err
-- one_arg_applications_type_err, bv_type_inference_err, not_a_fun_err
-- argument_types_dont_match_err, too_many_abstractions_err,
-- many_args_types_dont_match_err, use_fields_not_fun_err, must_be_tuple_type_err,
-- specific_case_not_abstraction_err, name_type_and_value_lists_err 

tuple_fun_type_err = ( \vs vt ->
  show (Tuple vs) ++ " can't have type: " ++ show vt ++ "\n"
  ) :: [ Value ] -> ValueType -> Error

values_fields_lengths_dont_match_err =
  "\nLength of tuple values and fields of the corresponding tuple_type must be " ++
  "the same\n"
  :: Error

no_application_err =
  "\nApplication expression should have at least one application operator\n"
  :: Error

one_arg_applications_type_err = ( \oaa vt inferred_vt -> 
  "\nCould't match type: " ++ show vt ++ "\nwith type:" ++ show inferred_vt ++
  "In one argument applications expression: " ++ show oaa ++ "\n"
  ) :: OneArgApplications -> ValueType -> ValueType -> Error

bv_type_inference_err = ( \pv ->
  "\nCannot infer types for values inside parenthesis in one argument" ++
  " application, please define the following as an intermediate value: " ++
  show pv ++ "\n"
  ) :: ParenthesisValue -> Error

not_a_fun_err = ( \fun_vt val_vt ->
  "\nThis is not a function type: " ++ show fun_vt ++
  "\nTrying to apply to argument of type: " ++ show val_vt ++ "\n"
  ) :: ValueType -> ValueType -> Error

argument_types_dont_match_err = ( \vt_right abs_bt->
  "\nArgument types don't match for one argument function application.\n" ++
  "Types involved:\n\nFirst:\n  " ++ show vt_right ++ "\nSecond:\n  " ++
  show abs_bt ++ "\n" ++ "\n"
  ) :: ValueType -> BaseType -> Error

too_many_abstractions_err = ( \as bts ->
  "\nMore abstractions than abstraction types.\n  Abstractions:  " ++ show as ++
  "\n  Types: " ++ show bts ++ "\n"
  ) :: [ Abstraction ] -> [ BaseType ] -> Error

many_args_types_dont_match_err = ( \vt1 vt2 ->
  "\nArgument types don't match for many arguments function application.\n" ++
  "Types involved:\n\nFirst:\n  " ++ show vt1 ++ "\nSecond:\n  " ++ show vt2 ++
  "\n" 
  ) :: ValueType -> ValueType -> Error

use_fields_not_fun_err = ( \vt ->
  "\nuse_fields should have function type but has: " ++ show vt ++ "\n"
  ) :: ValueType -> Error

must_be_tuple_type_err = ( \bt ->
  "\nuse_fields abstraction should have tuple_type type but has:" ++ show bt ++
  "\n"
  ) :: BaseType -> Error

specific_case_not_abstraction_err = ( \vt sc ->
  "\nSpecific case: " ++ show sc ++ "\nShould have abstaction type but has: " ++
  show vt ++ "\n"
  ) :: ValueType -> SpecificCase -> Error

name_type_and_value_lists_err = ( \ntavls ->
  "\nName, type and value lists must have the same length but are:\n  " ++
  show ntavls ++ "\n"
  ) :: NameTypeAndValueLists -> Error
