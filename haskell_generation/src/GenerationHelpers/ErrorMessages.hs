module GenerationHelpers.ErrorMessages where
  
import Helpers (Haskell, Error)

import ParsingTypes.LowLevel (ValueName, Abstraction)
import ParsingTypes.Types (TypeName)
import ParsingTypes.OperatorValues
import ParsingTypes.Values

import IntermediateTypes.Types (ValueType')

-- All: LowLevel, Values

-- LowLevel:
-- literal_not_int_err, type_check_err, tuple_abstraction_err,
-- tuple_function_type_err, tuple_less_than_2_err,
-- tuple_values_types_lengths_dont_match_err,
-- abstractions_types_lengths_dont_match_err

literal_not_int_err = ( \val_type ->
  "\nInteger literal cannot have type: " ++ show val_type ++ "\n"
  ) :: ValueType' -> Error

type_check_err = ( \value_name val_type map_val_type -> 
  "\nValue: " ++ show value_name ++ "\ncan't have both of these types:\n" ++
  show val_type ++ "\n" ++ show map_val_type ++ "\n"
  ) :: ValueName -> ValueType' -> ValueType' -> Error

tuple_abstraction_err = ( \tn -> "\nTuple matching TypeName: " ++ show tn ++ "\n" )
  :: TypeName -> Error

tuple_function_type_err = ( \val_type ->
  "\nMatching tuple but got function type: " ++ show val_type ++ "\n"
  ) :: ValueType' -> Error

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

tuple_fun_type_err = ( \lovs val_type ->
  show lovs ++ " can't have type: " ++ show val_type ++ "\n"
  ) :: [ InputOpExpr ] -> ValueType' -> Error

values_fields_lengths_dont_match_err =
  "\nLength of tuple values and fields of the corresponding tuple_type must be " ++
  "the same\n"
  :: Error

no_application_err =
  "\nApplication expression should have at least one application operator\n"
  :: Error

one_arg_applications_type_err = ( \oaa val_type inferred_vt -> 
  "\nCould't match type: " ++ show val_type ++ "\nwith type:" ++ show inferred_vt ++
  "In one argument applications expression: " ++ show oaa ++ "\n"
  ) :: FuncAppChain -> ValueType' -> ValueType' -> Error

bv_type_inference_err = ( \pv ->
  "\nCannot infer types for values inside parenthesis in one argument" ++
  " application, please define the following as an intermediate value: " ++
  show pv ++ "\n"
  ) :: Parenthesis -> Error

not_a_fun_err = ( \fun_vt val_vt ->
  "\nThis is not a function type: " ++ show fun_vt ++
  "\nTrying to apply to argument of type: " ++ show val_vt ++ "\n"
  ) :: ValueType' -> ValueType' -> Error

-- argument_types_dont_match_err = ( \vt_right abs_bt->
--   "\nArgument types don't match for one argument function application.\n" ++
--   "Types involved:\n\nFirst:\n  " ++ show vt_right ++ "\nSecond:\n  " ++
--   show abs_bt ++ "\n" ++ "\n"
--   ) :: ValueType' -> BaseType -> Error

-- too_many_abstractions_err = ( \as bts ->
--   "\nMore abstractions than abstraction types.\n  Abstractions:  " ++ show as ++
--   "\n  Types: " ++ show bts ++ "\n"
--   ) :: [ Abstraction ] -> [ BaseType ] -> Error

many_args_types_dont_match_err = ( \vt1 vt2 ->
  "\nArgument types don't match for many arguments function application.\n" ++
  "Types involved:\n\nFirst:\n  " ++ show vt1 ++ "\nSecond:\n  " ++ show vt2 ++
  "\n" 
  ) :: ValueType' -> ValueType' -> Error

use_fields_not_fun_err = ( \val_type ->
  "\nuse_fields should have function type but has: " ++ show val_type ++ "\n"
  ) :: ValueType' -> Error

-- must_be_tuple_type_err = ( \bt ->
--   "\nuse_fields abstraction should have tuple_type type but has:" ++ show bt ++
--   "\n"
--   ) :: BaseType -> Error

specific_case_not_abstraction_err = ( \val_type sc ->
  "\nSpecific case: " ++ show sc ++ "\nShould have abstaction type but has: " ++
  show val_type ++ "\n"
  ) :: ValueType' -> SpecificCase -> Error

name_type_and_value_lists_err = ( \ntavls ->
  "\nName, type and value lists must have the same length but are:\n  " ++
  show ntavls ++ "\n"
  ) :: Values -> Error
