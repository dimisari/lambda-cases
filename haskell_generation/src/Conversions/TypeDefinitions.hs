module Conversions.TypeDefinitions where

import ParsingTypes.Types (TypeName)
import ParsingTypes.TypeDefinitions
import IntermediateTypes.TypeDefinitions
import Conversions.Types (val_type_conv)

-- All: ValType, Type Definitions

-- TypeConsAndVars: cons_and_type_vars_conversion

cons_and_type_vars_conversion = ( 
  \(TypeConsAndVars constructor_name left_type_vars right_type_vars) ->
  TypeConsAndVars' constructor_name $
    flip zip [ "a", "b", "c", "d", "e" ] $
    left_type_vars_conversion left_type_vars ++
    right_type_vars_conversion right_type_vars
  ) :: TypeConsAndVars -> TypeConsAndVars'

left_type_vars_conversion = ( \case
  NoLeftTypeVars -> []
  OneLeftTypeVar type_name -> [ type_name ]
  ManyLeftTypeVars many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: LeftTypeVars -> [ TypeName ]

right_type_vars_conversion = ( \case
  NoRightTypeVars -> []
  OneRightTypeVar type_name -> [ type_name ]
  ManyRightTypeVars many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: RightTypeVars -> [ TypeName ]

many_t_names_in_paren_conv = ( \(ParenTypeNames n1 n2 ns) -> n1 : n2 : ns)
  :: ManyTNamesInParen -> [ TypeName ]

-- Type Definitions: 
-- field_conversion, tuple_type_def_conversion,
-- or_type_case_conversion, or_type_def_conversion

field_conversion = ( \(NameAndType value_name value_type) ->
  NameAndType' value_name (val_type_conv value_type)
  ) :: Field -> Field'

tuple_type_def_conversion = ( \(ConsVarsAndFields type_application fields) ->
  ConsVarsAndFields'
    (cons_and_type_vars_conversion type_application)
    (map field_conversion fields)
  ) :: TupleTypeDef -> TupleTypeDef'

or_type_case_conversion = ( \(NameAndMaybeInT value_name maybe_value_type) ->
  NameAndMaybeInT' value_name (val_type_conv <$> maybe_value_type)
  ) :: OrTypeCase -> OrTypeCase'

or_type_def_conversion = (
  \(ConsVarsAndCases type_application case1 case2 cases) ->
  ConsVarsAndCases'
    (cons_and_type_vars_conversion type_application)
    (map or_type_case_conversion $ case1 : case2 : cases)
  ) :: OrTypeDef -> OrTypeDef'
