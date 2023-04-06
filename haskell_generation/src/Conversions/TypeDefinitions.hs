module Conversions.TypeDefinitions where

import ParsingTypes.Types (TypeName)
import ParsingTypes.TypeDefinitions
import IntermediateTypes.TypeDefinitions
import Conversions.Types (value_type_conversion)

-- All: ValueType', Type Definitions

-- TypeConstructorAndVariables: cons_and_type_vars_conversion

cons_and_type_vars_conversion = ( 
  \(TypeConstructorAndVariables constructor_name left_type_vars right_type_vars) ->
  TypeConstructorAndVariables' constructor_name $
    flip zip [ "a", "b", "c", "d", "e" ] $
    left_type_vars_conversion left_type_vars ++
    right_type_vars_conversion right_type_vars
  ) :: TypeConstructorAndVariables -> TypeConstructorAndVariables'

left_type_vars_conversion = ( \case
  NoLeftTypeVariables -> []
  OneLeftTypeVariable type_name -> [ type_name ]
  ManyLeftTypeVariables many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: LeftTypeVariables -> [ TypeName ]

right_type_vars_conversion = ( \case
  NoRightTypeVariables -> []
  OneRightTypeVariable type_name -> [ type_name ]
  ManyRightTypeVariables many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: RightTypeVariables -> [ TypeName ]

many_t_names_in_paren_conv = ( \(ParenTypeNames n1 n2 ns) -> n1 : n2 : ns)
  :: ManyTypeNamesInParenthesis -> [ TypeName ]

-- Type Definitions: 
-- field_conversion, tuple_type_def_conversion,
-- or_type_case_conversion, or_type_def_conversion

field_conversion = ( \(NameAndType value_name value_type) ->
  NameAndType' value_name (value_type_conversion value_type)
  ) :: Field -> Field'

tuple_type_def_conversion = ( \(ConstructorAndFields type_application fields) ->
  ConstructorAndFields'
    (cons_and_type_vars_conversion type_application)
    (map field_conversion fields)
  ) :: TupleTypeDefinition -> TupleTypeDefinition'

or_type_case_conversion = ( \(NameAndMaybeInputType value_name maybe_value_type) ->
  NameAndMaybeInputType' value_name (value_type_conversion <$> maybe_value_type)
  ) :: OrTypeCase -> OrTypeCase'

or_type_def_conversion = (
  \(ConstructorAndCases type_application case1 case2 cases) ->
  ConstructorAndCases'
    (cons_and_type_vars_conversion type_application)
    (map or_type_case_conversion $ case1 : case2 : cases)
  ) :: OrTypeDefinition -> OrTypeDefinition'
