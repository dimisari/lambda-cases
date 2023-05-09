module Conversions.TypeDefinitions where

import ParsingTypes.Types (TypeName)
import ParsingTypes.TypeDefinitions
import IntermediateTypes.TypeDefinitions
import Conversions.Types (to_val_type)

-- All: ValType, Type Definitions

-- TypeNameExpr: cons_and_t_vars_conv

cons_and_t_vars_conv = ( \(TypeNameExpr cons_name left_t_vars right_t_vars) ->
  TNameExpr cons_name $
    flip zip [ "a", "b", "c", "d", "e" ] $
      left_t_vars_conv left_t_vars ++ right_t_vars_conv right_t_vars
  ) :: TypeNameExpr -> TNameExpr

left_t_vars_conv = ( \case
  NoLeftTypeVars -> []
  OneLeftTypeVar type_name -> [ type_name ]
  ManyLeftTypeVars many_tns_in_paren -> many_tns_in_paren_conv many_tns_in_paren
  ) :: LeftTypeVars -> [ TypeName ]

right_t_vars_conv = ( \case
  NoRightTypeVars -> []
  OneRightTypeVar type_name -> [ type_name ]
  ManyRightTypeVars many_tns_in_paren -> many_tns_in_paren_conv many_tns_in_paren
  ) :: RightTypeVars -> [ TypeName ]

many_tns_in_paren_conv = ( \(ParenTypeNames n1 n2 ns) -> n1 : n2 : ns)
  :: ManyTNamesInParen -> [ TypeName ]

-- Type Definitions: 
-- field_conv, tuple_type_def_conv,
-- or_type_case_conv, or_type_def_conv

field_conv = ( \(NameAndType value_name value_type) ->
  FNameAndType value_name (to_val_type value_type)
  ) :: Field -> TTField

tuple_type_def_conv = ( \(NameExprAndFields type_app fields) ->
  TTNameExprAndFields (cons_and_t_vars_conv type_app) (map field_conv fields)
  ) :: TupleTypeDef -> TupleTDef

or_type_case_conv = ( \(NameAndMaybeInT value_name maybe_value_type) ->
  CNameAndMaybeInT value_name (to_val_type <$> maybe_value_type)
  ) :: OrTypeCaseOld -> OrTCase

or_type_def_conv = ( \(NameExprAndCases type_app c1 c2 cs) ->
  OTNameExprAndCases
    (cons_and_t_vars_conv type_app) (map or_type_case_conv $ c1 : c2 : cs)
  ) :: OrTypeDef -> OrTDef
