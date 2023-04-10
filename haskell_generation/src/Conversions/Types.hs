module Conversions.Types where

import ParsingTypes.Types
import IntermediateTypes.Types

-- All: ValueType', Type Definitions

-- ValueType':
-- value_type_conversion, func_type_to_val_type, one_input_to_val_type,
-- multiple_inputs_to_val_type, output_type_to_val_type, cart_prod_to_val_type

value_type_conversion = ( \case
  FunctionType func_type -> func_type_to_val_type func_type
  ProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  TypeApplication type_application ->
    TypeApplication' $ type_application_conversion type_application
  ) :: ValueType -> ValueType'

func_type_to_val_type = ( \(InputAndOutputTypes input output_t) -> case input of 
  OneInputType input_t -> one_input_to_val_type input_t output_t
  MultipleInputTypes mult_ins -> multiple_inputs_to_val_type mult_ins output_t
  ) :: FunctionType -> ValueType'

one_input_to_val_type = ( \input_t output_t -> 
  FunctionType' $
    InputAndOutputType'
      (value_type_conversion input_t) (output_type_to_val_type output_t)
  ) :: ValueType -> OutputType -> ValueType'

multiple_inputs_to_val_type = ( \(TypesInParen in_t1 in_t2 in_ts) output_t -> 
  let
  output_type = case in_ts of 
    [] -> one_input_to_val_type in_t2 output_t
    in_t3 : rest_of_in_ts ->
      multiple_inputs_to_val_type
        (TypesInParen in_t2 in_t3 rest_of_in_ts) output_t
  in
  FunctionType' $ InputAndOutputType' (value_type_conversion in_t1) output_type
  ) :: ManyTypesInParen -> OutputType -> ValueType'

output_type_to_val_type = ( \case
  OutputTypeApp type_application ->
    TypeApplication' $ type_application_conversion type_application
  OutputProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  ) :: OutputType -> ValueType'

cart_prod_to_val_type = (
  \(ProductTypes value_type1 value_type2 other_value_types) ->
  ProductType' $
    value_type_conversion value_type1 : value_type_conversion value_type2 :
    map value_type_conversion other_value_types
  ) :: ProductType -> ValueType'

-- TypeApplication: type_application_conversion

type_application_conversion = ( 
  \(TypeConsAndInputs constructor_name left_type_inputs right_type_inputs) ->
  TypeConsAndInputs' constructor_name $
    left_type_inputs_conversion left_type_inputs ++
    right_type_inputs_conversion right_type_inputs
  ) :: TypeApplication -> TypeApplication'

left_type_inputs_conversion = ( \case
  NoLeftTypeInputs -> []
  OneLeftTypeInput type_input -> [ value_type_conversion type_input ]
  ManyLeftTypeInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren
  ) :: LeftTypeInputs -> [ ValueType' ]

right_type_inputs_conversion = ( \case
  NoRightTypeInputs -> []
  OneRightTypeInput type_input -> [ value_type_conversion type_input ]
  ManyRightTypeInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren 
  ) :: RightTypeInputs -> [ ValueType' ]

many_ts_in_paren_conv = ( \(TypesInParen t1 t2 ts) ->
  map value_type_conversion $ t1 : t2 : ts
  ) :: ManyTypesInParen -> [ ValueType' ]
