module Conversions.Types where

import ParsingTypes.Types
import IntermediateTypes.Types

-- All: ValType, Type Definitions

-- ValType:
-- val_type_conv, func_t_to_val_t, one_in_to_val_t,
-- mult_ins_to_val_t, out_t_to_val_t, prod_t_to_val_t

val_type_conv = ( \case
  FunctionType func_t -> func_t_to_val_t func_t
  ProductType prod_t -> prod_t_to_val_t prod_t
  TypeApplication type_app -> TypeApp $ type_app_conv type_app
  ) :: ValueType -> ValType

func_t_to_val_t = ( \(InAndOutTypes input out_t) -> case input of 
  OneInputType in_t -> one_in_to_val_t in_t out_t
  MultipleInputTypes mult_ins -> mult_ins_to_val_t mult_ins out_t
  ) :: FunctionType -> ValType

one_in_to_val_t = ( \in_t out_t -> 
  FuncType $ InAndOutTs (val_type_conv in_t) (out_t_to_val_t out_t)
  ) :: ValueType -> OutputType -> ValType

mult_ins_to_val_t = ( \(TypesInParen in_t1 in_t2 in_ts) out_t -> 
  let
  func_out_t = case in_ts of 
    [] -> one_in_to_val_t in_t2 out_t
    in_t3 : rest_of_in_ts ->
      mult_ins_to_val_t (TypesInParen in_t2 in_t3 rest_of_in_ts) out_t
  in
  FuncType $ InAndOutTs (val_type_conv in_t1) func_out_t
  ) :: ManyTypesInParen -> OutputType -> ValType

out_t_to_val_t = ( \case
  OutputTypeApp type_app -> TypeApp $ type_app_conv type_app
  OutputProductType prod_t -> prod_t_to_val_t prod_t
  ) :: OutputType -> ValType

prod_t_to_val_t = ( \(ProductTypes val_t1 val_t2 other_val_ts) ->
  ProdType $
    val_type_conv val_t1 : val_type_conv val_t2 : map val_type_conv other_val_ts
  ) :: ProductType -> ValType

-- TypeApplication: type_app_conv

type_app_conv = ( \(TypeConsAndInputs cons_name left_t_ins right_t_ins) ->
  ConsAndInTs cons_name $
    left_t_ins_conv left_t_ins ++ right_t_ins_conv right_t_ins
  ) :: TypeApplication -> TypeApp

left_t_ins_conv = ( \case
  NoLeftTInputs -> []
  OneLeftTInput type_input -> [ val_type_conv type_input ]
  ManyLeftTInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren
  ) :: LeftTInputs -> [ ValType ]

right_t_ins_conv = ( \case
  NoRightTInputs -> []
  OneRightTInput type_input -> [ val_type_conv type_input ]
  ManyRightTInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren 
  ) :: RightTInputs -> [ ValType ]

many_ts_in_paren_conv = ( \(TypesInParen t1 t2 ts) ->
  map val_type_conv $ t1 : t2 : ts
  ) :: ManyTypesInParen -> [ ValType ]
