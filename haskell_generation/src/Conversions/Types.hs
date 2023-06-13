module Conversions.Types where

import Helpers ((.>))

import Parsing.Types.Types
import IntermediateTypes.Types

class ToValType a where
  to_val_type :: a -> ValType

instance ToValType ValueType where
  to_val_type = \case
    FunctionType func_t -> to_val_type func_t
    ProductType prod_t -> to_val_type prod_t
    TypeApplication type_app -> to_val_type type_app

instance ToValType FunctionType where
  to_val_type = \(InAndOutTypes input out_t) -> case input of 
    OneInputType in_t ->
      FuncType $ InAndOutTs (to_val_type in_t) (to_val_type out_t)
    MultipleInputTypes mult_ins ->
      to_val_type (MultInTsType (ts_in_paren_to_value_types mult_ins) out_t)

data MultInTsType = MultInTsType [ ValueType ] OutputType

instance ToValType MultInTsType where
  to_val_type = \(MultInTsType in_ts out_t) -> case in_ts of
    [] -> to_val_type out_t
    in_t1 : in_ts_rest ->
      FuncType $ InAndOutTs
        (to_val_type in_t1)
        (to_val_type $ MultInTsType in_ts_rest out_t)

instance ToValType OutputType where
  to_val_type = \case
    OutputTypeApp type_app -> to_val_type type_app
    OutputProductType prod_t -> to_val_type prod_t

instance ToValType ProductType where
  to_val_type = \(ProductTypes val_t1 val_t2 other_val_ts) ->
    ProdType $ ProdTypes $ map to_val_type (val_t1 : val_t2 : other_val_ts)

instance ToValType TypeApplication where
  to_val_type = type_app_conv .> TypeApp

-- TypeApplication: type_app_conv

type_app_conv = ( \(TypeConsAndInputs cons_name left_t_ins right_t_ins) ->
  ConsAndTIns cons_name $
    left_t_ins_conv left_t_ins ++ right_t_ins_conv right_t_ins
  ) :: TypeApplication -> TypeApp

left_t_ins_conv = ( \case
  NoLeftTInputs -> []
  OneLeftTInput type_input -> [ to_val_type type_input ]
  ManyLeftTInputs many_ts_in_paren -> ts_in_paren_to_val_types many_ts_in_paren
  ) :: LeftTInputs -> [ ValType ]

right_t_ins_conv = ( \case
  NoRightTInputs -> []
  OneRightTInput type_input -> [ to_val_type type_input ]
  ManyRightTInputs many_ts_in_paren -> ts_in_paren_to_val_types many_ts_in_paren 
  ) :: RightTInputs -> [ ValType ]

ts_in_paren_to_val_types = ( \(TypesInParen t1 t2 ts) ->
  map to_val_type $ t1 : t2 : ts
  ) :: ManyTypesInParen -> [ ValType ]

ts_in_paren_to_value_types = ( \(TypesInParen t1 t2 ts) -> t1 : t2 : ts )
  :: ManyTypesInParen -> [ ValueType ]
