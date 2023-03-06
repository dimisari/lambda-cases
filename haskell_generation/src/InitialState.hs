module InitialState where

import qualified Data.Map as M
  ( empty, fromList )

import HaskellTypes.Types
  ( TypeName(..), ValueType(..) )
import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), ValFieldsOrCases(..) )
import HaskellTypes.Generation
  ( ValueMap, TypeMap, GenState(..) )

-- initial state: int_bt, int_int_tuple_bt, init_value_map, init_state
int_val_t = NamedType $ TN "Int"
  :: ValType

int_int_val_t =
  TupleValType int_val_t int_val_t []
  :: ValType

bool_val_t = NamedType $ TN "Bool"
  :: ValType

int_int_int_val_t_only =
  [ FunctionType int_val_t $ FunctionType int_val_t int_val_t ]
  :: [ ValType ]

init_value_map = 
  M.fromList
    [ ( VN "div" , int_int_int_val_t_only )
    , ( VN "mod" , int_int_int_val_t_only )
    , ( VN "get_first" , [ FunctionType int_int_val_t int_val_t ] )
    , ( VN "abs" , [ FunctionType int_val_t int_val_t ] )
    , ( VN "max" , int_int_int_val_t_only )
    , ( VN "min" , int_int_int_val_t_only )
    , ( VN "true" , [ bool_val_t ] )
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenState
