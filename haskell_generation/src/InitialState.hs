module InitialState where

import qualified Data.Map as M
  ( empty, fromList )

import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..) )
import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.AfterParsing
  ( ValType(..) )
import HaskellTypes.Generation
  ( ValMap, GenState(..) )

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

init_val_map = 
  M.fromList
    [ ( VN "div" , int_int_int_val_t_only )
    , ( VN "mod" , int_int_int_val_t_only )
    , ( VN "get_first" , [ FunctionType int_int_val_t int_val_t ] )
    , ( VN "abs" , [ FunctionType int_val_t int_val_t ] )
    , ( VN "max" , int_int_int_val_t_only )
    , ( VN "min" , int_int_int_val_t_only )
    , ( VN "true" , [ bool_val_t ] )
    ]
  :: ValMap

init_state = GS 0 init_val_map M.empty
  :: GenState
