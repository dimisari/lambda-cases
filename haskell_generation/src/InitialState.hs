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

-- Initial state:
-- int_val_t, int_int_val_t, bool_val_t, int_int_int_val_t_only
-- init_value_map, init_state

int_val_t = NamedType $ TN "Int"
  :: ValType

int_int_val_t =
  ProdType int_val_t int_val_t []
  :: ValType

bool_val_t = NamedType $ TN "Bool"
  :: ValType

int_int_int_val_t_only =
  [ FuncType int_val_t $ FuncType int_val_t int_val_t ]
  :: [ ValType ]

init_value_map = 
  M.fromList
    [ ( VN "div" , int_int_int_val_t_only )
    , ( VN "mod" , int_int_int_val_t_only )
    , ( VN "get_first" , [ FuncType int_int_val_t int_val_t ] )
    , ( VN "abs" , [ FuncType int_val_t int_val_t ] )
    , ( VN "max" , int_int_int_val_t_only )
    , ( VN "min" , int_int_int_val_t_only )
    , ( VN "true" , [ bool_val_t ] )
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenState
