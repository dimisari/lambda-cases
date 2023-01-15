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
  ( ValMap, ValueMap, GenState(..) )

-- initial state: int_bt, int_int_tuple_bt, init_value_map, init_state
int_bt = TypeName $ TN "Int"
  :: BaseType

int_vt = AbsTypesAndResType [] int_bt
  :: ValueType

int_int_tuple_bt =
  TupleType int_vt int_vt []
  :: BaseType

bool_bt = TypeName $ TN "Bool"
  :: BaseType

int_int_int_only = [ AbsTypesAndResType [ int_bt, int_bt ] int_bt ]
  :: [ ValueType ]

init_value_map = 
  M.fromList
    [ ( VN "div" , int_int_int_only )
    , ( VN "mod" , int_int_int_only )
    , ( VN "get_first" , [ AbsTypesAndResType [ int_int_tuple_bt ] int_bt ] )
    , ( VN "abs" , [ AbsTypesAndResType [ int_bt ] int_bt ] )
    , ( VN "max" , int_int_int_only )
    , ( VN "min" , int_int_int_only )
    , ( VN "true" , [ AbsTypesAndResType [ ] bool_bt ] )
    ]
  :: ValueMap

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

init_state = GS 0 init_val_map init_value_map M.empty
  :: GenState
