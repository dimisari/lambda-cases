module InitialState where

import qualified Data.Map as M
  ( empty, fromList )

import HaskellTypes.Types
  ( TypeName(..), ParenType(..), BaseType(..), ValueType(..) )
import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Generation
  ( ValueMap, GenState(..) )

-- initial state: int_bt, int_int_tuple_bt, init_value_map, init_state
int_bt = TypeName $ TN "Int"
  :: BaseType

int_vt = AbsTypesAndResType [] int_bt
  :: ValueType

int_int_tuple_bt =
  ParenType $ TupleType int_vt int_vt []
  :: BaseType

bool_bt = TypeName $ TN "Bool"
  :: BaseType

init_value_map = 
  M.fromList
    [ ( VN "div" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "mod" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "get_first" , AbsTypesAndResType [ int_int_tuple_bt ] int_bt)
    , ( VN "abs" , AbsTypesAndResType [ int_bt ] int_bt)
    , ( VN "max" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "min" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "true" , AbsTypesAndResType [ ] bool_bt)
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenState
