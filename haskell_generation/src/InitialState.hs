module InitialState where

import qualified Data.Map as M
  ( empty, fromList )

import HaskellTypes.Types
  ( TypeName(..), ValueType(..) )
import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), FuncType(..), ValFieldsOrCases(..) )
import HaskellTypes.Generation
  ( ValueMap, TypeMap, GenState(..) )

-- Initial state:
-- int, int_x_int, bool, int_to_int_to_int
-- init_value_map, init_state

int = NamedType $ TN "Int"
  :: ValType

int_x_int = ProdType [ int, int ]
  :: ValType

bool = NamedType $ TN "Bool"
  :: ValType

int_to_int_to_int =
  [ FuncType $ InAndOutType int $ FuncType $ InAndOutType int int ]
  :: [ ValType ]

init_value_map = 
  M.fromList
    [ (VN "div" , int_to_int_to_int)
    , (VN "mod" , int_to_int_to_int)
    , (VN "get_1st" , [ FuncType $ InAndOutType int_x_int int ])
    , (VN "abs" , [ FuncType $ InAndOutType int int ])
    , (VN "max" , int_to_int_to_int)
    , (VN "min" , int_to_int_to_int)
    , (VN "true" , [ bool ])
    , (VN "false" , [ bool ])
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenState
