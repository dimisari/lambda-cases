module InitialState where

import qualified Data.Map as M ( empty, fromList )

import HaskellTypes.LowLevel (ValueName(..))
import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types (ValueType(..))
import HaskellTypes.AfterParsing (ValueType'(..), FunctionType'(..), FieldsOrCases'(..))
import HaskellTypes.Generation (ValueMap, TypeMap, GenState(..))

-- Initial state:
-- int, int_x_int, bool, int_to_int_to_int
-- init_value_map, init_state

int = TypeName' $ TN "Int"
  :: ValueType'

int_x_int = ProductType' [ int, int ]
  :: ValueType'

bool = TypeName' $ TN "Bool"
  :: ValueType'

int_to_int_to_int =
  [ FunctionType' $ InAndOutType int $ FunctionType' $ InAndOutType int int ]
  :: [ ValueType' ]

init_value_map = 
  M.fromList
    [ (VN "div" , int_to_int_to_int)
    , (VN "mod" , int_to_int_to_int)
    , (VN "get_1st" , [ FunctionType' $ InAndOutType int_x_int int ])
    , (VN "abs" , [ FunctionType' $ InAndOutType int int ])
    , (VN "max" , int_to_int_to_int)
    , (VN "min" , int_to_int_to_int)
    , (VN "true" , [ bool ])
    , (VN "false" , [ bool ])
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenState
