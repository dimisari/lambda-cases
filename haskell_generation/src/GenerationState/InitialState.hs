module GenerationState.InitialState where

import qualified Data.Map as M ( empty, fromList )

import ParsingTypes.LowLevelValues (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..))
import ParsingTypes.Types (ValueType(..))

import AfterParsing.Types 

import GenerationState.TypesAndOperations (ValueMap, TypeMap, GenerationState(..))

-- Initial state:
-- int, int_x_int, bool, int_to_int_to_int
-- init_value_map, init_state

int = ConsAndTypeVars' $ ConsAndTVars' (TN "Int") []
  :: ValueType'

int_x_int = ProductType' [ int, int ]
  :: ValueType'

bool = ConsAndTypeVars' $ ConsAndTVars' (TN "Bool") []
  :: ValueType'

int_to_int_to_int =
  [ FunctionType' $
    InputAndOutputType' int $ FunctionType' $ InputAndOutputType' int int ]
  :: [ ValueType' ]

init_value_map = 
  M.fromList
    [ (VN "div" , int_to_int_to_int)
    , (VN "mod" , int_to_int_to_int)
    , (VN "get_1st" , [ FunctionType' $ InputAndOutputType' int_x_int int ])
    , (VN "abs" , [ FunctionType' $ InputAndOutputType' int int ])
    , (VN "max" , int_to_int_to_int)
    , (VN "min" , int_to_int_to_int)
    , (VN "true" , [ bool ])
    , (VN "false" , [ bool ])
    ]
  :: ValueMap

init_state = GS 0 init_value_map M.empty
  :: GenerationState
