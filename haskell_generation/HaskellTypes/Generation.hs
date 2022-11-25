{-# language LambdaCase #-}

module HaskellTypes.Generation where

import Control.Monad.State
  ( State, get, modify )
import qualified Data.Map as M
  ( Map, lookup, insert, empty, fromList )
--import Control.Monad.Trans.Except ( ExceptT )

import Helpers
  ( (.>), (==>) )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType )

-- Types
type TupleTypeMap =
  M.Map TypeName [ FieldAndType ] 

type ValueMap =
  M.Map ValueName ValueType

data GenState =
  GS { indent_level :: Int, tuple_type_map :: TupleTypeMap, value_map :: ValueMap  }

-- type Stateful = ExceptT String (State GenState)
type Stateful = State GenState

-- getting state fields
get_from_state = ( \f -> get >>= f .> return )
  :: (GenState -> a) -> Stateful a

( get_indent_level, get_tuple_type_map, get_value_map ) =
  ( indent_level, tuple_type_map, value_map ) ==> ( \(i,t,v) ->
  ( get_from_state i, get_from_state t, get_from_state v ))
  :: ( Stateful Int, Stateful TupleTypeMap, Stateful ValueMap )

-- updating state fields
( update_indent_level, update_tuple_type_map, update_value_map ) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \ttm -> modify ( \s -> s { tuple_type_map = ttm } )
  , \vm -> modify ( \s -> s { value_map = vm } ) )
  :: ( Int -> Stateful (), TupleTypeMap -> Stateful (), ValueMap -> Stateful () )

-- value map operations
value_map_lookup = ( \vn@(VN s) -> get_value_map >>= M.lookup vn .> \case
  Nothing -> error $ "No definition for value: " ++ s
  Just vt -> return vt
  ) :: ValueName -> Stateful ValueType

value_map_insert = ( \vn vt ->
  get_value_map >>= M.insert vn vt .> update_value_map
  ) :: ValueName -> ValueType -> Stateful ()

-- tuple type map operations
tuple_type_map_lookup = ( \tn -> get_tuple_type_map >>= M.lookup tn .> return)
  :: TypeName -> Stateful (Maybe [ FieldAndType ])

tuple_type_map_insert = ( \tn vt->
  get_tuple_type_map >>= M.insert tn vt .> update_tuple_type_map
  ) :: TypeName -> [ FieldAndType ] -> Stateful ()

-- initial state
int_bt = TypeName $ TN "Int"
  :: BaseType

int_int_tuple_bt = TupleType $ replicate 2 (AbsTypesAndResType [] int_bt)
  :: BaseType

init_value_map = 
  M.fromList
    [ ( VN "div" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "mod" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "get_first" , AbsTypesAndResType [ int_int_tuple_bt ] int_bt)
    , ( VN "abs" , AbsTypesAndResType [ int_bt ] int_bt)
    , ( VN "max" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    , ( VN "min" , AbsTypesAndResType [ int_bt, int_bt ] int_bt)
    ]
  :: ValueMap

init_state = GS 0 M.empty init_value_map
  :: GenState
