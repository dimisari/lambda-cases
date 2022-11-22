module HaskellTypes.Generation where

import Prelude
  ( Int, String, Maybe(..), (>>=), return )
import Control.Monad.State
  ( State, get, modify )
--import Control.Monad.Trans.Except ( ExceptT )
import qualified Data.Map as M
  ( Map, lookup, insert )

import Helpers
  ( (.>), (-->) )

import HaskellTypes.LowLevel
  ( ValueName )
import HaskellTypes.Types
  ( TypeName, ValueType, FieldAndType )

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
  ( indent_level, tuple_type_map, value_map )-->( \(i,t,v) ->
  ( get_from_state i, get_from_state t, get_from_state v ))
  :: ( Stateful Int, Stateful TupleTypeMap, Stateful ValueMap )

-- updating state fields
( update_indent_level, update_tuple_type_map, update_value_map ) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \ttm -> modify ( \s -> s { tuple_type_map = ttm } )
  , \vm -> modify ( \s -> s { value_map = vm } ) )
  :: ( Int -> Stateful (), TupleTypeMap -> Stateful (), ValueMap -> Stateful () )

-- value map operations
value_map_lookup = ( \vn -> get_value_map >>= M.lookup vn .> return )
  :: ValueName -> Stateful (Maybe ValueType)

value_map_insert = ( \( vn, vt ) ->
  get_value_map >>= M.insert vn vt .> update_value_map
  ) :: ( ValueName, ValueType ) -> Stateful ()

-- tuple type map operations
tuple_type_map_lookup = ( \tn -> get_tuple_type_map >>= M.lookup tn .> return)
  :: TypeName -> Stateful (Maybe [ FieldAndType ])

tuple_type_map_insert = ( \( tn, vt ) ->
  get_tuple_type_map >>= M.insert tn vt .> update_tuple_type_map
  ) :: ( TypeName, [ FieldAndType ] ) -> Stateful ()
