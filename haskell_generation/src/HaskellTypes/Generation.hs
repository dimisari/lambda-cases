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
  ( TypeName(..), ParenType(..), BaseType(..), ValueType(..), FieldAndType
  , CaseAndMaybeType, FieldsOrCases )

-- All:
-- Types, get fields, update fields, value_map operations, type_map operations,
-- initial state

-- Types: ValueMap, TypeMap, GenState, Stateful
type ValueMap =
  M.Map ValueName ValueType

type TypeMap =
  M.Map TypeName FieldsOrCases

data GenState =
  GS { indent_level :: Int, value_map :: ValueMap, type_map :: TypeMap }

type Stateful = State GenState

-- get fields: get_from_state, get_indent_level, get_value_map, get_type_map
get_from_state = ( \f -> get >>= f .> return )
  :: (GenState -> a) -> Stateful a

( get_indent_level, get_value_map, get_type_map ) =
  ( indent_level, value_map, type_map )==> \( i, v, t ) ->
  ( get_from_state i, get_from_state v, get_from_state t )
  :: ( Stateful Int, Stateful ValueMap, Stateful TypeMap )

-- update fields: update_indent_level, update_value_map, update_type_map
( update_indent_level, update_value_map, update_type_map ) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \vm -> modify ( \s -> s { value_map = vm } ) 
  , \tm -> modify ( \s -> s { type_map = tm } )
  ) :: ( Int -> Stateful (), ValueMap -> Stateful (), TypeMap -> Stateful () )

-- value_map operations: value_map_insert, value_map_get
value_map_insert =
  ( \vn vt -> get_value_map >>= M.insert vn vt .> update_value_map )
  :: ValueName -> ValueType -> Stateful ()

value_map_get = ( \vn@(VN s) -> get_value_map >>= M.lookup vn .> \case
  Nothing -> error $ "No definition for value: " ++ s
  Just vt -> return vt
  ) :: ValueName -> Stateful ValueType

-- type_map operations: type_map_exists_check, type_map_insert, type_map_get
type_map_exists_check = ( \tn -> get_type_map >>= M.lookup tn .> \case
  Just _ -> error $ "Type of the same name already defined: " ++ show tn
  Nothing -> return ()
  ) :: TypeName -> Stateful ()

type_map_insert =
  ( \tn foc -> get_type_map >>= M.insert tn foc .> update_type_map )
  :: TypeName -> FieldsOrCases -> Stateful ()

type_map_get = ( \tn@(TN s) -> get_type_map >>= M.lookup tn .> \case
  Nothing -> error $ "No definition for type: " ++ s
  Just foc -> return foc
  ) :: TypeName -> Stateful FieldsOrCases

-- initial state: int_bt, int_int_tuple_bt, init_value_map, init_state
int_bt = TypeName $ TN "Int"
  :: BaseType

int_vt = AbsTypesAndResType [] int_bt
  :: ValueType

int_int_tuple_bt =
  ParenType $ TupleType int_vt int_vt []
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

init_state = GS 0 init_value_map M.empty
  :: GenState
