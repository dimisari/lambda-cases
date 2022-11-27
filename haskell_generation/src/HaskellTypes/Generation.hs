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
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType, CaseAndType )

{- 
  All:
  Types, get fields, update fields, value_map operations,
  tuple_type_map operations, or_type_map operations,
  initial state
-}

-- Types
type ValueMap =
  M.Map ValueName ValueType

type TupleTypeMap =
  M.Map TypeName [ FieldAndType ] 

type OrTypeMap =
  M.Map TypeName [ CaseAndType ] 

data GenState =
  GS
  { indent_level :: Int, value_map :: ValueMap
  , tuple_type_map :: TupleTypeMap, or_type_map :: OrTypeMap }

-- type Stateful = ExceptT String (State GenState)
type Stateful = State GenState

-- get fields
get_from_state = ( \f -> get >>= f .> return )
  :: (GenState -> a) -> Stateful a

( get_indent_level, get_value_map, get_tuple_type_map, get_or_type_map ) =
  ( indent_level, value_map, tuple_type_map, or_type_map )==> \( i, v, t, o ) ->
  ( get_from_state i, get_from_state v, get_from_state t, get_from_state o )
  :: ( Stateful Int, Stateful ValueMap, Stateful TupleTypeMap, Stateful OrTypeMap )

-- update fields
( update_indent_level, update_value_map, update_tuple_type_map, update_or_type_map ) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \vm -> modify ( \s -> s { value_map = vm } ) 
  , \ttm -> modify ( \s -> s { tuple_type_map = ttm } )
  , \otm -> modify ( \s -> s { or_type_map = otm } ) )
  :: ( Int -> Stateful (), ValueMap -> Stateful (), TupleTypeMap -> Stateful ()
     , OrTypeMap -> Stateful ())

-- value_map operations
value_map_lookup = ( \vn@(VN s) -> get_value_map >>= M.lookup vn .> \case
  Nothing -> error $ "No definition for value: " ++ s
  Just vt -> return vt
  ) :: ValueName -> Stateful ValueType

value_map_insert = ( \vn vt ->
  get_value_map >>= M.insert vn vt .> update_value_map
  ) :: ValueName -> ValueType -> Stateful ()

-- tuple_type_map operations
tuple_type_map_lookup = ( \tn -> get_tuple_type_map >>= M.lookup tn .> return)
  :: TypeName -> Stateful (Maybe [ FieldAndType ])

tuple_type_map_insert = ( \tn fatl->
  get_tuple_type_map >>= M.insert tn fatl .> update_tuple_type_map
  ) :: TypeName -> [ FieldAndType ] -> Stateful ()

-- or type map operations
or_type_map_lookup = ( \tn -> get_or_type_map >>= M.lookup tn .> return)
  :: TypeName -> Stateful (Maybe [ CaseAndType ])

or_type_map_insert = ( \tn catl->
  get_or_type_map >>= M.insert tn catl .> update_or_type_map
  ) :: TypeName -> [ CaseAndType ] -> Stateful ()

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

init_state = GS 0 init_value_map M.empty M.empty
  :: GenState
