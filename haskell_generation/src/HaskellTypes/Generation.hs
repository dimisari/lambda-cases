{-# language LambdaCase #-}

module HaskellTypes.Generation where

import Control.Monad.State
  ( State, get, modify )
import qualified Data.Map as M
  ( Map, lookup, insert, insertWith )
--import Control.Monad.Trans.Except ( ExceptT )

import Helpers
  ( (.>), (==>) )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType, CaseAndMaybeType
  , FieldsOrCases )
import HaskellTypes.AfterParsing
  ( ValType(..), ValFieldsOrCases(..) )

-- All:
-- Types, get fields, update fields, value_map operations, type_map operations

-- Types: ValueMap, TypeMap, GenState, Stateful
type ValMap =
  M.Map ValueName [ ValType ]

type ValTypeMap =
  M.Map TypeName ValFieldsOrCases

data GenState = GS
  { indent_level :: Int, val_map :: ValMap, val_type_map :: ValTypeMap }

type Stateful = State GenState

-- get fields: get_from_state, get_indent_level, get_value_map, get_type_map
get_from_state = ( \f -> get >>= f .> return )
  :: (GenState -> a) -> Stateful a

(get_indent_level, get_val_map, get_val_type_map) =
  (indent_level, val_map, val_type_map)==> \(i, vm, vtm) ->
  (get_from_state i, get_from_state vm, get_from_state vtm)
  :: (Stateful Int, Stateful ValMap, Stateful ValTypeMap)

-- update fields: update_indent_level, update_value_map, update_type_map
(update_indent_level, update_val_map, update_val_type_map) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \vm -> modify ( \s -> s { val_map = vm } ) 
  , \vtm -> modify ( \s -> s { val_type_map = vtm } )
  ) :: ( Int -> Stateful (), ValMap -> Stateful (), ValTypeMap -> Stateful () )

-- val_map operations: val_map_insert, value_map_get
val_map_insert =
  (\vn vt -> get_val_map >>= M.insertWith (++) vn [vt] .> update_val_map)
  :: ValueName -> ValType -> Stateful ()

val_map_get = ( \vn -> get_val_map >>= M.lookup vn .> \case
  Nothing -> error $ "No definition for value: " ++ show vn
  Just vts -> case vts of
    [] -> undefined
    vt:_ -> return vt
  ) :: ValueName -> Stateful ValType

-- val_type_map operations:
-- val_type_map_exists_check, val_type_map_insert, val_type_map_get
val_type_map_exists_check = ( \tn -> get_val_type_map >>= M.lookup tn .> \case
  Just _ -> error $ "Type of the same name already defined: " ++ show tn
  Nothing -> return ()
  ) :: TypeName -> Stateful ()

val_type_map_insert =
  (\tn vfoc -> get_val_type_map >>= M.insert tn vfoc .> update_val_type_map)
  :: TypeName -> ValFieldsOrCases -> Stateful ()

val_type_map_get = ( \tn@(TN s) -> get_val_type_map >>= M.lookup tn .> \case
  Nothing -> error $ "No definition for type: " ++ s
  Just foc -> return foc
  ) :: TypeName -> Stateful ValFieldsOrCases
