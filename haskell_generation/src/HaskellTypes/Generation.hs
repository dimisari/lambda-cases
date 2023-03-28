module HaskellTypes.Generation where

import Control.Monad.State (State, get, modify)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import qualified Data.Map as M (Map, lookup, insert, insertWith)

import Helpers (Haskell, Error, (.>), (==>))

import HaskellTypes.LowLevel (ValueName(..))
import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types
import HaskellTypes.AfterParsing (ValueType'(..), FieldsOrCases'(..))

-- All: Types, get fields, update fields, value_map operations, type_map operations

-- Types: ValueMap, TypeMap, GenState, Stateful

type ValueMap =
  M.Map ValueName [ ValueType' ]

type TypeMap =
  M.Map TypeName FieldsOrCases'

data GenState =
  GS { indent_level :: Int, value_map :: ValueMap, type_map :: TypeMap }

type Stateful = ExceptT Error (State GenState)

-- get fields: get_from_state, get_indent_level, get_value_map, get_type_map

get_from_state = ( \f -> get >>= f .> return )
  :: (GenState -> a) -> Stateful a

(get_indent_level, get_value_map, get_type_map) =
  (indent_level, value_map, type_map)==> \(i, vm, tm) ->
  (get_from_state i, get_from_state vm, get_from_state tm)
  :: (Stateful Int, Stateful ValueMap, Stateful TypeMap)

-- update fields: update_indent_level, update_value_map, update_type_map

(update_indent_level, update_value_map, update_type_map) =
  ( \il -> modify ( \s -> s { indent_level = il } )
  , \vm -> modify ( \s -> s { value_map = vm } ) 
  , \tm -> modify ( \s -> s { type_map = tm } )
  ) :: (Int -> Stateful (), ValueMap -> Stateful (), TypeMap -> Stateful ())

-- value_map operations: value_map_insert, value_map_get
  
value_map_insert = ( \vn vt ->
  get_value_map >>= M.insertWith (++) vn [vt] .> update_value_map
  ) :: ValueName -> ValueType' -> Stateful ()

value_map_get = ( \vn -> get_value_map >>= M.lookup vn .> \case
  Nothing -> throwE $ "No definition for value: " ++ show vn
  Just vts -> case vts of
    [] -> undefined
    vt:_ -> return vt
  ) :: ValueName -> Stateful ValueType'

-- type_map operations: type_map_exists_check, type_map_insert, type_map_get

type_map_exists_check = ( \tn -> get_type_map >>= M.lookup tn .> \case
  Just _ -> throwE $ "Type of the same name already defined: " ++ show tn
  Nothing -> return ()
  ) :: TypeName -> Stateful ()

type_map_insert =
  (\tn vfoc -> get_type_map >>= M.insert tn vfoc .> update_type_map)
  :: TypeName -> FieldsOrCases' -> Stateful ()

type_map_get = ( \tn@(TN s) -> get_type_map >>= M.lookup tn .> \case
  Nothing -> throwE $ "No definition for type: " ++ s
  Just foc -> return foc
  ) :: TypeName -> Stateful FieldsOrCases'

debug = ( \f -> catchE f (\_ -> throwE "hi" ) )
  :: Stateful a -> Stateful a

debug_with_msg = ( \f s -> catchE f (\_ -> throwE s ) )
  :: Stateful a -> String -> Stateful a
