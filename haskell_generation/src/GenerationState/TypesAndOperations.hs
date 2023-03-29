module GenerationState.TypesAndOperations where

import Control.Monad.State (State, get, modify)
import Control.Monad.Trans.Except (ExceptT, throwE, catchE)
import qualified Data.Map as M (Map, lookup, insert, insertWith)

import Helpers (Haskell, Error, (.>), (==>))

import ParsingTypes.LowLevelValues (ValueName(..))
import ParsingTypes.LowLevelTypes (TypeName(..))
import ParsingTypes.Types

import AfterParsing.Types (ValueType'(..), FieldsOrCases(..))

-- All: Types, get fields, update fields, value_map operations, type_map operations

-- Types: ValueMap, TypeMap, GenerationState, Stateful

type ValueMap =
  M.Map ValueName [ ValueType' ]

type TypeMap =
  M.Map TypeName FieldsOrCases

data GenerationState =
  GS { indent_level :: Int, value_map :: ValueMap, type_map :: TypeMap }

type Stateful = ExceptT Error (State GenerationState)

-- get fields: get_from_state, get_indent_level, get_value_map, get_type_map

get_from_state = ( \f -> get >>= f .> return )
  :: (GenerationState -> a) -> Stateful a

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

type_map_insert = ( \type_name fields_or_cases ->
  get_type_map >>= \type_map ->
  M.lookup type_name type_map ==> \case
    Just _ -> throwE $ "Type of the same name already defined: " ++ show type_name
    Nothing -> update_type_map $ M.insert type_name fields_or_cases type_map
  ) :: TypeName -> FieldsOrCases -> Stateful ()

type_map_get = ( \type_name@(TN s) -> get_type_map >>= M.lookup type_name .> \case
  Nothing -> throwE $ "No definition for type: " ++ s
  Just foc -> return foc
  ) :: TypeName -> Stateful FieldsOrCases

