module GenerationState.TypesAndOperations where

import Control.Monad.State (State, get, modify)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map as M (Map, lookup, insert, insertWith)

import Helpers (Error, (.>), (==>))

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types

import IntermediateTypes.Types (ValType(..))
import IntermediateTypes.TypeDefinitions (TypeInfo(..))

-- All: Types, get fields, update fields, value_map operations, type_map operations

-- Types: ValueMap, TypeMap, GenerationState, Stateful

type ValueMap =
  M.Map ValueName [ ValType ]

type TypeMap =
  M.Map TypeName TypeInfo

data GenerationState = GS
  { ind_lev :: Int
  , value_map :: ValueMap
  , type_map :: TypeMap
  , or_type_cases :: [ ValueName ]
  }

type Stateful = ExceptT Error (State GenerationState)

-- get fields: get_from_state, get_ind_lev, get_value_map, get_type_map

get_from_state = ( \f -> get >>= f .> return )
  :: (GenerationState -> a) -> Stateful a

(get_ind_lev, get_value_map, get_type_map, get_or_t_cs) =
  (ind_lev, value_map, type_map, or_type_cases)==> \(i, vm, tm, otc) ->
  (get_from_state i, get_from_state vm, get_from_state tm, get_from_state otc)
  :: (Stateful Int, Stateful ValueMap, Stateful TypeMap, Stateful [ ValueName ])

-- update fields: update_ind_lev, update_value_map, update_type_map

(update_ind_lev, update_value_map, update_type_map, update_or_t_cs
  ) =
  ( \il -> modify ( \s -> s { ind_lev = il } )
  , \vm -> modify ( \s -> s { value_map = vm } ) 
  , \tm -> modify ( \s -> s { type_map = tm } )
  , \otc -> modify ( \s -> s { or_type_cases = otc } )
  ) ::
  ( Int -> Stateful ()
  , ValueMap -> Stateful ()
  , TypeMap -> Stateful ()
  , [ ValueName ] -> Stateful ()
  )

-- value_map operations: value_map_insert, value_map_get
  
value_map_insert = ( \vn vt ->
  get_value_map >>= M.insertWith (++) vn [vt] .> update_value_map
  ) :: ValueName -> ValType -> Stateful ()

value_map_get = ( \vn -> get_value_map >>= M.lookup vn .> \case
  Nothing -> throwE $ "No definition for value: " ++ show vn
  Just vts -> case vts of
    [] -> undefined
    vt:_ -> return vt
  ) :: ValueName -> Stateful ValType

-- type_map operations: type_map_exists_check, type_map_insert, type_map_get

type_map_insert = ( \type_name fields_or_cases ->
  get_type_map >>= \type_map ->
  M.lookup type_name type_map ==> \case
    Just _ -> throwE $ "Type of the same name already defined: " ++ show type_name
    Nothing -> update_type_map $ M.insert type_name fields_or_cases type_map
  ) :: TypeName -> TypeInfo -> Stateful ()

type_map_get = ( \type_name@(TN s) -> get_type_map >>= M.lookup type_name .> \case
  Nothing -> throwE $ "No definition for type: " ++ s
  Just foc -> return foc
  ) :: TypeName -> Stateful TypeInfo

-- or_type_cases operations: 

insert_to_or_t_cs = ( \value_name -> 
  get_or_t_cs >>= (value_name:) .> update_or_t_cs
  ) :: ValueName -> Stateful ()

in_or_t_cs = ( \value_name ->
  get_or_t_cs >>= elem value_name .> return
  ) :: ValueName -> Stateful Bool
