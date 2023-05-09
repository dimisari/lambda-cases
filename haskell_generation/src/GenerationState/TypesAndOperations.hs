module GenerationState.TypesAndOperations where

import Control.Monad.State (State, get, modify)
import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map as M (Map, lookup, insert, insertWith)

import Helpers ((.>), (==>), Haskell)

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types

import IntermediateTypes.Types (ValType(..))
import IntermediateTypes.TypeDefinitions (TypeInfo(..))

import GenerationHelpers.ErrorMessages

-- All: Types, get fields, set fields, value_map operations, type_map operations

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

-- get fields:

(get_ind_lev, get_value_map, get_type_map, get_or_t_cs) =
  (ind_lev <$> get, value_map <$> get, type_map <$> get, or_type_cases <$> get)
  :: (Stateful Int, Stateful ValueMap, Stateful TypeMap, Stateful [ ValueName ])

-- set fields:
-- set_ind_lev, set_value_map, set_type_map, set_or_t_cs

(set_ind_lev, set_value_map, set_type_map, set_or_t_cs) =
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

-- modify fields:
-- modify_ind_lev, modify_value_map, modify_type_map, modify_or_t_cs

(modify_ind_lev, modify_value_map, modify_type_map, modify_or_t_cs) =
  ( \f -> modify ( \s -> s { ind_lev = f $ ind_lev s } )
  , \f -> modify ( \s -> s { value_map = f $ value_map s } ) 
  , \f -> modify ( \s -> s { type_map = f $ type_map s } )
  , \f -> modify ( \s -> s { or_type_cases = f $ or_type_cases s } )
  ) ::
  ( (Int -> Int) -> Stateful ()
  , (ValueMap -> ValueMap) -> Stateful ()
  , (TypeMap -> TypeMap) -> Stateful ()
  , ([ ValueName ] -> [ ValueName ]) -> Stateful ()
  )

-- value_map operations: value_map_insert, value_map_get, value_map_remove
  
value_map_insert = ( \val_name val_type ->
  get_value_map >>= M.insertWith (++) val_name [val_type] .> set_value_map
  ) :: ValueName -> ValType -> Stateful ()

value_map_get = ( \val_name -> get_value_map >>= M.lookup val_name .> \case
  Nothing -> throwE $ no_def_for_val_err val_name
  Just vts -> case vts of
    [] -> throwE $ no_def_for_val_err val_name
    val_type:_ -> return val_type
  ) :: ValueName -> Stateful ValType

value_map_remove = ( \val_name ->
  get_value_map >>= \val_map -> 
  M.lookup val_name val_map ==> \case
    Just (_:t_list_tail) ->
      set_value_map $ M.insert val_name t_list_tail val_map
    _ -> error "removed from "
  ) :: ValueName -> Stateful ()

-- type_map operations: type_map_insert, type_map_get

type_map_insert = ( \type_name fields_or_cases ->
  get_type_map >>= \type_map ->
  M.lookup type_name type_map ==> \case
    Just _ -> throwE $ type_exist_err type_name
    Nothing -> set_type_map $ M.insert type_name fields_or_cases type_map
  ) :: TypeName -> TypeInfo -> Stateful ()

type_map_get = ( \type_name -> get_type_map >>= M.lookup type_name .> \case
  Nothing -> throwE $ no_def_for_type_err type_name
  Just foc -> return foc
  ) :: TypeName -> Stateful TypeInfo

-- or_type_cases operations: insert_to_or_t_cs, in_or_t_cs

insert_to_or_t_cs = ( \value_name -> 
  get_or_t_cs >>= (value_name:) .> set_or_t_cs
  ) :: ValueName -> Stateful ()

in_or_t_cs = ( \value_name -> get_or_t_cs >>= elem value_name .> return )
  :: ValueName -> Stateful Bool

--

indentation = indent <$> get_ind_lev
  :: Stateful Haskell

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> String
