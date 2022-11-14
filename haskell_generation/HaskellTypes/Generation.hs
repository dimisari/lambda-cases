{-# LANGUAGE LambdaCase #-}

module HaskellTypes.Generation where

import Prelude ( Int, String, (>>=), return )
import Control.Monad.State ( State, get, modify )
import qualified Data.Map as M ( Map )

import Helpers ( (.>) )
import HaskellTypes.LowLevel ( ValueName )
import HaskellTypes.Types ( TypeName )

-- types
type TupleTypeMap =
  M.Map TypeName [ ValueName ] 

data GenState =
  IndentAndTupleTypes { indent_level :: Int, tuple_type_map :: TupleTypeMap }

type Stateful = State GenState

-- getting state fields
get_indent_level =
  get >>= indent_level .> return
  :: Stateful Int

get_tuple_type_map =
  get >>= tuple_type_map .> return
  :: Stateful TupleTypeMap

-- updating state fields
update_indent_level = ( \il ->
  modify ( \s -> s { indent_level = il } )
  ) :: Int -> Stateful ()

update_tuple_type_map = ( \ttm ->
  modify ( \s -> s { tuple_type_map = ttm } )
  ) :: TupleTypeMap -> Stateful ()

