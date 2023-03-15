{-# language LambdaCase #-}

module CodeGenerators.Types where

import Data.List
  ( intercalate )
import qualified Data.Map as M
  ( insert, lookup )

import Helpers
  ( Haskell, (==>), (.>) )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..), ValueType(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), FieldAndValType(..), ProdTypeDefinition(..)
  , CaseAndMaybeValType(..), ValOrTypeDefinition(..), ValFieldsOrCases(..)
  , ValTypeDefinition(..), value_type_to_val_type )
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_insert, type_map_exists_check )

-- All: ValueType, TupleTypeDefinition, OrTypeDefinition, TypeDefinition

value_type_g = ( value_type_to_val_type .> show )
  :: ValueType -> Haskell

-- ValType

val_type_g = ( \case
  FuncType t_in t_out -> val_type_g t_in ++ " -> " ++ val_type_g t_out
  NamedType tn -> show tn
  ProdType t1 t2 ts -> 
    "(" ++ map val_type_g (t1 : t2 : ts)==>intercalate ", " ++ ")" 
  ) :: ValType -> Haskell

-- ProdTypeDefinition

tuple_val_type_def_g = ( \(NameAndValFields tn fs) ->
  let
  tuple_g =
    fs==>mapM field_and_val_type_g >>= \fs_g ->
    return $ show tn ++ "C { " ++ intercalate ", " fs_g ++ " }"
    :: Stateful Haskell

  field_and_val_type_g = ( \(FVT vn vt) ->
    value_map_insert (VN $ "get_" ++ show vn) (FuncType (NamedType tn) vt) >>
    return ("get_" ++ show vn ++ " :: " ++ val_type_g vt)
    ) :: FieldAndValType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (FieldAndValTypeList fs) >> tuple_g >>= \tv_g ->
  return $ "\ndata " ++ show tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: ProdTypeDefinition -> Stateful Haskell

-- ValOrTypeDefinition

val_or_type_def_g = ( \(ValNameAndCases tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_val_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_val_type_g = ( \(CMVT vn mvt) ->
    ( case mvt of
      Nothing -> value_map_insert vn $ NamedType tn
      _ -> return () ) >>
    return ("C" ++ show vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt)
    ) :: CaseAndMaybeValType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (CaseAndMaybeValTypeList otvs) >> or_values_g >>=
    \otvs_g ->
  return $ "\n\ndata " ++ show tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show"
  ) :: ValOrTypeDefinition -> Stateful Haskell

-- ValTypeDefinition

val_type_def_g = ( \case
  ProdTypeDefinition tt -> tuple_val_type_def_g tt
  ValOrTypeDefinition ot -> val_or_type_def_g ot
  ) :: ValTypeDefinition -> Stateful Haskell
