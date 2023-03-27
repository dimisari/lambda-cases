module CodeGenerators.Types where

import Data.List (intercalate)
import qualified Data.Map as M (insert, lookup)

import Helpers (Haskell, (==>), (.>))

import HaskellTypes.LowLevel (ValueName(..))
import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types (ValueType(..))
import HaskellTypes.AfterParsing
import HaskellTypes.Generation 

-- All: ValType, TupleTypeDefinition, OrTypeDefinition, TypeDefinition

-- ValType: val_type_g

val_type_g = show
  :: ValType -> Haskell

-- TupleTypeDef: tuple_type_def_g, fields_g, field_g

tuple_type_def_g = ( \(TTNameAndFields type_name fields) ->
  type_map_exists_check type_name >>
  type_map_insert type_name (FieldList fields) >>
  fields_g fields type_name >>= \fields_hs ->
  return $
    "\ndata " ++ show type_name ++ " =\n  " ++ fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDef -> Stateful Haskell

fields_g = ( \fields type_name ->
  fields==>mapM (flip field_g type_name) >>= \fields_hs ->
  return $ show type_name ++ "C { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field ] -> TypeName -> Stateful Haskell

field_g = ( \(FNameAndType value_name val_type) type_name ->
  value_map_insert
    (VN $ "get_" ++ show value_name)
    (FuncType $ InAndOutType (NamedType type_name) val_type) >>
  return ("get_" ++ show value_name ++ " :: " ++ val_type_g val_type)
  ) :: Field -> TypeName -> Stateful Haskell

-- OrTypeDef: or_type_def_g, or_type_cases_g, or_type_case_g

or_type_def_g = ( \(ValNameAndCases type_name or_type_cases) -> 
  type_map_exists_check type_name >>
  type_map_insert type_name (OrTypeCaseList or_type_cases) >>
  or_type_cases_g or_type_cases type_name >>= \cases_hs ->
  return $
    "\n\ndata " ++ show type_name ++ " =\n  " ++ cases_hs ++ "\n  deriving Show"
  ) :: OrTypeDef -> Stateful Haskell

or_type_cases_g = ( \or_type_cases type_name ->
  mapM (flip or_type_case_g type_name) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase ] -> TypeName -> Stateful Haskell

or_type_case_g = ( \(NameAndMaybeType value_name maybe_val_type) type_name ->
  value_map_insert value_name ( case maybe_val_type of
    Nothing -> NamedType type_name
    Just val_type -> FuncType $ InAndOutType val_type $ NamedType type_name ) >>
  return ("C" ++ show value_name ++ case maybe_val_type of 
    Nothing -> ""
    Just val_type  -> " " ++ show val_type)
  ) :: OrTypeCase -> TypeName -> Stateful Haskell

-- TypeDef: type_def_g

type_def_g = ( \case
  TupleTypeDef tuple_type_def -> tuple_type_def_g tuple_type_def
  OrTypeDef or_type_def -> or_type_def_g or_type_def
  ) :: TypeDef -> Stateful Haskell
