module CodeGenerators.TypeDefinitions where

import Data.List (intercalate)
import qualified Data.Map as M (insert, lookup)

import Helpers (Haskell, (==>), (.>))

import HaskellTypes.LowLevel (ValueName(..))
import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types (ValueType(..))
import HaskellTypes.TypeDefinitions (TypeDefinition)
import HaskellTypes.AfterParsing
import HaskellTypes.Generation 

-- All: ValueType', TupleTypeDefinition, OrTypeDefinition, TypeDefinition

-- TupleTypeDefinition': tuple_type_def_g, fields_g, field_g

tuple_type_def_g = ( \(TTNameAndFields type_name fields) ->
  type_map_exists_check type_name >>
  type_map_insert type_name (FieldList fields) >>
  fields_g fields type_name >>= \fields_hs ->
  return $
    "\ndata " ++ show type_name ++ " =\n  " ++ fields_hs ++ "\n  deriving Show\n"
  ) :: TupleTypeDefinition' -> Stateful Haskell

fields_g = ( \fields type_name ->
  fields==>mapM (flip field_g type_name) >>= \fields_hs ->
  return $ show type_name ++ "C { " ++ intercalate ", " fields_hs ++ " }"
  ) :: [ Field' ] -> TypeName -> Stateful Haskell

field_g = ( \(FNameAndType value_name val_type) type_name ->
  value_map_insert
    (VN $ "get_" ++ show value_name)
    (FunctionType' $ InAndOutType (TypeName' type_name) val_type) >>
  return ("get_" ++ show value_name ++ " :: " ++ show val_type)
  ) :: Field' -> TypeName -> Stateful Haskell

-- OrTypeDefinition': or_type_def_g, or_type_cases_g, or_type_case_g

or_type_def_g = ( \(ValNameAndCases type_name or_type_cases) -> 
  type_map_exists_check type_name >>
  type_map_insert type_name (OrTypeCaseList or_type_cases) >>
  or_type_cases_g or_type_cases type_name >>= \cases_hs ->
  return $
    "\n\ndata " ++ show type_name ++ " =\n  " ++ cases_hs ++ "\n  deriving Show"
  ) :: OrTypeDefinition' -> Stateful Haskell

or_type_cases_g = ( \or_type_cases type_name ->
  mapM (flip or_type_case_g type_name) or_type_cases >>= \cases_hs ->
  return $ intercalate " | " cases_hs 
  ) :: [ OrTypeCase' ] -> TypeName -> Stateful Haskell

or_type_case_g = ( \(NameAndMaybeType value_name maybe_val_type) type_name ->
  value_map_insert value_name ( case maybe_val_type of
    Nothing -> TypeName' type_name
    Just val_type -> FunctionType' $ InAndOutType val_type $ TypeName' type_name )
    >>
  return ("C" ++ show value_name ++ case maybe_val_type of 
    Nothing -> ""
    Just val_type  -> " " ++ show val_type)
  ) :: OrTypeCase' -> TypeName -> Stateful Haskell

-- TypeDefinition: type_definition_g, type_def_g

type_definition_g = type_def_conversion .> type_def_g
  :: TypeDefinition -> Stateful Haskell

type_def_g = ( \case
  TupleTypeDefinition' tuple_type_def -> tuple_type_def_g tuple_type_def
  OrTypeDefinition' or_type_def -> or_type_def_g or_type_def
  ) :: TypeDefinition' -> Stateful Haskell
