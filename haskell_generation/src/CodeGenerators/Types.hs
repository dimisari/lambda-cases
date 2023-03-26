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
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_insert, type_map_exists_check )

-- All: ValType, TupleTypeDefinition, OrTypeDefinition, TypeDefinition

-- ValType

val_type_g = show
  :: ValType -> Haskell

-- TupleTypeDef

tuple_val_type_def_g = ( \(TTNameAndFields tn fs) ->
  let
  tuple_g =
    fs==>mapM (field_and_val_type_g tn) >>= \fs_g ->
    return $ show tn ++ "C { " ++ intercalate ", " fs_g ++ " }"
    :: Stateful Haskell

  in
  type_map_exists_check tn >>
  type_map_insert tn (FieldList fs) >> tuple_g >>= \tv_g ->
  return $ "\ndata " ++ show tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleTypeDef -> Stateful Haskell

field_and_val_type_g = ( \tn (FNameAndType vn vt) ->
  value_map_insert
    (VN $ "get_" ++ show vn)
    (FuncType $ InAndOutType (NamedType tn) vt) >>
  return ("get_" ++ show vn ++ " :: " ++ val_type_g vt)
  ) :: TypeName -> Field -> Stateful Haskell

-- OrTypeDef

val_or_type_def_g = ( \(ValNameAndCases tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_val_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_val_type_g = ( \(NameAndMaybeType vn mvt) ->
    ( case mvt of
      Nothing -> value_map_insert vn $ NamedType tn
      _ -> return () ) >>
    return ("C" ++ show vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt)
    ) :: OrTypeCase -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (OrTypeCaseList otvs) >> or_values_g >>=
    \otvs_g ->
  return $ "\n\ndata " ++ show tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show"
  ) :: OrTypeDef -> Stateful Haskell

-- TypeDef

val_type_def_g = ( \case
  TupleTypeDef tt -> tuple_val_type_def_g tt
  OrTypeDef ot -> val_or_type_def_g ot
  ) :: TypeDef -> Stateful Haskell
