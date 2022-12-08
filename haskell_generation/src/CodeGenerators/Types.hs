{-# language LambdaCase #-}

module CodeGenerators.Types where

import Data.List
  ( intercalate )
import qualified Data.Map as M
  ( insert, lookup )

import Helpers
  ( Haskell, (==>), (.>), paren_comma_sep_g )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..), ParenType(..), BaseType(..), ValueType(..), FieldAndType(..)
  , TupleTypeDef(..), OrTypeDef(..), CaseAndMaybeType(..), FieldsOrCases(..)
  , TypeDef(..) )
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_insert, type_map_exists_check )

-- All: ParenType, ValueType, TupleTypeDef

-- ParenType
paren_type_g = ( \case
  TupleType vts -> paren_comma_sep_g value_type_g vts
  ParenVT vt -> case vt of
    (AbsTypesAndResType [] bt) -> base_type_g bt
    _ -> "(" ++ value_type_g vt ++ ")"
  ) :: ParenType -> Haskell

-- ParenType
base_type_g = ( \case
  ParenType pt -> paren_type_g pt
  TypeName tn -> show tn
  ) :: BaseType -> Haskell

-- ValueType
value_type_g = ( \(AbsTypesAndResType bts bt) -> 
  bts==>concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
  ) :: ValueType -> Haskell

-- TupleTypeDef
tuple_type_g = ( \(NameAndValue tn ttv) ->
  let
  tuple_value_g =
    ttv==>mapM field_and_type_g >>= \ttv_g ->
    return $ show tn ++ "C { " ++ intercalate ", " ttv_g ++ " }"
    :: Stateful Haskell

  field_and_type_g = ( \(FT vn vt@(AbsTypesAndResType bts bt) ) ->
    value_map_insert
      (VN $ "get_" ++ show vn)
      (AbsTypesAndResType (TypeName tn : bts) bt) >>
    return ("get_" ++ show vn ++ " :: " ++ value_type_g vt)
    ) :: FieldAndType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (FieldAndTypeList ttv) >> tuple_value_g >>= \tv_g ->
  return $ "\ndata " ++ show tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleTypeDef -> Stateful Haskell

-- OrTypeDef
or_type_g = ( \(NameAndValues tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_type_g = ( \(CT vn mvt) ->
    return $ "C" ++ show vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt
    ) :: CaseAndMaybeType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (CaseAndMaybeTypeList otvs) >> or_values_g >>= \otvs_g ->
  return $ "\n\ndata " ++ show tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show"
  ) :: OrTypeDef -> Stateful Haskell

-- Type
type_g = ( \case
  TupleTypeDef tt -> tuple_type_g tt
  OrTypeDef ot -> or_type_g ot
  ) :: TypeDef -> Stateful Haskell
