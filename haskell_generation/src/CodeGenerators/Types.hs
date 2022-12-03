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
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..), TupleType(..)
  , OrType(..), CaseAndMaybeType(..), FieldsOrCases(..), Type(..) )
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_insert, type_map_exists_check )
import CodeGenerators.LowLevel
  ( value_name_g )

-- All: TypeName, BaseType, ValueType, TupleType

-- TypeName
type_name_g = show
  :: TypeName -> Haskell

-- BaseType
base_type_g = ( \case
  ParenTupleType vts -> paren_comma_sep_g value_type_g vts
  ParenthesisType vt -> case vt of
    (AbsTypesAndResType [] bt) -> base_type_g bt
    _ -> "(" ++ value_type_g vt ++ ")"
  TypeName tn -> type_name_g tn
  ) :: BaseType -> Haskell

-- ValueType
value_type_g = ( \(AbsTypesAndResType bts bt) -> 
  bts==>concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
  ) :: ValueType -> Haskell

-- TupleType
tuple_type_g = ( \(NameAndValue tn ttv) ->
  let
  tuple_value_g =
    ttv==>mapM field_and_type_g >>= \ttv_g ->
    return $ type_name_g tn ++ "C { " ++ intercalate ", " ttv_g ++ " }"
    :: Stateful Haskell

  field_and_type_g = ( \(FT vn vt@(AbsTypesAndResType bts bt) ) ->
    value_map_insert
      (VN $ "get_" ++ value_name_g vn)
      (AbsTypesAndResType (TypeName tn : bts) bt) >>
    return ("get_" ++ value_name_g vn ++ " :: " ++ value_type_g vt)
    ) :: FieldAndType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (FieldAndTypeList ttv) >> tuple_value_g >>= \tv_g ->
  return $ "data " ++ type_name_g tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleType -> Stateful Haskell

-- OrType
or_type_g = ( \(NameAndValues tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_type_g = ( \(CT vn mvt) ->
    return $ "C" ++ value_name_g vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt
    ) :: CaseAndMaybeType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (CaseAndMaybeTypeList otvs) >> or_values_g >>= \otvs_g ->
  return $ "data " ++ type_name_g tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show\n"
  ) :: OrType -> Stateful Haskell

-- Type
type_g = ( \case
  TupleType tt -> tuple_type_g tt
  OrType ot -> or_type_g ot
  ) :: Type -> Stateful Haskell
