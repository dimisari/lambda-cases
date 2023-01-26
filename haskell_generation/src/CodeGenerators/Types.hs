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
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..)
  , TupleTypeDef(..), OrTypeDef(..), CaseAndMaybeType(..), FieldsOrCases(..)
  , TypeDef(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), FieldAndValType(..), TupleValTypeDef(..)
  , CaseAndMaybeValType(..), ValOrTypeDef(..), ValFieldsOrCases(..)
  , ValTypeDef(..) )
import HaskellTypes.Generation
  ( Stateful
  , val_map_insert, val_type_map_insert, val_type_map_exists_check
  , value_map_insert, type_map_insert, type_map_exists_check )

-- All: BaseType, ValueType, TupleTypeDef, OrTypeDef, TypeDef

-- BaseType
base_type_g = ( \case
  TypeName tn -> show tn
  TupleType vt1 vt2 vts ->
    "(" ++ intercalate ", " (map value_type_g (vt1 : vt2 : vts)) ++ ")" 
  ParenType vt -> case vt of
    (AbsTypesAndResType [] bt) -> base_type_g bt
    _ -> "(" ++ value_type_g vt ++ ")"
  ) :: BaseType -> Haskell

-- ValueType
value_type_g = ( \(AbsTypesAndResType bts bt) -> 
  bts==>concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
  ) :: ValueType -> Haskell

-- ValType
val_type_g = ( \case
  FunctionType t_in t_out -> val_type_g t_in ++ " -> " ++ val_type_g t_out
  NamedType tn -> show tn
  TupleValType t1 t2 ts -> 
    "(" ++ map val_type_g (t1 : t2 : ts)==>intercalate ", " ++ ")" 
  ) :: ValType -> Haskell

-- TupleTypeDef
tuple_type_def_g = ( \(NameAndValue tn ttv) ->
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

-- TupleValTypeDef
tuple_val_type_def_g = ( \(NameAndFields tn fs) ->
  let
  tuple_value_g =
    fs==>mapM field_and_val_type_g >>= \fs_g ->
    return $ show tn ++ "C { " ++ intercalate ", " fs_g ++ " }"
    :: Stateful Haskell

  field_and_val_type_g = ( \(FVT vn vt) ->
    val_map_insert (VN $ "get_" ++ show vn) (FunctionType (NamedType tn) vt) >>
    return ("get_" ++ show vn ++ " :: " ++ val_type_g vt)
    ) :: FieldAndValType -> Stateful Haskell
  in
  val_type_map_exists_check tn >>
  val_type_map_insert tn (FieldAndValTypeList fs) >> tuple_value_g >>= \tv_g ->
  return $ "\ndata " ++ show tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleValTypeDef -> Stateful Haskell

-- OrTypeDef
or_type_def_g = ( \(NameAndValues tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_type_g = ( \(CMT vn mvt) ->
    ( case mvt of
      Nothing -> value_map_insert vn $ AbsTypesAndResType [] $ TypeName $ tn
      _ -> return () ) >>
    return ("C" ++ show vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt)
    ) :: CaseAndMaybeType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (CaseAndMaybeTypeList otvs) >> or_values_g >>= \otvs_g ->
  return $ "\n\ndata " ++ show tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show"
  ) :: OrTypeDef -> Stateful Haskell

-- ValOrTypeDef
val_or_type_def_g = ( \(ValNameAndValues tn otvs) -> 
  let
  or_values_g =
    otvs==>mapM case_and_maybe_val_type_g >>= \otvs_g ->
    return $ intercalate " | " otvs_g 
    :: Stateful Haskell

  case_and_maybe_val_type_g = ( \(CMVT vn mvt) ->
    ( case mvt of
      Nothing -> val_map_insert vn $ NamedType tn
      _ -> return () ) >>
    return ("C" ++ show vn ++ case mvt of 
      Nothing -> ""
      Just vt  -> " " ++ show vt)
    ) :: CaseAndMaybeValType -> Stateful Haskell
  in
  val_type_map_exists_check tn >>
  val_type_map_insert tn (CaseAndMaybeValTypeList otvs) >> or_values_g >>=
    \otvs_g ->
  return $ "\n\ndata " ++ show tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show"
  ) :: ValOrTypeDef -> Stateful Haskell

-- TypeDef
type_def_g = ( \case
  TupleTypeDef tt -> tuple_type_def_g tt
  OrTypeDef ot -> or_type_def_g ot
  ) :: TypeDef -> Stateful Haskell

-- ValTypeDef
val_type_def_g = ( \case
  TupleValTypeDef tt -> tuple_val_type_def_g tt
  ValOrTypeDef ot -> val_or_type_def_g ot
  ) :: ValTypeDef -> Stateful Haskell
