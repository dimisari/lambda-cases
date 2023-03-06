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
  ( ValType(..), FieldAndValType(..), TupleValTypeDef(..)
  , CaseAndMaybeValType(..), ValOrTypeDef(..), ValFieldsOrCases(..)
  , ValTypeDef(..), value_type_to_val_type )
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_insert, type_map_exists_check )

-- All: BaseType, ValueType, TupleTypeDef, OrTypeDef, TypeDef

-- BaseType
-- base_type_g = ( \case
--   TypeName tn -> show tn
--   TupleType vt1 vt2 vts ->
--     "(" ++ intercalate ", " (map value_type_g (vt1 : vt2 : vts)) ++ ")" 
--   ParenType vt -> case vt of
--     (AbsTypesAndResType [] bt) -> base_type_g bt
--     _ -> "(" ++ value_type_g vt ++ ")"
--   ) :: BaseType -> Haskell

-- base_type_g = ( base_type_to_val_type .> show )
--   :: BaseType -> Haskell

-- ValueType
-- value_type_g = ( \(AbsTypesAndResType bts bt) -> 
--   bts==>concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
--   ) :: ValueType -> Haskell

value_type_g = ( value_type_to_val_type .> show )
  :: ValueType -> Haskell

-- ValType
val_type_g = ( \case
  FunctionType t_in t_out -> val_type_g t_in ++ " -> " ++ val_type_g t_out
  NamedType tn -> show tn
  TupleValType t1 t2 ts -> 
    "(" ++ map val_type_g (t1 : t2 : ts)==>intercalate ", " ++ ")" 
  ) :: ValType -> Haskell

-- TupleValTypeDef
tuple_val_type_def_g = ( \(NameAndFields tn fs) ->
  let
  tuple_value_g =
    fs==>mapM field_and_val_type_g >>= \fs_g ->
    return $ show tn ++ "C { " ++ intercalate ", " fs_g ++ " }"
    :: Stateful Haskell

  field_and_val_type_g = ( \(FVT vn vt) ->
    value_map_insert (VN $ "get_" ++ show vn) (FunctionType (NamedType tn) vt) >>
    return ("get_" ++ show vn ++ " :: " ++ val_type_g vt)
    ) :: FieldAndValType -> Stateful Haskell
  in
  type_map_exists_check tn >>
  type_map_insert tn (FieldAndValTypeList fs) >> tuple_value_g >>= \tv_g ->
  return $ "\ndata " ++ show tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleValTypeDef -> Stateful Haskell

-- ValOrTypeDef
val_or_type_def_g = ( \(ValNameAndValues tn otvs) -> 
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
  ) :: ValOrTypeDef -> Stateful Haskell

-- ValTypeDef
val_type_def_g = ( \case
  TupleValTypeDef tt -> tuple_val_type_def_g tt
  ValOrTypeDef ot -> val_or_type_def_g ot
  ) :: ValTypeDef -> Stateful Haskell
