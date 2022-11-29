{-# language LambdaCase #-}

module CodeGenerators.Types where

import Data.List
  ( intercalate )
import qualified Data.Map as M
  ( insert, lookup )

import Helpers
  ( Haskell, (==>), (.>), parenthesis_comma_sep_g )

import HaskellTypes.LowLevel
  ( ValueName(..) )
import HaskellTypes.Types
  ( TypeName(..), BaseType(..), ValueType(..), FieldAndType(..), TupleType(..)
  , OrType(..), CaseAndType(..), FieldsOrCases(..) )
import HaskellTypes.Generation
  ( Stateful, value_map_insert, type_map_lookup
  , type_map_insert )

import CodeGenerators.LowLevel
  ( value_name_g )
import CodeGenerators.ErrorMessages
  ( tuple_type_err_msg, or_type_err_msg )

-- All: TypeName, BaseType, ValueType, TupleType

-- TypeName
type_name_g = show
  :: TypeName -> Haskell

-- BaseType
base_type_g = ( \case
  TupleType vts -> parenthesis_comma_sep_g value_type_g vts

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
tuple_type_g = ( \(NameAndValue tn ttv) -> type_map_lookup tn >>= \case
  Just _ -> error tuple_type_err_msg

  Nothing ->
    let
    field_and_type_g = ( \(FT vn vt@(AbsTypesAndResType bts bt) ) ->
      value_map_insert
        (VN $ "get_" ++ value_name_g vn)
        (AbsTypesAndResType (TypeName tn : bts) bt) >>
      return ("get_" ++ value_name_g vn ++ " :: " ++ value_type_g vt)
      ) :: FieldAndType -> Stateful Haskell

    tuple_value_g =
      ttv==>mapM field_and_type_g >>= \ttv_g ->
      return $ type_name_g tn ++ "C { " ++ intercalate ", " ttv_g ++ " }"
      :: Stateful Haskell
    in
    type_map_insert tn (FieldAndTypeList ttv) >> tuple_value_g >>= \tv_g ->
    return $ "data " ++ type_name_g tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleType -> Stateful Haskell

-- OrType
or_type_g = ( \(NameAndValues tn otvs) -> type_map_lookup tn >>= \case
  Just _ -> error or_type_err_msg

  Nothing ->
    let
    case_and_type_g = ( \(CT vn vt ) ->
      value_map_insert
        (VN $ "is_" ++ value_name_g vn)
        (AbsTypesAndResType [ TypeName tn ] $ TypeName $ TN "Bool") >>
      return ( "C" ++ value_name_g vn ++ " " ++ show vt )
      ) :: CaseAndType -> Stateful Haskell

    or_values_g =
      otvs==>mapM case_and_type_g >>= \otvs_g ->
      return $ intercalate " | " otvs_g 
      :: Stateful Haskell
    in
    type_map_insert tn (CaseAndTypeList otvs) >> or_values_g >>= \otvs_g ->
    return $
      "data " ++ type_name_g tn ++ " =\n  " ++ otvs_g ++ "\n  deriving Show\n"
  ) :: OrType -> Stateful Haskell
