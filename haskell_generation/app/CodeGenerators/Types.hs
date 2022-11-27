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
  ( TypeName, BaseType(..), ValueType(..), FieldAndType(..), TupleTypeValue(..)
  , TupleType(..) )
import HaskellTypes.Generation
  ( Stateful, TupleTypeMap, tuple_type_map_lookup, tuple_type_map_insert
  , value_map_insert )

import CodeGenerators.LowLevel
  ( value_name_g )
import CodeGenerators.ErrorMessages
  ( tuple_type_err_msg )

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
tuple_type_g = ( \(NameAndTupleValue tn ttv) -> tuple_type_map_lookup tn >>= \case
  Just _ -> error tuple_type_err_msg

  Nothing ->
    let
    fatl = ttv ==> \(FieldAndTypeList l) -> l
      :: [ FieldAndType ]

    additional_bt = TypeName tn
      :: BaseType

    field_and_type_g = ( \(FT vn vt@(AbsTypesAndResType bts bt) ) ->
      value_map_insert
        (VN $ "get_" ++ value_name_g vn) (AbsTypesAndResType (additional_bt : bts) bt) >>
      return ("get_" ++ value_name_g vn ++ " :: " ++ value_type_g vt)
      ) :: FieldAndType -> Stateful Haskell

    tuple_value_g =
      fatl==>mapM field_and_type_g >>= \fatl_g ->
      return $ type_name_g tn ++ "C { " ++ intercalate ", " fatl_g ++ " }"
      :: Stateful Haskell
    in
    tuple_type_map_insert tn fatl >> tuple_value_g >>= \tv_g ->
    return $ "data " ++ type_name_g tn ++ " =\n  " ++ tv_g ++ "\n  deriving Show\n"
  ) :: TupleType -> Stateful Haskell
