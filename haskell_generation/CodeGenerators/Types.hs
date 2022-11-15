{-# language LambdaCase #-}

module CodeGenerators.Types where

import Prelude ( Maybe(..), (++), ($), (>>), (>>=), concatMap, map, show, return, error )
import Data.List ( intercalate )
import qualified Data.Map as M ( insert, lookup )

import Helpers ( Haskell, (-->), (.>), parenthesis_comma_sep_g )

import HaskellTypes.LowLevel ( ValueName(..) )
import CodeGenerators.LowLevel ( value_name_g )

import HaskellTypes.Types
  ( TypeName, BaseType(..), ValueType(..), FieldAndType(..), TupleTypeValue(..)
  , TupleType(..) )
import HaskellTypes.Generation
  ( Stateful, TupleTypeMap, get_tuple_type_map, update_tuple_type_map
  , tuple_type_map_lookup, tuple_type_map_insert )

-- TypeName
type_name_g = show
  :: TypeName -> Haskell

-- BaseType
base_type_g = ( \case
  TupleType vts -> parenthesis_comma_sep_g value_type_g vts

  ParenthesisType vt -> case vt of
    (AbstractionTypesAndResultType [] bt) -> base_type_g bt
    _ -> "(" ++ value_type_g vt ++ ")"

  TypeName tn -> type_name_g tn
  ) :: BaseType -> Haskell

-- ValueType
value_type_g = ( \(AbstractionTypesAndResultType bts bt) -> 
  bts-->concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
  ) :: ValueType -> Haskell

-- FieldAndType
field_and_type_g = ( \(FT vn vt) ->
  "get_" ++ value_name_g vn ++ " :: " ++ value_type_g vt
  ) :: FieldAndType -> Haskell

-- TupleTypeValue
tuple_value_g = ( \(FieldAndTypeList fatl) ->
  "C { " ++ fatl-->map field_and_type_g--> intercalate ", " ++ " }"
  ) :: TupleTypeValue -> Haskell

-- TupleType
tuple_type_g = ( \(NameAndTupleValue tn ttv) ->
  let
  tuple_type_map_operations = ( tuple_type_map_lookup tn >>= \case
    Just _ -> error "tuple_type of the same name already defined"
    Nothing ->
      tuple_type_map_insert
      ( tn, ttv --> \(FieldAndTypeList fatl) -> fatl --> map (\(FT vn _) -> vn) )
    ) :: Stateful ()
  in
  tuple_type_map_operations >> return (
    "data " ++ type_name_g tn ++ " =\n  " ++
    type_name_g tn ++ tuple_value_g ttv ++ "\n  deriving Show\n"
    )
  ) :: TupleType -> Stateful Haskell
