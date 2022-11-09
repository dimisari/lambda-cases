{-# LANGUAGE LambdaCase #-}

module CodeGenerators.Types where

import Prelude ( (++), concatMap, map, show )
import Data.List ( intercalate )

import Helpers ( Haskell, (-->), (.>), parenthesis_comma_sep_g )
import HaskellTypes.Types
  ( TypeName, BaseType(..), ValueType(..), FieldAndType(..), TupleValue(..)
  , TupleType(..) )
import CodeGenerators.LowLevel ( value_name_g )

type_name_g = show
  :: TypeName -> Haskell

base_type_g = ( \case
  TupleType vts -> parenthesis_comma_sep_g value_type_g vts
  ParenthesisType vt -> case vt of
    (AbstractionTypesAndResultType [] bt) -> base_type_g bt
    _ -> "(" ++ value_type_g vt ++ ")"
  TypeName tn -> type_name_g tn
  ) :: BaseType -> Haskell

value_type_g = ( \(AbstractionTypesAndResultType bts bt) -> 
  bts-->concatMap (base_type_g .> (++ " -> ")) ++ base_type_g bt
  ) :: ValueType -> Haskell

field_and_type_g = ( \(FT vn vt) ->
  "get_" ++ value_name_g vn ++ " :: " ++ value_type_g vt
  ) :: FieldAndType -> Haskell

tuple_value_g = ( \(FieldAndTypeList fatl) ->
  "C { " ++ fatl-->map field_and_type_g--> intercalate ", " ++ " }"
  ) :: TupleValue -> Haskell

tuple_type_g = ( \(NameAndTuple tn tv) ->
  let tn_g = type_name_g tn in
  "data " ++ tn_g ++ " =\n  " ++ tn_g ++ tuple_value_g tv ++ "\n"
  ) :: TupleType -> Haskell
