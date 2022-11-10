{-# LANGUAGE LambdaCase #-}

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
  ( Stateful, TupleTypeMap, get_tuple_type_map, update_tuple_type_map )

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
  "get_" ++ value_name_g vt vn ++ " :: " ++ value_type_g vt
  ) :: FieldAndType -> Haskell

tuple_value_g = ( \(FieldAndTypeList fatl) ->
  "C { " ++ fatl-->map field_and_type_g--> intercalate ", " ++ " }"
  ) :: TupleTypeValue -> Haskell

tuple_type_g = ( \(NameAndTuple tn ttv) ->
  get_tuple_type_map >>= \ttm ->
  ( case M.lookup tn ttm of 
      Just _ -> error "tuple_type of the same name already defined"
      Nothing ->
        let
        vns = ttv --> \(FieldAndTypeList fatl) -> fatl --> map (\(FT vn _) -> vn)
          :: [ ValueName ]
        new_map = M.insert tn vns ttm
          :: TupleTypeMap
        in 
        update_tuple_type_map new_map ) >>
  let
  tn_g = type_name_g tn
  deriving_show = "\n  deriving Show"
  in
  return $
    "data " ++ tn_g ++ " =\n  " ++ tn_g ++ tuple_value_g ttv ++ deriving_show ++ "\n"
  ) :: TupleType -> Stateful Haskell
