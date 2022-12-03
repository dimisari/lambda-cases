{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import qualified Data.Map as M
  ( lookup )

import Helpers
  ( Haskell, (==>), (.>), paren_comma_sep_g )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types
  ( BaseType(..), ValueType(..), TypeName(..), vt_shortest_equivalent )
import HaskellTypes.Generation
  ( Stateful, value_map_get, value_map_insert )

import CodeGenerators.ErrorMessages
  ( literal_not_int_err, type_check_err, tuple_matching_err
  , tuple_function_type_err, tuple_less_than_2_err
  , tuple_values_types_lengths_dont_match_err
  , abstractions_types_lengths_dont_match_err )

{-
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction, Abstractions
-}

-- Literal 
literal_g = ( vt_shortest_equivalent .> \case
  AbsTypesAndResType [] (TypeName (TN "Int")) -> show
  vt -> error $ literal_not_int_err vt
  ) :: ValueType -> Literal -> Haskell

-- ValueName
value_name_g = ( \(VN vn) -> vn)
  :: ValueName -> Haskell
  
-- LiteralOrValueName
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l
  ValueName vn ->
    value_map_get vn >>= \lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn -> case vt == lookup_vt of 
  False -> error $ type_check_err vn lookup_vt vt 
  True -> return $ value_name_g vn
  ) :: ValueType -> ValueType -> ValueName -> Stateful Haskell

-- TupleMatching
tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  TypeName tn -> error $ tuple_matching_err tn
  ParenthesisType vt -> value_type_tuple_matching_g vt
  ParenTupleType vts -> value_types_tuple_matching_g vts 
  ) :: BaseType -> TupleMatching -> Stateful Haskell

value_type_tuple_matching_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> error $ tuple_function_type_err vt
  -- maybe throw some warning or error here ? when could it arise ?
  AbsTypesAndResType [] bt -> tuple_matching_g bt
  ) :: ValueType -> TupleMatching -> Stateful Haskell

value_types_tuple_matching_g = ( \vts (TM vns) -> vns ==> \case
    [] -> error tuple_less_than_2_err
    [ _ ] -> error tuple_less_than_2_err
    _ -> case length vts == length vns of
      False -> error tuple_values_types_lengths_dont_match_err
      True -> correct_value_types_value_names_g vts vns
  ) :: [ ValueType ] -> TupleMatching -> Stateful Haskell

correct_value_types_value_names_g = ( \vts vns -> 
  zipWith value_map_insert vns vts==>sequence_ >>
  paren_comma_sep_g value_name_g vns==>return
  ) :: [ ValueType ] -> [ ValueName ] -> Stateful Haskell

-- Abstraction
abstraction_g = ( \bt -> \case
  ValueNameAb vn -> value_map_insert vn vt >> value_name_g vn ==> return where
    vt = ( bt ==> \case
      ParenthesisType vt -> vt
      _ -> AbsTypesAndResType [] bt
      ) :: ValueType 
  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- Abstractions
abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error abstractions_types_lengths_dont_match_err
  True -> correct_abstractions_g bts as
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell

correct_abstractions_g = ( \bts -> \case
  [] -> return ""
  as ->
    zipWith bt_abstraction_g bts as==>sequence==>fmap concat >>= \as_g ->
    return $ "\\" ++ as_g ++ "-> "
    where
    bt_abstraction_g = ( \bt a -> abstraction_g bt a >>= (++ " ") .> return )
      :: BaseType -> Abstraction -> Stateful Haskell
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell
