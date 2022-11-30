{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import qualified Data.Map as M
  ( lookup )

import Helpers
  ( Haskell, (==>), (.>), parenthesis_comma_sep_g )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types
  ( BaseType(..), ValueType(..), TypeName(..), vt_shortest_equivalent )
import HaskellTypes.Generation
  ( Stateful, value_map_lookup, value_map_insert )

import CodeGenerators.ErrorMessages
  ( literal_err_msg, type_check_value_name_err_msg, tuple_matching_err_msg
  , value_type_tuple_matching_err_msg
  , value_types_tuple_matching_err_msg1, value_types_tuple_matching_err_msg2
  , abstractions_err_msg )

{-
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction, Abstractions
-}

-- Literal 
literal_g = ( vt_shortest_equivalent .> \case
  AbsTypesAndResType [] (TypeName (TN "Int")) -> show
  vt -> error $ literal_err_msg vt
  ) :: ValueType -> Literal -> Haskell

-- ValueName
value_name_g = ( \(VN vn) -> vn)
  :: ValueName -> Haskell
  
-- LiteralOrValueName
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l
  ValueName vn ->
    value_map_lookup vn >>= \lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn -> case vt == lookup_vt of 
  False -> error $ type_check_value_name_err_msg vn lookup_vt vt 
  True -> return $ value_name_g vn
  ) :: ValueType -> ValueType -> ValueName -> Stateful Haskell

-- TupleMatching
tuple_matching_g = ( \case
  -- possibly later with symbol table ?
  TypeName tn -> error $ tuple_matching_err_msg tn
  ParenthesisType vt -> value_type_tuple_matching_g vt
  TupleType vts -> value_types_tuple_matching_g vts 
  ) :: BaseType -> TupleMatching -> Stateful Haskell

value_type_tuple_matching_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) -> error $
    value_type_tuple_matching_err_msg vt
  -- maybe throw some warning or error here ? when could it arise ?
  AbsTypesAndResType [] bt -> tuple_matching_g bt
  ) :: ValueType -> TupleMatching -> Stateful Haskell

value_types_tuple_matching_g = ( \vts (TM vns) -> vns ==> \case
    [] -> error value_types_tuple_matching_err_msg1
    [ _ ] -> error value_types_tuple_matching_err_msg1
    _ -> case length vts == length vns of
      False -> error value_types_tuple_matching_err_msg2
      True -> value_types_value_names_g vts vns
  ) :: [ ValueType ] -> TupleMatching -> Stateful Haskell

value_types_value_names_g = ( \vts vns -> 
  zipWith value_map_insert vns vts==>sequence_ >>
  parenthesis_comma_sep_g value_name_g vns==>return
  ) :: [ ValueType ] -> [ ValueName ] -> Stateful Haskell

-- Abstraction
abstraction_g = ( \bt -> \case
  ValueNameAb vn ->
    let
    vt = ( bt ==> \case
      ParenthesisType vt -> vt
      _ -> AbsTypesAndResType [] bt
      ) :: ValueType 
    in
    value_map_insert vn vt >> value_name_g vn ==> return

  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- Abstractions
abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error abstractions_err_msg

  True -> correct_abstractions_g bts as
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell

correct_abstractions_g = ( \bts -> \case
  [] -> return ""

  as ->
    let
    bt_abstraction_g = ( \bt a -> abstraction_g bt a >>= (++ " ") .> return )
      :: BaseType -> Abstraction -> Stateful Haskell
    in
    zipWith bt_abstraction_g bts as==>sequence==>fmap concat >>= \as_g ->
    return $ "\\" ++ as_g ++ "-> "
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell
