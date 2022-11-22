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

{- 
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction, Abstractions
-}

-- Literal 
literal_g = ( vt_shortest_equivalent .> \case
  AbsTypesAndResType [] (TypeName (TN "Int")) -> lit_g

  vt -> error $ "Integer literal cannot have type: " ++ show vt
  ) :: ValueType -> Literal -> Haskell

lit_g = \case
  Constant0 -> "0"
  Constant1 -> "1"
  :: Literal -> Haskell

-- ValueName
value_name_g = ( \(VN vn) -> vn)
  :: ValueName -> Haskell
  
-- LiteralOrValueName
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l

  ValueName vn -> lookup_value_name_g vt vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

lookup_value_name_g = ( \vt vn -> value_map_lookup vn >>= \case
  Nothing -> error $ "Could not find value: " ++ value_name_g vn

  Just lookup_vt -> type_check_value_name_g vt lookup_vt vn
  ) :: ValueType -> ValueName -> Stateful Haskell

type_check_value_name_g = ( \vt lookup_vt vn -> case vt == lookup_vt of 
  False -> error $
    "Value: " ++ show vn ++ "\nhas type: " ++ show lookup_vt ++ "\nnot: " ++ show vt

  True -> return $ value_name_g vn
  ) :: ValueType -> ValueType -> ValueName -> Stateful Haskell

-- TupleMatching
tuple_matching_g = ( \case
    -- possibly later with symbol table ?
  TypeName tn -> error $ "tuple matching TypeName :" ++ show tn

  ParenthesisType vt -> value_type_tuple_matching_g vt

  TupleType vts -> value_types_tuple_matching_g vts 
  ) :: BaseType -> TupleMatching -> Stateful Haskell

value_type_tuple_matching_g = ( \case
  vt@(AbsTypesAndResType (_:_) _) ->
    error $ "trying to match tuple but got type: " ++ show vt

  -- maybe throw some warning or error here ? when could it arise ?
  AbsTypesAndResType [] bt -> tuple_matching_g bt
  ) :: ValueType -> TupleMatching -> Stateful Haskell

value_types_tuple_matching_g = ( \vts (TM vns) -> vns ==> \case
    [] -> error "should not have less than 2 in tuple"

    [ _ ] -> error "should not have less than 2 in tuple"

    _ -> case length vts == length vns of
      False -> error "tuple values and tuple types must be of the same length"

      True -> value_types_value_names_g vts vns
  ) :: [ ValueType ] -> TupleMatching -> Stateful Haskell

value_types_value_names_g = ( \vts vns -> 
  zip vns vts ==> mapM_ value_map_insert >>
  parenthesis_comma_sep_g value_name_g vns ==> return
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
    value_map_insert ( vn, vt ) >> value_name_g vn ==> return

  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- Abstractions
correct_abstractions_g = ( \bts -> \case
  [] -> return ""

  as ->
    let
    bt_abstraction_g = ( \( bt, a ) -> abstraction_g bt a >>= (++ " ") .> return )
      :: ( BaseType, Abstraction ) -> Stateful Haskell
    in
    zip bts as ==> mapM bt_abstraction_g ==> fmap concat >>=
      ("\\" ++) .> (++ "-> ") .> return
  ) :: [ BaseType ] -> [ Abstraction ] -> Stateful Haskell

abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error "abstractions and abstraction types must be of the same length"

  True -> correct_abstractions_g bts as
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell
