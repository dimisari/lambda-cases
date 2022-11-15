{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude
  ( Bool(..), Maybe(..), (>>=), (++), (==), ($), (>>), concatMap, error, zip, init, last
  , map, length, show, return, undefined, concat, fmap, mapM, mapM_ )
import qualified Data.Map as M ( lookup )

import Helpers ( Haskell, (-->), parenthesis_comma_sep_g )
import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types ( BaseType(..), ValueType(..), TypeName(..) )

import HaskellTypes.Generation ( Stateful, value_map_lookup, value_map_insert )

lit_g = \case
  Constant0 -> "0"
  Constant1 -> "1"
  :: Literal -> Haskell

literal_g = ( \case
  AbstractionTypesAndResultType [] (TypeName (TN "Int")) -> lit_g
  vt -> error $ "Integer literal cannot have type: " ++ show vt
  ) :: ValueType -> Literal -> Haskell

value_name_g = ( \(VN vn) -> vn)
  :: ValueName -> Haskell
  
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l
  ValueName vn -> 
    let
    check_type_equality = ( \lookup_vt -> case lookup_vt == vt of 
      False -> error $ "Value has type: " ++ show lookup_vt ++ " not: " ++ show vt
      True -> return $ value_name_g vn
      ) :: ValueType -> Stateful Haskell
    in
    value_map_lookup vn >>= \case
      Nothing -> error $ "Could not find value: " ++ value_name_g vn
      Just lookup_vt -> check_type_equality lookup_vt
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

tuple_matching_g = ( \case
  TypeName tn ->
    -- possibly later with symbol table ?
    error $ "tuple matching TypeName :" ++ show tn
  ParenthesisType (AbstractionTypesAndResultType [] bt) ->
    tuple_matching_g bt
  TupleType vts -> \(FieldNames vns) -> vns --> \case
    [] -> error "should not have less than 2 in tuple"
    [ _ ] -> error "should not have less than 2 in tuple"
    _ -> case length vts == length vns of
      False -> error "tuple values and tuple types must be of the same length"
      True ->
        zip vns vts-->mapM_ value_map_insert >>
        parenthesis_comma_sep_g value_name_g vns --> return
  ) :: BaseType -> TupleMatching -> Stateful Haskell

abstraction_g = ( \bt -> \case
  ValueNameAb vn -> return $ value_name_g vn
  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error "abstractions and abstraction types must be of the same length"
  True ->
    let
    insert_all_in_value_map =
      undefined
      :: Stateful ()
    map_abstraction_g_concat = 
      zip bts as
        -->mapM ( \(bt, a) -> abstraction_g bt a >>= \a_g -> return $ a_g ++ " ")
        -->fmap concat
      :: Stateful Haskell
    in
    insert_all_in_value_map >>
    map_abstraction_g_concat >>= \as_g ->
    return $ case as of
      [] -> ""
      _ -> "\\" ++ as_g ++ "-> "
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell
