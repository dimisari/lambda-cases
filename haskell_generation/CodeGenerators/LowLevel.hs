{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import Prelude
  ( Bool(..), Maybe(..), (>>=), (++), (==), ($), (>>), (<$>), concatMap, error, zip
  , init, last, map, length, show, return, undefined, concat, fmap, mapM, mapM_ )
import qualified Data.Map as M ( lookup )

import Helpers ( Haskell, (-->), (.>), parenthesis_comma_sep_g )
import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), LiteralOrValueName(..), TupleMatching(..)
  , Abstraction(..), Abstractions(..) )
import HaskellTypes.Types ( BaseType(..), ValueType(..), TypeName(..) )

import HaskellTypes.Generation ( Stateful, value_map_lookup, value_map_insert )

{- 
  All:
  Literal, ValueName, LiteralOrValueName, TupleMatching, Abstraction, Abstractions
-}

-- Literal 
lit_g = \case
  Constant0 -> "0"
  Constant1 -> "1"
  :: Literal -> Haskell

literal_g = ( \case
  AbsTypesAndResType [] (TypeName (TN "Int")) -> lit_g
  vt -> error $ "Integer literal cannot have type: " ++ show vt
  ) :: ValueType -> Literal -> Haskell

-- ValueName
value_name_g = ( \(VN vn) -> vn)
  :: ValueName -> Haskell
  
-- LiteralOrValueName
literal_or_value_name_g = ( \vt -> \case
  Literal l -> return $ literal_g vt l

  ValueName vn -> value_map_lookup vn >>= \case
    Nothing -> error $ "Could not find value: " ++ value_name_g vn

    Just lookup_vt -> case lookup_vt == vt of 
      False -> error $ "Value has type: " ++ show lookup_vt ++ "\nnot: " ++ show vt

      True -> return $ value_name_g vn
  ) :: ValueType -> LiteralOrValueName -> Stateful Haskell

-- TupleMatching
tuple_matching_g = ( \case
    -- possibly later with symbol table ?
  TypeName tn -> error $ "tuple matching TypeName :" ++ show tn

  ParenthesisType vt -> vt --> \case
    AbsTypesAndResType (_:_) _ ->
      error $ "trying to match tuple but got type: " ++ show vt

    -- maybe throw some warning or error here ? when could it arise ?
    AbsTypesAndResType [] bt -> tuple_matching_g bt

  TupleType vts -> \(FieldNames vns) -> vns --> \case
    [] -> error "should not have less than 2 in tuple"
    [ _ ] -> error "should not have less than 2 in tuple"

    _ -> case length vts == length vns of
      False -> error "tuple values and tuple types must be of the same length"

      True ->
        zip vns vts-->mapM_ value_map_insert >>
        parenthesis_comma_sep_g value_name_g vns --> return
  ) :: BaseType -> TupleMatching -> Stateful Haskell

-- Abstraction
abstraction_g = ( \bt -> \case
  ValueNameAb vn ->
    let
    vt = ( bt --> \case
      ParenthesisType vt -> vt
      _ -> AbsTypesAndResType [] bt
      ) :: ValueType 
    in
    value_map_insert ( vn, vt ) >> value_name_g vn --> return

  TupleMatching tm -> tuple_matching_g bt tm
  ) :: BaseType -> Abstraction -> Stateful Haskell

-- Abstractions
abstractions_g = ( \bts (As as) -> case length bts == length as of
  False -> error "abstractions and abstraction types must be of the same length"

  True -> case as of
    [] -> return ""
    _ ->
      zip bts as-->mapM ( \( bt, a ) -> abstraction_g bt a >>= (++ " ") .> return )
        -->fmap concat >>= ("\\" ++) .> (++ "-> ") .> return
  ) :: [ BaseType ] -> Abstractions -> Stateful Haskell
