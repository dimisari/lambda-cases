{-# language LambdaCase #-}

module CodeGenerators.LowLevel where

import Data.List
  ( intercalate )
import qualified Data.Map as M
  ( lookup )
import Control.Monad
  ( (>=>) )

import Helpers
  ( Haskell, (==>), (.>) )

import HaskellTypes.LowLevel
  ( Literal(..), ValueName(..), Abstraction(..), ManyAbstractions(..), Input(..) )
import HaskellTypes.Types
  ( TypeName(..) )
import HaskellTypes.AfterParsing
  ( ValType(..), ValFieldsOrCases(..), FieldAndValType(..) )

import HaskellTypes.Generation
  ( Stateful, value_map_get, value_map_insert, type_map_get )

import CodeGenerators.ErrorMessages
import CodeGenerators.TypeChecking
  ( type_check_value_name_g, type_name_to_val_type )

-- All: Literal, ValueName, Abstraction, ManyAbstractions

-- Literal: literal_g, literal_type_inference_g

literal_g = ( \val_type literal -> 
  (val_type == NamedType (TN "Int")) ==> \case
    True -> return $ show literal
    False -> error $ literal_not_int_err val_type
  ) :: ValType -> Literal -> Stateful Haskell

literal_type_inference_g = ( \literal ->
  return (NamedType $ TN "Int", show literal)
  ) :: Literal -> Stateful (ValType, Haskell)

-- ValueName: value_name_g, value_name_type_inference_g

value_name_g = ( \val_type value_name -> 
  value_map_get value_name >>= \map_val_type ->
  type_check_value_name_g value_name val_type map_val_type
  ) :: ValType -> ValueName -> Stateful Haskell

value_name_type_inference_g = ( \value_name ->
  value_map_get value_name >>= \map_val_type ->
  return (map_val_type, show value_name)
  ) :: ValueName -> Stateful (ValType, Haskell)

-- Abstraction: abstraction_g, use_fields_g

abstraction_g = ( \val_type -> \case
  AbstractionName value_name -> val_name_insert_and_return val_type value_name
  UseFields -> use_fields_g val_type
  ) :: ValType -> Abstraction -> Stateful Haskell

val_name_insert_and_return = ( \val_type value_name ->
  value_map_insert value_name val_type >> return (show value_name)
  ) :: ValType -> ValueName -> Stateful Haskell

use_fields_g = ( \val_type -> case val_type of
  NamedType tn -> type_map_get tn "use_fields_g" >>= \case
    FieldAndValTypeList fields -> 
      value_map_insert (VN "tuple") val_type >>
      mapM field_and_val_type_g fields >>= \val_names ->
      return $ "tuple@(" ++ show tn ++ "C" ++ concatMap (" " ++) val_names ++ ")"
    _ -> undefined
  _ -> undefined 
  ) :: ValType -> Stateful Haskell

field_and_val_type_g = ( \(FVT field_name field_type) ->
  val_name_insert_and_return field_type field_name
  ) :: FieldAndValType -> Stateful Haskell

-- ManyAbstractions: many_abstractions_g

many_abstractions_g = (
  \val_type (AbstractionsNames value_name1 value_name2 value_names) ->
  abstractions_g val_type (value_name1 : value_name2 : value_names)
  ) :: ValType -> ManyAbstractions -> Stateful (ValType, Haskell)

-- Input: input_g, abstractions_g

input_g = ( \val_type input ->
  let 
  case_result = case input of
    OneAbstraction abstraction -> abstractions_g val_type [ abstraction ] 
    ManyAbstractions many_abs -> many_abstractions_g val_type many_abs
    :: Stateful (ValType, Haskell)
  in
  case_result >>= \(val_type, hs) -> return (val_type, "\\" ++ hs ++ " -> ")
  ) :: ValType -> Input -> Stateful (ValType, Haskell)

abstractions_g = ( \val_type abstractions -> case abstractions of
  [] -> return (val_type, "")
  abs1 : other_abs -> case val_type of
    FuncType input_type output_type -> 
      abstraction_g input_type abs1 >>= \abs1_hs ->
      abstractions_g output_type other_abs >>= \(abs_type, other_abs_hs) ->
      return (abs_type, abs1_hs ++ " " ++ other_abs_hs)
    _ -> undefined
  ) :: ValType -> [ Abstraction ] -> Stateful (ValType, Haskell)
