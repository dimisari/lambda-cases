module Generation.Final.LowLevel where

import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Control.Monad ((>=>), zipWithM)
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers 

import ParsingTypes.LowLevel
import ParsingTypes.Types (TypeName(..))

import IntermediateTypes.Values (Input2(..))
import IntermediateTypes.Types
import IntermediateTypes.TypeDefinitions (TTField(..), TypeInfo(..))

import Conversions.Values (input_to_input2)

import Generation.State.TypesAndOperations

import Generation.Helpers.ErrorMessages
import Generation.Helpers.TypeChecking (equiv_types)
import Generation.Helpers.Helpers 

-- Classes: Generate, GenerateInfer, InTGenInserted

-- Generate: Pos a, ValueName, Literal

class Generate a where
  generate :: a -> ValType -> Stateful Haskell

instance Generate a => Generate (Pos a) where
  generate = \(WithPos pos a) val_type -> 
    catchEaddPos (generate a val_type) pos

instance Generate ValueName where
  generate = \val_name val_type -> 
    value_map_get val_name >>= \map_val_type ->
    equiv_types val_type map_val_type >>= \case
      False -> throwE $ type_check_err (show val_name) val_type map_val_type
      True -> if_or_t_case_add_c val_name

instance Generate Literal where
  generate = \lit val_type -> (val_type == int) ==> \case
    True -> return $ show lit
    False -> throwE $ lit_not_int_err val_type

-- GenerateInfer: Pos a, ValueName, Literal

class GenerateInfer a where
  generate_infer :: a -> Stateful (Haskell, ValType)

instance GenerateInfer a => GenerateInfer (Pos a) where
  generate_infer = \(WithPos pos a) -> 
    catchEaddPos (generate_infer a) pos

instance GenerateInfer ValueName where
  generate_infer = \val_name -> 
    value_map_get val_name >>= \map_val_type ->
    if_or_t_case_add_c val_name >>= \value_name_hs ->
    return (value_name_hs, map_val_type)

instance GenerateInfer Literal where
  generate_infer = \lit -> return (show lit, int)

-- InTGenInserted

class InTGenInserted a where
  type_gen_inserted :: a -> ValType -> Stateful (Haskell, [ ValueName ])

instance InTGenInserted a => InTGenInserted (Pos a) where
  type_gen_inserted = \(WithPos pos a) t ->
    catchEaddPos (type_gen_inserted a t) pos

instance InTGenInserted Abstraction where
  type_gen_inserted = \case
    AbstractionName val_name ->
      \t -> gen_inserted (ValNameType (remove_pos val_name) t)
    UseFields pos ->
      type_gen_inserted (WithPos pos UseFields_)

data UseFields_ = UseFields_

instance InTGenInserted UseFields_ where
  type_gen_inserted = \UseFields_ val_type -> 
    value_map_insert (VN "tuple") val_type >>
    type_gen_inserted UseFieldsMatching val_type >>= \(matching_hs, inserted) ->
    return ("tuple" ++ matching_hs, VN "tuple" : inserted)

data UseFieldsMatching = UseFieldsMatching

instance InTGenInserted UseFieldsMatching where
  type_gen_inserted = \UseFieldsMatching -> \case
    TypeApp (ConsAndTIns type_name _) -> gen_inserted (UseFieldsTypeName type_name)
    ProdType prod_type -> gen_inserted prod_type
    val_t -> throwE $ use_fields_err val_t

-- ValueName: if_or_t_case_add_c

if_or_t_case_add_c = ( \val_name -> in_or_t_cs val_name >>= \case
  True -> return $ "C" ++ show val_name
  _ -> return $ show val_name
  ) :: ValueName -> Stateful Haskell

-- Input

input_g = input_to_input2 .> input2_g
  :: Input -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

input2_g = ( \(Input2 abstractions) val_type ->
  abstractions_g abstractions val_type >>= \(input_hs, final_t, inserted) ->
  return ("\\" ++ input_hs ++ "-> ", final_t, inserted)
  ) :: Input2 -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

abstractions_g = ( \abstractions val_type -> case abstractions of
  [] -> return ("", val_type, [])
  abs1 : other_abs ->
    abs_check_func_t abs1 val_type >>= \func_type ->
    abstractions_func_t_g abs1 other_abs func_type
  ) :: [ Pos Abstraction ] -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

abs_check_func_t = ( \(WithPos pos abs) -> \case
  FuncType func_t -> return func_t
  val_type -> add_pos_to_err pos $ not_func_t_err abs val_type
  ) :: Pos Abstraction -> ValType -> Stateful FuncType

abstractions_func_t_g = ( \abs1 other_abs (InAndOutTs in_t out_t) -> 
  type_gen_inserted abs1 in_t >>= \(abs1_hs, inserted1) ->
  abstractions_g other_abs out_t >>= \(other_abs_hs, final_t, inserted) ->
  return (abs1_hs ++ " " ++ other_abs_hs, final_t, inserted1 ++ inserted)
  )
  ::
  Pos Abstraction -> [ Pos Abstraction ] -> FuncType ->
  Stateful (Haskell, ValType, [ ValueName ])
