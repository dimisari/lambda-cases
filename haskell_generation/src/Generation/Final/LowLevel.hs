module Generation.Final.LowLevel where

import Text.Parsec (SourcePos)
import Data.List (intercalate)
import Control.Monad ((>=>), zipWithM)
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers 

import Parsing.Types.LowLevel
import Parsing.Types.Types (TypeName(..))

import Intermediate.Types.Values (Input2(..))
import Intermediate.Types.Types
import Intermediate.Types.TypeDefinitions (TTField(..), TypeInfo(..))

import Intermediate.Conversions.Values (input_to_input2)

import Generation.State.TypesAndOperations

import Generation.Helpers.ErrorMessages
import Generation.Helpers.TypeChecking (check_equiv_types, equiv_types)
import Generation.Helpers.Helpers 

-- Classes: Generate, GenerateInfer, InTGenInserted

-- Generate: Pos a, ValueName, Literal

class Generate a where
  generate :: a -> ValType -> Stateful Haskell

instance Generate a => Generate (Pos a) where
  generate = \(WithPos pos a) vt -> 
    catchEaddPos (generate a vt) pos

instance Generate ValueName where
  generate = \vn vt -> 
    value_map_get vn >>= \map_vt ->
    check_equiv_types vt map_vt (show vn) >> if_or_t_case_add_c vn

instance Generate Literal where
  generate = \lit vt -> (vt == int) ==> \case
    True -> return $ show lit
    False -> throwE $ lit_not_int_err vt

-- GenerateInfer: Pos a, ValueName, Literal

class GenerateInfer a where
  generate_infer :: a -> Stateful (Haskell, ValType)

instance GenerateInfer a => GenerateInfer (Pos a) where
  generate_infer = \(WithPos pos a) -> 
    catchEaddPos (generate_infer a) pos

instance GenerateInfer ValueName where
  generate_infer = \vn -> 
    if_or_t_case_add_c vn !+! value_map_get vn

(!+!) :: Stateful a -> Stateful b -> Stateful (a, b)
sa !+! sb = sa >>= \a -> sb >>= \b -> return (a, b)

instance GenerateInfer Literal where
  generate_infer = show .> ( \a -> (a, int) ) .> return

-- InTGenInserted

class InTGenInserted a where
  type_gen_inserted :: a -> ValType -> Stateful (Haskell, [ ValueName ])

instance InTGenInserted a => InTGenInserted (Pos a) where
  type_gen_inserted = \(WithPos pos a) t ->
    catchEaddPos (type_gen_inserted a t) pos

instance InTGenInserted Abstraction where
  type_gen_inserted = \case
    AbstractionName vn -> \t -> gen_inserted (ValNameType vn t)
    UseFields -> type_gen_inserted UseFields_

data UseFields_ = UseFields_

instance InTGenInserted UseFields_ where
  type_gen_inserted = \UseFields_ vt -> 
    value_map_insert (VN "tuple") vt >>
    type_gen_inserted UseFieldsMatching vt >>= \(matching_hs, inserted) ->
    return ("tuple" ++ matching_hs, VN "tuple" : inserted)

data UseFieldsMatching = UseFieldsMatching

instance InTGenInserted UseFieldsMatching where
  type_gen_inserted = \UseFieldsMatching -> \case
    TypeApp (ConsAndTIns type_name _) -> gen_inserted (UseFieldsTypeName type_name)
    ProdType prod_type -> gen_inserted prod_type
    val_t -> throwE $ use_fields_err val_t

-- ValueName: if_or_t_case_add_c

if_or_t_case_add_c = ( \vn -> in_or_t_cs vn >>= \case
  True -> return $ "C" ++ show vn
  _ -> return $ show vn
  ) :: ValueName -> Stateful Haskell

-- Input

input_g = input_to_input2 .> input2_g
  :: Input -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

input2_g = ( \(Input2 abstractions) vt ->
  abstractions_g abstractions vt >>= \(input_hs, final_t, inserted) ->
  return ("\\" ++ input_hs ++ "-> ", final_t, inserted)
  ) :: Input2 -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

abstractions_g = ( \abstractions vt -> case abstractions of
  [] -> return ("", vt, [])
  abs1 : other_abs ->
    abs_check_func_t abs1 vt >>= \func_type ->
    abstractions_func_t_g abs1 other_abs func_type
  ) :: [ Pos Abstraction ] -> ValType -> Stateful (Haskell, ValType, [ ValueName ])

abs_check_func_t = ( \(WithPos pos abs) -> \case
  FuncType func_t -> return func_t
  vt -> add_pos_to_err pos $ not_func_t_err abs vt
  ) :: Pos Abstraction -> ValType -> Stateful FuncType

abstractions_func_t_g = ( \abs1 other_abs (InAndOutTs in_t out_t) -> 
  type_gen_inserted abs1 in_t >>= \(abs1_hs, inserted1) ->
  abstractions_g other_abs out_t >>= \(other_abs_hs, final_t, inserted) ->
  return (abs1_hs ++ " " ++ other_abs_hs, final_t, inserted1 ++ inserted)
  )
  ::
  Pos Abstraction -> [ Pos Abstraction ] -> FuncType ->
  Stateful (Haskell, ValType, [ ValueName ])
