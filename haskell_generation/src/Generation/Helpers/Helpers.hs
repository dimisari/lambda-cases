module Generation.Helpers.Helpers where

import Data.List (intercalate)
import Control.Monad ((>=>), zipWithM)
import Text.Parsec (SourcePos)
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers ((.>), Haskell, Pos(..))

import Parsing.Types.LowLevel (ValueName(..))
import Parsing.Types.Types (TypeName)

import Intermediate.Types.Types 
import Intermediate.Types.TypeDefinitions (TypeInfo(..), TTField(..))

import Generation.Helpers.TypeChecking
import Generation.Helpers.ErrorMessages

import Generation.State.TypesAndOperations


-- GenerateInserted

class GenerateInserted a where
  gen_inserted :: a -> Stateful (Haskell, [ ValueName ])

data ValNameType =
  ValNameType ValueName ValType

instance GenerateInserted ValNameType where
  gen_inserted = gen_1inserted .> fmap ( \(hs, vn) -> (hs, [ vn ]) )

data TTNameAndFields = 
  TTNameAndFields TypeName [ TTField ]

instance GenerateInserted TTNameAndFields where
  gen_inserted = \(TTNameAndFields type_name fields) ->
    unzip <$> mapM gen_1inserted fields >>= \(fields_hs, inserted) ->
    return
      ("@(C" ++ show type_name ++ concatMap (" " ++) fields_hs ++ ")", inserted)

instance GenerateInserted ProdType where
  gen_inserted = \(ProdTypes types) ->
    let
    prod_val_name_types = zipWith ValNameType prod_t_field_ns types
      :: [ ValNameType ]
    in
    unzip <$> mapM gen_1inserted prod_val_name_types >>= \(fields_hs, inserted) ->
    return ("@(" ++ intercalate ", " fields_hs ++ ")", inserted)

data UseFieldsTypeName = UseFieldsTypeName TypeName

instance GenerateInserted UseFieldsTypeName where
  gen_inserted = \(UseFieldsTypeName type_name) ->
    type_map_get type_name >>= \case
      TupleType _ fields -> gen_inserted $ TTNameAndFields type_name fields
      _ -> throwE $ use_fields_err $ tn_to_val_t type_name

data ValueTypeName = ValueTypeName TypeName

instance GenerateInserted ValueTypeName where
  gen_inserted = \(ValueTypeName type_name) ->
    type_map_get type_name >>= \case
      TupleType _ fields -> gen_inserted $ TTNameAndFields type_name fields
      _ -> return ("", [])

-- Generate1Inserted

class Generate1Inserted a where
  gen_1inserted :: a -> Stateful (Haskell, ValueName)

instance Generate1Inserted ValNameType where
  gen_1inserted = \(ValNameType vn vt) ->
    value_map_insert vn vt >> return (show vn, vn)

instance Generate1Inserted TTField where
  gen_1inserted = \(FNameAndType fn ft) -> gen_1inserted (ValNameType fn ft)

-- GenerateHs

class GenerateHs a where
  gen_hs :: a -> Stateful Haskell

instance GenerateHs ValNameType where
  gen_hs = \(ValNameType vn vt) -> return $ show vn

instance GenerateHs TTField where
  gen_hs = \(FNameAndType fn ft) -> gen_hs (ValNameType fn ft)

-- Inserted1

class Inserted1 a where
  inserted1 :: a -> Stateful ValueName

instance Inserted1 ValNameType where
  inserted1 = \(ValNameType vn vt) -> value_map_insert vn vt >> return vn

instance Inserted1 TTField where
  inserted1 = \(FNameAndType fn ft) -> inserted1 (ValNameType fn ft)

-- 

prod_t_field_ns = map VN [ "first", "second", "third", "fourth", "fifth" ]
  :: [ ValueName ]

--  value in cases

maybe_value_g = ( \vn vt -> 
  value_map_get vn >>= \val_name_t ->
  equiv_types val_name_t vt >>= \case
    True -> return ("", [])
    False -> has_value_g val_name_t
  ) :: ValueName -> ValType -> Stateful (Haskell, [ ValueName ])

has_value_g = ( \case 
  FuncType (InAndOutTs in_t _) ->
    value_map_insert (VN "value") in_t >>
    value_matching_in_t_g in_t >>= \(value_matching_hs, inserted) ->
    return $ (" value" ++ value_matching_hs, VN "value" : inserted)
  _ -> error
    "case_with_value_vn_g: val_name_t not a FuncType, should be impossible"
  ) :: ValType -> Stateful (Haskell, [ ValueName ])

value_matching_in_t_g = ( \case
  TypeApp (ConsAndTIns type_name []) -> gen_inserted (ValueTypeName type_name)
  ProdType prod_type -> gen_inserted prod_type
  _ -> return ("", [])
  ) :: ValType -> Stateful (Haskell, [ ValueName ])

-- error position

add_pos_to_err = ( \pos err -> case err of
  (False, err_t, err_msg) ->
    throwE $ (True, err_t, "\n" ++ show pos ++ "\n\n" ++ err_msg)
  _ -> throwE $ err
  ) :: SourcePos -> Error -> Stateful a

catchEaddPos = ( \g pos -> catchE g (add_pos_to_err pos) )
  :: Stateful a -> SourcePos -> Stateful a

