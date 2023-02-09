{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.LowLevel
  ( ValueName )
import HaskellTypes.Types
  ( TypeName, BaseType(..), ValueType(..), FieldAndType(..), TupleTypeDef(..)
  , CaseAndMaybeType(..), OrTypeDef(..), TypeDef(..), FieldsOrCases(..) )
import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )

data ApplicationTree = 
  Application ApplicationTree ApplicationTree | BaseValueLeaf BaseValue
  deriving Show

to_application_tree = ( \(OAA bv_ad bv_ads bv_last) ->
  to_application_tree_help bv_last (reverse $ bv_ad : bv_ads )
  ) :: OneArgApplications -> ApplicationTree 

to_application_tree_help = ( \prev_bv -> \case
  [] -> BaseValueLeaf prev_bv
  ( bv, ad ) : bv_ads -> combine_with_reverse_direction
    (BaseValueLeaf prev_bv) ad (to_application_tree_help bv bv_ads)
  ) :: BaseValue -> [ ( BaseValue, ApplicationDirection ) ] -> ApplicationTree 

combine_with_reverse_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at2 at1
  RightApplication -> Application at1 at2
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

data ValType =
  FunctionType ValType ValType | NamedType TypeName |
  TupleValType ValType ValType [ ValType ]
  deriving (Eq, Show)

base_type_to_val_type = ( \case
  TypeName tn -> NamedType tn
  ParenType vt -> value_type_to_val_type vt
  TupleType vt1 vt2 vts -> TupleValType t1 t2 ts where
    t1 : t2 : ts = map value_type_to_val_type (vt1 : vt2 : vts)
      :: [ ValType ]
  ) :: BaseType -> ValType

value_type_to_val_type = ( \(AbsTypesAndResType bts bt) -> case bts of
  bt1:other_bts -> FunctionType t1 t2 where
    t1 = base_type_to_val_type bt1
      :: ValType
    t2 = value_type_to_val_type $ AbsTypesAndResType other_bts bt
      :: ValType
  [] -> base_type_to_val_type bt
  ) :: ValueType -> ValType

data FieldAndValType =
  FVT { get_f_name :: ValueName, get_f_valtype :: ValType }
  deriving Show

ft_to_fvt = ( \(FT vn vt) -> FVT vn (value_type_to_val_type vt) )
  :: FieldAndType -> FieldAndValType

data TupleValTypeDef =
  NameAndFields TypeName [ FieldAndValType ]
  deriving Show

ttd_to_tvtd = ( \(NameAndValue tn fts) -> NameAndFields tn (map ft_to_fvt fts) )
  :: TupleTypeDef -> TupleValTypeDef

data CaseAndMaybeValType =
  CMVT ValueName (Maybe ValType)
  deriving Show

camt_to_camvt = ( \(CMT vn mvt) -> CMVT vn (value_type_to_val_type <$> mvt) )
  :: CaseAndMaybeType -> CaseAndMaybeValType

data ValOrTypeDef =
  ValNameAndValues TypeName [ CaseAndMaybeValType ]
  deriving Show

otd_to_votd = ( \(NameAndValues tn camts) ->
  ValNameAndValues tn (map camt_to_camvt camts)
  ) :: OrTypeDef -> ValOrTypeDef

data ValTypeDef =
  TupleValTypeDef TupleValTypeDef | ValOrTypeDef ValOrTypeDef
  deriving Show

td_to_vtd = \case
  TupleTypeDef ttd -> TupleValTypeDef $ ttd_to_tvtd ttd
  OrTypeDef otd -> ValOrTypeDef $ otd_to_votd otd
  :: TypeDef -> ValTypeDef

data ValFieldsOrCases =
  FieldAndValTypeList [ FieldAndValType ] |
  CaseAndMaybeValTypeList [ CaseAndMaybeValType ]
  deriving Show

foc_to_vfoc = undefined
  :: FieldsOrCases -> ValFieldsOrCases
