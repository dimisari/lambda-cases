{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import Data.List
  ( intercalate )

import Helpers
  ( (==>) )

import HaskellTypes.LowLevel
  ( ValueName )
import HaskellTypes.Types
  ( TypeName, CartesianProduct(..), Output(..), MultipleInputs(..), Input(..)
  , FunctionType(..), ValueType(..)
  , FieldAndType(..), TupleTypeDef(..), CaseAndMaybeType(..), OrTypeDef(..)
  , TypeDef(..), FieldsOrCases(..) )
import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), FunctionApplicationChain(..)
  , MathApplication(..), AbsOpOrOpExpression(..), OperatorExpression(..)
  , EqualityFactor(..), SubtractionFactor(..), MultiplicationFactor(..)
  , ParenthesisValue(..) )

data ApplicationTree = 
  Application ApplicationTree ApplicationTree | BaseValueLeaf BaseValue
  deriving Show

-- fac_to_app_tree

fac_to_app_tree = ( \(ValuesAndDirections bv_ad bv_ads bv_last) ->
  fac_to_app_tree_help bv_last (reverse $ bv_ad : bv_ads)
  ) :: FunctionApplicationChain -> ApplicationTree 

fac_to_app_tree_help = ( \prev_bv -> \case
  [] -> BaseValueLeaf prev_bv
  (bv, ad) : bv_ads -> combine_with_reverse_direction
    (BaseValueLeaf prev_bv) ad (fac_to_app_tree_help bv bv_ads)
  ) :: BaseValue -> [ (BaseValue, ApplicationDirection) ] -> ApplicationTree 

combine_with_reverse_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at2 at1
  RightApplication -> Application at1 at2
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

-- math_app_to_app_tree

math_app_to_app_tree = ( \(MathApp value_name expr1 exprs) ->
  bvs_to_app_tree $ ValueName value_name : map expr_to_base_value (expr1 : exprs) 
  ) :: MathApplication -> ApplicationTree 

expr_to_base_value = ( \expr -> case expr of
  OperatorExpression (EquF (SFEF (MFSF (BaseValueMF bv)))) -> bv
  _ -> ParenthesisValue $ Parenthesis $ expr
  ) :: AbsOpOrOpExpression -> BaseValue

bvs_to_app_tree = ( \case
  [] -> error "empty list in bvs_to_app_tree"
  [ bv ] -> BaseValueLeaf bv
  bv : bvs -> Application (BaseValueLeaf bv) $ bvs_to_app_tree bvs
  ) :: [ BaseValue ] -> ApplicationTree

data ValType =
  FuncType ValType ValType | NamedType TypeName |
  TupleValType ValType ValType [ ValType ]
  deriving Eq

instance Show ValType where
  show = \case
    FuncType in_vt out_vt -> (case in_vt of
      FuncType _ _ ->
        "(" ++ show in_vt ++ ")"
      _ -> show in_vt) ++ " -> " ++ show out_vt
    NamedType tn -> show tn
    TupleValType vt1 vt2 vts ->
      "(" ++ map show (vt1 : vt2 : vts) ==> intercalate ", " ++ ")"

value_type_to_val_type = ( \case
  FunctionType func_type -> func_type_to_val_type func_type
  CartesianProduct cartesian_product -> cp_to_val_type cartesian_product
  TypeName name -> NamedType name
  ) :: ValueType -> ValType

func_type_to_val_type = ( \(InputAndOutput input output) -> case input of 
  OneInput input -> one_input_to_val_type input output
  MultipleInputs mult_ins -> multiple_inputs_to_val_type mult_ins output
  ) :: FunctionType -> ValType

one_input_to_val_type = ( \input output -> 
  let
  input_type = value_type_to_val_type input
    :: ValType
  output_type = nocp_to_val_type output
    :: ValType
  in
  FuncType input_type output_type
  ) :: ValueType -> Output -> ValType

multiple_inputs_to_val_type = (
  \(InputTypes in_t1 in_t2 in_ts) output -> 
  let
  input_type = value_type_to_val_type in_t1
    :: ValType
  output_type = case in_ts of 
    [] -> one_input_to_val_type in_t2 output
    in_t3 : rest_of_in_ts ->
      multiple_inputs_to_val_type (InputTypes in_t2 in_t3 rest_of_in_ts) output
  in
  FuncType input_type output_type
  ) :: MultipleInputs -> Output -> ValType

nocp_to_val_type = ( \case
  OutputTypeName name -> NamedType name
  OutputCartesianProduct cartesian_product -> cp_to_val_type cartesian_product
  ) :: Output -> ValType

cp_to_val_type = ( \(Types value_type1 value_type2 other_value_types) ->
  TupleValType
    (value_type_to_val_type value_type1)
    (value_type_to_val_type value_type2)
    (map value_type_to_val_type other_value_types)
  ) :: CartesianProduct -> ValType

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

-- Commented out:

-- data TupleType =
--   TT ValType ValType [ ValType ]
--   deriving (Eq, Show)

-- data ValType =
--   FuncType ValType TupleType | NamedType TypeName
--   deriving (Eq, Show)
