module HaskellTypes.AfterParsing where

import Data.List (intercalate)

import Helpers ((==>))

import HaskellTypes.LowLevel (ValueName)
import HaskellTypes.LowLevelTypes (TypeName)
import HaskellTypes.Types
import HaskellTypes.Values
import HaskellTypes.TypeDefinitions

-- All: Types, Functions

-- Types:
-- Application, ApplicationTree,
-- FunctionType', ValueType',
-- Field', TupleTypeDefinition', OrTypeCase'
-- OrTypeDefinition', TypeDefinition', FieldsOrCases'

data Application =
  ApplicationTrees ApplicationTree ApplicationTree
  deriving Show

data ApplicationTree = 
  Application Application | BaseValueLeaf BaseValue
  deriving Show

data FunctionType' = 
  InAndOutType ValueType' ValueType'
  deriving Eq

instance Show FunctionType' where
  show = \(InAndOutType in_t out_t) -> (case in_t of
    FunctionType' _ -> "(" ++ show in_t ++ ")"
    _ -> show in_t) ++ " -> " ++ show out_t

data ValueType' =
  FunctionType' FunctionType' | TypeName' TypeName | ProductType' [ ValueType' ]
  deriving Eq

instance Show ValueType' where
  show = \case
    FunctionType' func_type -> show func_type
    TypeName' type_name -> show type_name
    ProductType' types -> "(" ++ map show types==>intercalate ", " ++ ")"

data Field' =
  FNameAndType { get_name :: ValueName, get_type :: ValueType' }
  deriving Show

data TupleTypeDefinition' =
  TTNameAndFields TypeName [ Field' ]
  deriving Show

data OrTypeCase' =
  NameAndMaybeType ValueName (Maybe ValueType')
  deriving Show

data OrTypeDefinition' =
  ValNameAndCases TypeName [ OrTypeCase' ]
  deriving Show

data TypeDefinition' =
  TupleTypeDefinition' TupleTypeDefinition' | OrTypeDefinition' OrTypeDefinition'
  deriving Show

data FieldsOrCases' =
  FieldList [ Field' ] | OrTypeCaseList [ OrTypeCase' ]
  deriving Show

-- Functions: ApplicationTree, ValueType', Conversions

-- ApplicationTree: FunctionApplicationChain, MathApplication

-- FunctionApplicationChain:
-- func_app_chain_to_app_tree, func_app_chain_to_app_tree_help,
-- combine_with_reverse_direction

func_app_chain_to_app_tree = ( \(ValuesAndDirections bv_ad bv_ads bv_last) ->
  func_app_chain_to_app_tree_help bv_last (reverse $ bv_ad : bv_ads)
  ) :: FunctionApplicationChain -> ApplicationTree 

func_app_chain_to_app_tree_help = ( \prev_bv -> \case
  [] -> BaseValueLeaf prev_bv
  (bv, ad) : bv_ads -> combine_with_reverse_direction
    (BaseValueLeaf prev_bv) ad (func_app_chain_to_app_tree_help bv bv_ads)
  ) :: BaseValue -> [ (BaseValue, ApplicationDirection) ] -> ApplicationTree 

combine_with_reverse_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application $ ApplicationTrees at2 at1
  RightApplication -> Application $ ApplicationTrees at1 at2
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

-- MathApplication: math_app_to_app_tree, base_vals_to_app_tree, expr_to_base_value

math_app_to_app_tree = ( \(MathApp value_name expr1 exprs) ->
  base_vals_to_app_tree $
    ValueName value_name : map expr_to_base_value (expr1 : exprs) 
  ) :: MathApplication -> ApplicationTree 

base_vals_to_app_tree = ( \case
  [] -> error "empty list in base_vals_to_app_tree"
  [ bv ] -> BaseValueLeaf bv
  bv : bvs ->
    Application $ ApplicationTrees (BaseValueLeaf bv) $ base_vals_to_app_tree bvs
  ) :: [ BaseValue ] -> ApplicationTree

expr_to_base_value = ( \expr -> case expr of
  OperatorExpression
    (EqualityFactor
      (SubtractionFactor
        (MultiplicationFactor
          (BaseValue bv)))) -> bv
  _ -> Parenthesis $ InnerExpression $ expr
  ) :: InputOpExprOrOpExpr -> BaseValue

-- ValueType':
-- value_type_to_val_type, func_type_to_val_type, one_input_to_val_type,
-- multiple_inputs_to_val_type, output_type_to_val_type, cart_prod_to_val_type

value_type_to_val_type = ( \case
  FunctionType func_type -> func_type_to_val_type func_type
  ProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  TypeName name -> TypeName' name
  ) :: ValueType -> ValueType'

func_type_to_val_type = ( \(InputAndOutput input output) -> case input of 
  OneInput input -> one_input_to_val_type input output
  MultipleInputs mult_ins -> multiple_inputs_to_val_type mult_ins output
  ) :: FunctionType -> ValueType'

one_input_to_val_type = ( \input output -> 
  FunctionType' $
    InAndOutType (value_type_to_val_type input) $ output_type_to_val_type output
  ) :: ValueType -> OutputType -> ValueType'

multiple_inputs_to_val_type = ( \(InTypes in_t1 in_t2 in_ts) output -> 
  let
  output_type = case in_ts of 
    [] -> one_input_to_val_type in_t2 output
    in_t3 : rest_of_in_ts ->
      multiple_inputs_to_val_type (InTypes in_t2 in_t3 rest_of_in_ts) output
  in
  FunctionType' $ InAndOutType (value_type_to_val_type in_t1) output_type
  ) :: InputTypes -> OutputType -> ValueType'

output_type_to_val_type = ( \case
  OutputTypeName name -> TypeName' name
  OutputProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  ) :: OutputType -> ValueType'

cart_prod_to_val_type = ( \(Types value_type1 value_type2 other_value_types) ->
  ProductType' $
    value_type_to_val_type value_type1 : value_type_to_val_type value_type2 :
    map value_type_to_val_type other_value_types
  ) :: ProductType -> ValueType'

-- Conversions: 
-- field_conversion, tuple_t_def_conv, or_t_case_conv,
-- or_t_def_conv, type_def_conversion, fields_or_cases_conversion

field_conversion = ( \(NameAndType value_name value_type) ->
  FNameAndType value_name (value_type_to_val_type value_type)
  ) :: Field -> Field'

tuple_t_def_conv = ( \(NameAndFields type_name fields) ->
  TTNameAndFields type_name (map field_conversion fields)
  ) :: TupleTypeDefinition -> TupleTypeDefinition'

or_t_case_conv = ( \(OrTypeCase value_name maybe_value_type) ->
  NameAndMaybeType value_name (value_type_to_val_type <$> maybe_value_type)
  ) :: OrTypeCase -> OrTypeCase'

or_t_def_conv = ( \(NameAndCases type_name case1 case2 cases) ->
  ValNameAndCases type_name (map or_t_case_conv $ case1 : case2 : cases)
  ) :: OrTypeDefinition -> OrTypeDefinition'

type_def_conversion = \case
  TupleTypeDefinition tt_def -> TupleTypeDefinition' $ tuple_t_def_conv tt_def
  OrTypeDefinition or_type_def -> OrTypeDefinition' $ or_t_def_conv or_type_def
  :: TypeDefinition -> TypeDefinition'
