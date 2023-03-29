module AfterParsing.Conversions where

import Data.List (intercalate)

import Helpers ((==>))

import ParsingTypes.LowLevelValues (ValueName)
import ParsingTypes.LowLevelTypes (TypeApplication(..))
import ParsingTypes.Types
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

import AfterParsing.Types

-- All: ApplicationTree, ValueType', Other

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
-- value_type_conversion, func_type_to_val_type, one_input_to_val_type,
-- multiple_inputs_to_val_type, output_type_to_val_type, cart_prod_to_val_type

value_type_conversion = ( \case
  FunctionType func_type -> func_type_to_val_type func_type
  ProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  TypeApplication (NameAndTypeInputs name _ _) -> TypeName' name
  ) :: ValueType -> ValueType'

func_type_to_val_type = ( \(InputAndOutput input output) -> case input of 
  OneInput input -> one_input_to_val_type input output
  MultipleInputs mult_ins -> multiple_inputs_to_val_type mult_ins output
  ) :: FunctionType -> ValueType'

one_input_to_val_type = ( \input output -> 
  FunctionType' $
    InAndOutType (value_type_conversion input) $ output_type_to_val_type output
  ) :: ValueType -> OutputType -> ValueType'

multiple_inputs_to_val_type = ( \(InTypes in_t1 in_t2 in_ts) output -> 
  let
  output_type = case in_ts of 
    [] -> one_input_to_val_type in_t2 output
    in_t3 : rest_of_in_ts ->
      multiple_inputs_to_val_type (InTypes in_t2 in_t3 rest_of_in_ts) output
  in
  FunctionType' $ InAndOutType (value_type_conversion in_t1) output_type
  ) :: InputTypes -> OutputType -> ValueType'

output_type_to_val_type = ( \case
  OutputTypeApplication (NameAndTypeInputs name _ _) -> TypeName' name
  OutputProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  ) :: OutputType -> ValueType'

cart_prod_to_val_type = ( \(Types value_type1 value_type2 other_value_types) ->
  ProductType' $
    value_type_conversion value_type1 : value_type_conversion value_type2 :
    map value_type_conversion other_value_types
  ) :: ProductType -> ValueType'

-- Other: field_conversion, or_type_case_conversion,

field_conversion = ( \(NameAndType value_name value_type) ->
  FNameAndType value_name (value_type_conversion value_type)
  ) :: Field -> Field'

or_type_case_conversion = ( \(OrTypeCase value_name maybe_value_type) ->
  NameAndMaybeType value_name (value_type_conversion <$> maybe_value_type)
  ) :: OrTypeCase -> OrTypeCase'
