module AfterParsing.Conversions where

import Data.List (intercalate)

import Helpers ((==>))

import ParsingTypes.LowLevel (ValueName)
import ParsingTypes.Types
import ParsingTypes.OperatorValues
import ParsingTypes.Values
import ParsingTypes.TypeDefinitions

import AfterParsing.Types

-- All: ApplicationTree, ValueType', Type Definitions

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

math_app_to_app_tree = ( \(NameAndInputExpressions value_name expr1 exprs) ->
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
  PureOperatorExpression
    (EqualityTerm
      (SubtractionTerm
        (MultiplicationFactor
          (BaseValue bv)))) -> bv
  _ -> Parenthesis $ InnerExpression $ expr
  ) :: OperatorExpression -> BaseValue

-- ValueType':
-- value_type_conversion, func_type_to_val_type, one_input_to_val_type,
-- multiple_inputs_to_val_type, output_type_to_val_type, cart_prod_to_val_type

value_type_conversion = ( \case
  FunctionType func_type -> func_type_to_val_type func_type
  ProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  TypeApplication type_application ->
    TypeApplication' $ type_application_conversion type_application
  ) :: ValueType -> ValueType'

func_type_to_val_type = ( \(InputAndOutputTypes input output_t) -> case input of 
  OneInputType input_t -> one_input_to_val_type input_t output_t
  MultipleInputTypes mult_ins -> multiple_inputs_to_val_type mult_ins output_t
  ) :: FunctionType -> ValueType'

one_input_to_val_type = ( \input_t output_t -> 
  FunctionType' $
    InputAndOutputType'
      (value_type_conversion input_t) (output_type_to_val_type output_t)
  ) :: ValueType -> OutputType -> ValueType'

multiple_inputs_to_val_type = ( \(TypesInParenthesis in_t1 in_t2 in_ts) output_t -> 
  let
  output_type = case in_ts of 
    [] -> one_input_to_val_type in_t2 output_t
    in_t3 : rest_of_in_ts ->
      multiple_inputs_to_val_type
        (TypesInParenthesis in_t2 in_t3 rest_of_in_ts) output_t
  in
  FunctionType' $ InputAndOutputType' (value_type_conversion in_t1) output_type
  ) :: ManyTypesInParenthesis -> OutputType -> ValueType'

output_type_to_val_type = ( \case
  OutputTypeApplication type_application ->
    TypeApplication' $ type_application_conversion type_application
  OutputProductType cartesian_product -> cart_prod_to_val_type cartesian_product
  ) :: OutputType -> ValueType'

cart_prod_to_val_type = (
  \(ProductTypes value_type1 value_type2 other_value_types) ->
  ProductType' $
    value_type_conversion value_type1 : value_type_conversion value_type2 :
    map value_type_conversion other_value_types
  ) :: ProductType -> ValueType'

-- TypeApplication: type_application_conversion

type_application_conversion = ( 
  \(TypeConstructorAndInputs constructor_name left_type_inputs right_type_inputs) ->
  TypeConstructorAndInputs' constructor_name $
    left_type_inputs_conversion left_type_inputs ++
    right_type_inputs_conversion right_type_inputs
  ) :: TypeApplication -> TypeApplication'

left_type_inputs_conversion = ( \case
  NoLeftTypeInputs -> []
  OneLeftTypeInput type_input -> [ value_type_conversion type_input ]
  ManyLeftTypeInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren
  ) :: LeftTypeInputs -> [ ValueType' ]

right_type_inputs_conversion = ( \case
  NoRightTypeInputs -> []
  OneRightTypeInput type_input -> [ value_type_conversion type_input ]
  ManyRightTypeInputs many_ts_in_paren -> many_ts_in_paren_conv many_ts_in_paren 
  ) :: RightTypeInputs -> [ ValueType' ]

many_ts_in_paren_conv = ( \(TypesInParenthesis t1 t2 ts) ->
  map value_type_conversion $ t1 : t2 : ts
  ) :: ManyTypesInParenthesis -> [ ValueType' ]

-- TypeConstructorAndVariables: cons_and_type_vars_conversion

cons_and_type_vars_conversion = ( 
  \(TypeConstructorAndVariables constructor_name left_type_vars right_type_vars) ->
  TypeConstructorAndVariables' constructor_name $
    flip zip [ "a", "b", "c", "d", "e" ] $
    left_type_vars_conversion left_type_vars ++
    right_type_vars_conversion right_type_vars
  ) :: TypeConstructorAndVariables -> TypeConstructorAndVariables'

left_type_vars_conversion = ( \case
  NoLeftTypeVariables -> []
  OneLeftTypeVariable type_name -> [ type_name ]
  ManyLeftTypeVariables many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: LeftTypeVariables -> [ TypeName ]

right_type_vars_conversion = ( \case
  NoRightTypeVariables -> []
  OneRightTypeVariable type_name -> [ type_name ]
  ManyRightTypeVariables many_t_names_in_paren ->
    many_t_names_in_paren_conv many_t_names_in_paren
  ) :: RightTypeVariables -> [ TypeName ]

many_t_names_in_paren_conv = ( \(ParenTypeNames n1 n2 ns) ->
  n1 : n2 : ns
  ) :: ManyTypeNamesInParenthesis -> [ TypeName ]

-- Type Definitions: 
-- field_conversion, tuple_type_def_conversion,
-- or_type_case_conversion, or_type_def_conversion

field_conversion = ( \(NameAndType value_name value_type) ->
  NameAndType' value_name (value_type_conversion value_type)
  ) :: Field -> Field'

tuple_type_def_conversion = ( \(ConstructorAndFields type_application fields) ->
  ConstructorAndFields'
    (cons_and_type_vars_conversion type_application)
    (map field_conversion fields)
  ) :: TupleTypeDefinition -> TupleTypeDefinition'

or_type_case_conversion = ( \(NameAndMaybeInputType value_name maybe_value_type) ->
  NameAndMaybeInputType' value_name (value_type_conversion <$> maybe_value_type)
  ) :: OrTypeCase -> OrTypeCase'

or_type_def_conversion = (
  \(ConstructorAndCases type_application case1 case2 cases) ->
  ConstructorAndCases'
    (cons_and_type_vars_conversion type_application)
    (map or_type_case_conversion $ case1 : case2 : cases)
  ) :: OrTypeDefinition -> OrTypeDefinition'
