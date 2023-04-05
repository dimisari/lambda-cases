module CodeGenerators.OperatorValues where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel (ValueName(..), Abstraction(..))
import ParsingTypes.Types (TypeName(..), ValueType(..))
import ParsingTypes.OperatorValues
import ParsingTypes.Values

import AfterParsing.Types 
import AfterParsing.Conversions 

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (types_are_equivalent)

import CodeGenerators.LowLevelValues

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- FunctionApplicationChain,
-- MultiplicationFactor, Multiplication, SubtractionTerm, Subtraction
-- EqualityTerm, Equality
-- PureOperatorExpression, InputOperatorExpression, OperatorExpression

-- Parenthesis: parenthesis_g, paren_type_inference_g

parenthesis_g = ( \(InnerExpression expr) expr_t ->
  input_op_expr_or_op_expr_g expr expr_t >>= \expr_hs ->
  return $ "(" ++ expr_hs ++ ")"
  ) :: Parenthesis -> ValueType' -> Stateful Haskell

paren_type_inference_g = ( \(InnerExpression expr) ->
  input_op_expr_or_op_expr_type_inf_g expr >>= \(expr_hs, expr_t) ->
  return ("(" ++ expr_hs ++ ")", expr_t)
  ) :: Parenthesis -> Stateful (Haskell, ValueType')

-- Tuple:
-- tuple_g, tuple_values_type_name_g, tuple_values_types_g,
-- tuple_values_field_types_g, tuple_type_inference_g

tuple_g = ( \(TupleExpressions val1 val2 vals) -> \case
  FunctionType' _ -> throwE "Tuple can't have function type"
  TypeApplication' (TypeConstructorAndInputs' type_name _) ->
    tuple_values_type_name_g (val1 : val2 : vals) type_name
  ProductType' types -> tuple_values_types_g (val1 : val2 : vals) types
  ) :: Tuple -> ValueType' -> Stateful Haskell

tuple_values_type_name_g = ( \values type_name -> type_map_get type_name >>= \case
  OrTypeCaseList _ -> throwE "Tuple can't be an or_type"
  FieldList fields -> tuple_values_fields_g values fields type_name
  ) :: [ OperatorExpression ] -> TypeName -> Stateful Haskell

tuple_values_fields_g = ( \values fields type_name ->
  case length values == length fields of 
    False -> throwE values_fields_lengths_dont_match_err
    True -> tuple_values_field_types_g values (map get_type fields) type_name
  ) :: [ OperatorExpression ] -> [ Field' ] -> TypeName -> Stateful Haskell

tuple_values_field_types_g = ( \values types type_name ->
  get_indent_level >>= \indent_level ->
  zipWith input_op_expr_or_op_expr_g values types==>sequence
    >>= concatMap ( \value_hs ->  " (" ++ value_hs ++ ")" ) .> \values_hs -> 
  return $ "\n" ++ indent (indent_level + 1) ++ "C" ++ show type_name ++ values_hs
  ) :: [ OperatorExpression ] -> [ ValueType' ] -> TypeName -> Stateful Haskell

tuple_values_types_g = ( \values types -> case length types == length values of
  False -> throwE "Length of tuple does not match length of product type"
  True -> 
    zipWith input_op_expr_or_op_expr_g values types==>sequence >>= \vals_hs ->
    return $ "(" ++ intercalate ", " vals_hs ++ ")"
  ) :: [ OperatorExpression ] -> [ ValueType' ] -> Stateful Haskell

tuple_type_inference_g = ( \(TupleExpressions val1 val2 values) ->
  mapM input_op_expr_or_op_expr_type_inf_g (val1 : val2 : values) >>= unzip .>
    \(vals_hs, types) -> 
  return ("(" ++ intercalate ", " vals_hs ++ ")", ProductType' types)
  ) :: Tuple -> Stateful (Haskell, ValueType')

-- MathApplication: math_application_g, math_app_type_inference_g

math_application_g =
  math_app_to_app_tree .> application_tree_g
  :: MathApplication -> ValueType' -> Stateful Haskell

math_app_type_inference_g =  
  math_app_to_app_tree .> app_tree_type_inference_g
  :: MathApplication -> Stateful (Haskell, ValueType')

-- BaseValue: base_value_g, base_value_type_inference_g

base_value_g = ( \case
  Parenthesis parenthesis -> parenthesis_g parenthesis
  Tuple tuple -> tuple_g tuple
  Literal literal -> literal_g literal
  ValueName value_name -> value_name_g value_name
  MathApplication math_application -> math_application_g math_application
  ) :: BaseValue -> ValueType' -> Stateful Haskell

base_value_type_inference_g = ( \case
  Parenthesis parenthesis -> paren_type_inference_g parenthesis
  Tuple tuple -> tuple_type_inference_g tuple
  Literal literal -> literal_type_inference_g literal
  ValueName value_name -> value_name_type_inference_g value_name
  MathApplication math_application -> math_app_type_inference_g math_application
  ) :: BaseValue -> Stateful (Haskell, ValueType')

-- FunctionApplicationChain:
-- func_app_chain_g, application_tree_g, app_tree_type_inference_g

func_app_chain_g = 
  func_app_chain_to_app_tree .> application_tree_g
  :: FunctionApplicationChain -> ValueType' -> Stateful Haskell

application_tree_g = ( \case 
  BaseValueLeaf base_value -> base_value_g base_value
  Application application -> application_g application
  ) :: ApplicationTree -> ValueType' -> Stateful Haskell

application_g = ( \application val_type ->
  application_type_inference_g application >>= \(application_hs, application_t) ->
  types_are_equivalent val_type application_t >>= \case 
    True -> return application_hs
    False -> throwE "Cannot match types"
  ) :: Application -> ValueType' -> Stateful Haskell

app_tree_type_inference_g = ( \case 
  Application application -> application_type_inference_g application
  BaseValueLeaf base_value -> base_value_type_inference_g base_value
  ) :: ApplicationTree -> Stateful (Haskell, ValueType')

application_type_inference_g = ( \application@(ApplicationTrees tree1 tree2) ->
  app_tree_type_inference_g tree1 >>= \(tree1_hs, tree1_t) ->
  case tree1_t of 
    FunctionType' func_type ->
      catchE
        (tree1_func_type_g tree1_hs func_type tree2)
        (application_handler application)
    _ -> throwE "Cannot apply something that is not a function"
  ) :: Application -> Stateful (Haskell, ValueType')

tree1_func_type_g = (
  \tree1_hs func_type@(InputAndOutputType' input_t output_t) tree2 -> 
  application_tree_g tree2 input_t >>= \tree2_hs ->
  let 
  tree2_hs' = case tree2 of 
    Application _ -> "(" ++ tree2_hs ++ ")"
    _ -> tree2_hs
    :: Haskell
  in
  return (tree1_hs ++ " " ++ tree2_hs', output_t) 
  ) :: Haskell -> FunctionType' -> ApplicationTree ->
       Stateful (Haskell, ValueType')

application_handler = ( \(ApplicationTrees tree1 tree2) error_msg -> case tree2 of 
  BaseValueLeaf (Tuple (TupleExpressions val1 val2 vals)) -> 
    let
    tree1' = 
      Application $ ApplicationTrees
        tree1 $
        BaseValueLeaf $ Parenthesis $ InnerExpression val1
    tree2' = BaseValueLeaf $ case vals of 
      [] -> Parenthesis $ InnerExpression val2
      val3 : other_vals -> Tuple $ TupleExpressions val2 val3 other_vals
    in
    application_type_inference_g $ ApplicationTrees tree1' tree2'
  BaseValueLeaf base_value -> 
    let
    tree1' = 
      Application $ ApplicationTrees
        tree1 $
        Application $ ApplicationTrees
          (BaseValueLeaf $ ValueName $ VN "get_1st")
          $ BaseValueLeaf base_value
    tree2' = 
      Application $ ApplicationTrees
        (BaseValueLeaf $ ValueName $ VN "get_all_but_1st")
        $ BaseValueLeaf base_value
    in
    application_type_inference_g $ ApplicationTrees tree1' tree2'
  _ -> error $ show tree1 ++ "\n" ++ show tree2 ++ "\n" ++ error_msg
  ) :: Application -> Error -> Stateful (Haskell, ValueType')

-- MultiplicationFactor: multiplication_factor_g

multiplication_factor_g = ( \case
  FunctionApplicationChain func_app_chain -> func_app_chain_g func_app_chain 
  BaseValue base_value -> base_value_g base_value 
  ) :: MultiplicationFactor -> ValueType' -> Stateful Haskell

-- Multiplication: multiplication_g

multiplication_g = ( \(MultiplicationFactors mul_fac1 mul_fac2 mul_facs) val_type -> 
  mapM (flip multiplication_factor_g val_type) (mul_fac1 : mul_fac2 : mul_facs) >>=
  intercalate " * " .> return
  ) :: Multiplication -> ValueType' -> Stateful Haskell

-- SubtractionTerm: subtraction_factor_g

subtraction_factor_g = ( \case
  Multiplication multiplication -> multiplication_g multiplication
  MultiplicationFactor mul_fac -> multiplication_factor_g mul_fac
  ) :: SubtractionTerm -> ValueType' -> Stateful Haskell

-- Subtraction: subtraction_g

subtraction_g = ( \(SubtractionTerms subtraction_factor1 subtraction_factor2) val_type ->
  subtraction_factor_g subtraction_factor1 val_type >>= \subtraction_factor1_hs ->
  subtraction_factor_g subtraction_factor2 val_type >>= \subtraction_factor2_hs ->
  return $ subtraction_factor1_hs ++ " - " ++ subtraction_factor2_hs
  ) :: Subtraction -> ValueType' -> Stateful Haskell

-- EqualityTerm: equality_factor_g

equality_factor_g = ( \case
  Subtraction subtraction -> subtraction_g subtraction
  SubtractionTerm subtraction_factor -> subtraction_factor_g subtraction_factor
  ) :: EqualityTerm -> ValueType' -> Stateful Haskell

-- Equality: equality_g

equality_g = ( \(EqualityTerms equality_factor1 equality_factor2) -> \case 
  TypeApplication' (TypeConstructorAndInputs' (TN "Bool") []) ->
    let int_t = TypeApplication' $ TypeConstructorAndInputs' (TN "Int") [] in
    equality_factor_g equality_factor1 int_t >>= \equality_factor1_hs ->
    equality_factor_g equality_factor2 int_t >>= \equality_factor2_hs ->
    return $ equality_factor1_hs ++ " == " ++ equality_factor2_hs
  _ -> undefined
  ) :: Equality -> ValueType' -> Stateful Haskell

-- PureOperatorExpression: operator_expression_g, op_expr_type_inference_g

operator_expression_g = ( \case
  EqualityTerm equality_factor -> equality_factor_g equality_factor
  Equality equality -> equality_g equality
  ) :: PureOperatorExpression -> ValueType' -> Stateful Haskell

op_expr_type_inference_g = ( \operator_expr ->
  let
  pair = case operator_expr of
    Equality equality -> (equality_g equality val_type, val_type) where
      val_type = TypeApplication' $ TypeConstructorAndInputs' (TN "Bool") []
    EqualityTerm equ_fac -> (equality_factor_g equ_fac val_type, val_type) where
      val_type = TypeApplication' $ TypeConstructorAndInputs' (TN "Int") []
    :: (Stateful Haskell, ValueType')
  in
  pair ==> \(g, t) -> g >>= \hs -> return (hs, t)
  ) :: PureOperatorExpression -> Stateful (Haskell, ValueType')

-- InputOperatorExpression: input_op_expression_g, input_op_expr_type_inference_g

input_op_expression_g = ( \(InputAndPureOperatorExpression input operator_expr) ->
  input_g input >=> \(output_type, input_hs) ->
  operator_expression_g operator_expr output_type >>= \op_expr_hs ->
  return $ input_hs ++ op_expr_hs
  ) :: InputOperatorExpression -> ValueType' -> Stateful Haskell

abs_op_expr_type_inference_g = ( \(InputAndPureOperatorExpression as opval) ->
  undefined
  ) :: InputOperatorExpression -> Stateful (Haskell, ValueType')

-- OperatorExpression: input_op_expression_g, input_op_expr_type_inference_g

input_op_expr_or_op_expr_g = ( \case
  InputOperatorExpression input_op_expr -> input_op_expression_g input_op_expr
  PureOperatorExpression operator_expr -> operator_expression_g operator_expr
  ) :: OperatorExpression -> ValueType' -> Stateful Haskell

input_op_expr_or_op_expr_type_inf_g = ( \case
  InputOperatorExpression input_op_expr -> abs_op_expr_type_inference_g input_op_expr
  PureOperatorExpression operator_expr -> op_expr_type_inference_g operator_expr
  ) :: OperatorExpression -> Stateful (Haskell, ValueType')

