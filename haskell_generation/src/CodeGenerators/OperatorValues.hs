module CodeGenerators.OperatorValues where

import Data.List (intercalate)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import ParsingTypes.LowLevel (ValueName(..))
import ParsingTypes.Types (TypeName(..))
import ParsingTypes.OperatorValues

import IntermediateTypes.Types 
import IntermediateTypes.TypeDefinitions (Field'(..), TypeInfo(..))
import IntermediateTypes.Values

import Conversions.Values

import GenerationState.TypesAndOperations

import GenerationHelpers.ErrorMessages
import GenerationHelpers.TypeChecking (types_are_equivalent)

import CodeGenerators.LowLevel

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- FuncAppChain, MultiplicationFactor, Multiplication,
-- AddSubTerm, AddSubExpr, Equality, PureOpExpr, InputOpExpr,
-- OperatorExpression

-- Parenthesis: parenthesis_g, paren_type_inference_g

parenthesis_g = ( \(InnerExpression expr) expr_t ->
  operator_expression_g expr expr_t >>= \expr_hs ->
  return $ "(" ++ expr_hs ++ ")"
  ) :: Parenthesis -> ValType -> Stateful Haskell

paren_type_inference_g = ( \(InnerExpression expr) ->
  op_expr_type_inf_g expr >>= \(expr_hs, expr_t) ->
  return ("(" ++ expr_hs ++ ")", expr_t)
  ) :: Parenthesis -> Stateful (Haskell, ValType)

-- Tuple:
-- tuple_g, tuple_values_type_name_g, tuple_values_types_g,
-- tuple_values_field_types_g, tuple_type_inference_g

tuple_g = ( \(TupleExpressions val1 val2 vals) -> \case
  FuncType _ -> throwE "Tuple can't have function type"
  TypeApp (TypeConsAndInputs' type_name _) ->
    tuple_values_type_name_g (val1 : val2 : vals) type_name
  ProdType types -> tuple_values_types_g (val1 : val2 : vals) types
  ) :: Tuple -> ValType -> Stateful Haskell

tuple_values_type_name_g = ( \values type_name -> type_map_get type_name >>= \case
  OrType _ _ -> throwE "Tuple can't be an or_type"
  TupleType _ fields -> tuple_values_fields_g values fields type_name
  _ -> throwE "Tuple can't be an or_type"
  ) :: [ OperatorExpression ] -> TypeName -> Stateful Haskell

tuple_values_fields_g = ( \values fields type_name ->
  case length values == length fields of 
    False -> throwE values_fields_lengths_dont_match_err
    True -> tuple_values_field_types_g values (map get_type fields) type_name
  ) :: [ OperatorExpression ] -> [ Field' ] -> TypeName -> Stateful Haskell

tuple_values_field_types_g = ( \values types type_name ->
  get_ind_lev >>= \ind_lev ->
  zipWith operator_expression_g values types==>sequence
    >>= concatMap ( \value_hs ->  " (" ++ value_hs ++ ")" ) .> \values_hs -> 
  return $ "\n" ++ indent (ind_lev + 1) ++ "C" ++ show type_name ++ values_hs
  ) :: [ OperatorExpression ] -> [ ValType ] -> TypeName -> Stateful Haskell

tuple_values_types_g = ( \values types -> case length types == length values of
  False -> throwE "Length of tuple does not match length of product type"
  True -> 
    zipWith operator_expression_g values types==>sequence >>= \vals_hs ->
    return $ "(" ++ intercalate ", " vals_hs ++ ")"
  ) :: [ OperatorExpression ] -> [ ValType ] -> Stateful Haskell

tuple_type_inference_g = ( \(TupleExpressions val1 val2 values) ->
  mapM op_expr_type_inf_g (val1 : val2 : values) >>= unzip .>
    \(vals_hs, types) -> 
  return ("(" ++ intercalate ", " vals_hs ++ ")", ProdType types)
  ) :: Tuple -> Stateful (Haskell, ValType)

-- MathApplication: math_application_g, math_app_type_inference_g

math_application_g =
  math_app_to_app_tree .> application_tree_g
  :: MathApplication -> ValType -> Stateful Haskell

math_app_type_inference_g =  
  math_app_to_app_tree .> app_tree_type_inference_g
  :: MathApplication -> Stateful (Haskell, ValType)

-- BaseValue: base_value_g, base_value_type_inference_g

base_value_g = ( \case
  Parenthesis parenthesis -> parenthesis_g parenthesis
  Tuple tuple -> tuple_g tuple
  Literal literal -> literal_g literal
  ValueName value_name -> value_name_g value_name
  MathApplication math_application -> math_application_g math_application
  ) :: BaseValue -> ValType -> Stateful Haskell

base_value_type_inference_g = ( \case
  Parenthesis parenthesis -> paren_type_inference_g parenthesis
  Tuple tuple -> tuple_type_inference_g tuple
  Literal literal -> literal_type_inference_g literal
  ValueName value_name -> value_name_type_inference_g value_name
  MathApplication math_application -> math_app_type_inference_g math_application
  ) :: BaseValue -> Stateful (Haskell, ValType)

-- FuncAppChain:
-- func_app_chain_g, application_tree_g, app_tree_type_inference_g

func_app_chain_g = 
  func_app_chain_to_app_tree .> application_tree_g
  :: FuncAppChain -> ValType -> Stateful Haskell

func_app_chain_type_inf_g = 
  func_app_chain_to_app_tree .> app_tree_type_inference_g
  :: FuncAppChain -> Stateful (Haskell, ValType)

application_tree_g = ( \case 
  BaseValueLeaf base_value -> base_value_g base_value
  Application application -> application_g application
  ) :: ApplicationTree -> ValType -> Stateful Haskell

application_g = ( \application val_type ->
  application_type_inference_g application >>= \(application_hs, application_t) ->
  types_are_equivalent val_type application_t >>= \case 
    True -> return application_hs
    False -> throwE "Cannot match types"
  ) :: Application -> ValType -> Stateful Haskell

app_tree_type_inference_g = ( \case 
  Application application -> application_type_inference_g application
  BaseValueLeaf base_value -> base_value_type_inference_g base_value
  ) :: ApplicationTree -> Stateful (Haskell, ValType)

application_type_inference_g = ( \application@(ApplicationTrees tree1 tree2) ->
  app_tree_type_inference_g tree1 >>= \(tree1_hs, tree1_t) ->
  case tree1_t of 
    FuncType func_type ->
      catchE
        (tree1_func_type_g tree1_hs func_type tree2)
        (application_handler application func_type)
    _ -> throwE "Cannot apply something that is not a function"
  ) :: Application -> Stateful (Haskell, ValType)

tree1_func_type_g = (
  \tree1_hs func_type@(InAndOutTs input_t output_t) tree2 -> 
  application_tree_g tree2 input_t >>= \tree2_hs ->
  let 
  tree2_hs' = case tree2 of 
    Application _ -> "(" ++ tree2_hs ++ ")"
    _ -> tree2_hs
    :: Haskell
  in
  return (tree1_hs ++ " " ++ tree2_hs', output_t) 
  ) :: Haskell -> FuncType -> ApplicationTree ->
       Stateful (Haskell, ValType)

application_handler = (
  \(ApplicationTrees tree1 tree2) (InAndOutTs input_t _) error_msg ->
  case tree2 of 
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
    BaseValueLeaf base_value -> base_value_type_inference_g base_value >>= \case
      (_,ProdType (t1:_)) -> types_are_equivalent input_t t1 >>= \case 
        True ->
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
        _ -> throwE $ show tree1 ++ "\n" ++ show tree2 ++ "\n" ++ error_msg
      _ -> throwE $ show tree1 ++ "\n" ++ show tree2 ++ "\n" ++ error_msg
    _ -> throwE $ show tree1 ++ "\n" ++ show tree2 ++ "\n" ++ error_msg
  ) :: Application -> FuncType -> Error -> Stateful (Haskell, ValType)

-- MultiplicationFactor: multiplication_factor_g

multiplication_factor_g = ( \case
  FuncAppChain func_app_chain -> func_app_chain_g func_app_chain 
  BaseValue base_value -> base_value_g base_value 
  ) :: MultiplicationFactor -> ValType -> Stateful Haskell

mult_factor_type_inf_g = ( \case
  FuncAppChain func_app_chain ->  func_app_chain_type_inf_g func_app_chain 
  BaseValue base_value -> base_value_type_inference_g base_value 
  ) :: MultiplicationFactor -> Stateful (Haskell, ValType)

-- Multiplication: multiplication_g

multiplication_g = ( \(Factors mul_f1 mul_f2 mul_fs) val_type -> 
  mapM (flip multiplication_factor_g val_type) (mul_f1 : mul_f2 : mul_fs) >>=
  intercalate " * " .> return
  ) :: Multiplication -> ValType -> Stateful Haskell

mult_type_inf_g = ( \(Factors mul_f1 mul_f2 mul_fs) -> 
  mapM (flip multiplication_factor_g int) (mul_f1 : mul_f2 : mul_fs) >>=
  intercalate " * " .> ( \hs -> return (hs, int) )
  ) :: Multiplication -> Stateful (Haskell, ValType)

-- AddSubTerm: add_sub_term_g

add_sub_term_g = ( \case
  Mult multiplication -> multiplication_g multiplication
  MultFactor mul_f -> multiplication_factor_g mul_f
  ) :: AddSubTerm -> ValType -> Stateful Haskell

add_sub_term_type_inf_g = ( \case
  Mult multiplication -> mult_type_inf_g multiplication
  MultFactor mult_fac -> mult_factor_type_inf_g mult_fac
  ) :: AddSubTerm -> Stateful (Haskell, ValType)

-- AddSubExpr:
-- add_sub_expr_g, add_sub_expr_type_inf_g, add_sub_or_term_g,
-- add_sub_expr_type_inf_g, addition_g, subtraction_g, add_sub_g

add_sub_expr_g = add_sub_expr_conv .> add_sub_or_term_g
  :: AddSubExpr -> ValType -> Stateful Haskell

add_sub_expr_type_inf_g = add_sub_expr_conv .> add_sub_or_term_type_inf_g
  :: AddSubExpr -> Stateful (Haskell, ValType)

add_sub_or_term_g = ( \case
  Addition addition -> addition_g addition
  Subtraction' subtraction -> subtraction_g subtraction
  Term term -> add_sub_term_g term
  ) :: AddSubOrTerm -> ValType -> Stateful Haskell

add_sub_or_term_type_inf_g = ( \case
  Addition addition -> addition_g addition int >>= \hs -> return (hs, int)
  Subtraction' subtr -> subtraction_g subtr int >>= \hs -> return (hs, int)
  Term term -> add_sub_term_type_inf_g term
  ) :: AddSubOrTerm -> Stateful (Haskell, ValType)

addition_g = ( \(ExprPlusTerm expr term) -> add_sub_g expr " + " term )
  :: Addition -> ValType -> Stateful Haskell

subtraction_g = ( \(ExprMinusTerm expr term) -> add_sub_g expr " - " term )
  :: Subtraction' -> ValType -> Stateful Haskell

add_sub_g = ( \expr op term val_type ->
  add_sub_or_term_g expr val_type >>= \expr_hs ->
  add_sub_term_g term val_type >>= \term_hs ->
  return $ expr_hs ++ op ++ term_hs
  ) :: AddSubOrTerm -> String -> AddSubTerm -> ValType -> Stateful Haskell

-- Equality: equality_g

equality_g = ( \(EqualityTerms term1 term2) -> \case 
  TypeApp (TypeConsAndInputs' (TN "Bool") []) ->
    add_sub_expr_g term1 int >>= \term1_hs ->
    add_sub_expr_g term2 int >>= \term2_hs ->
    return $ term1_hs ++ " == " ++ term2_hs
  _ -> undefined
  ) :: Equality -> ValType -> Stateful Haskell

equality_type_inf_g = ( \(EqualityTerms term1 term2) ->
  add_sub_expr_g term1 int >>= \term1_hs ->
  add_sub_expr_g term2 int >>= \term2_hs ->
  return (term1_hs ++ " == " ++ term2_hs, bool)
  ) :: Equality -> Stateful (Haskell, ValType)

-- PureOpExpr: pure_operator_expression_g, pure_op_expr_type_inf_g

pure_operator_expression_g = ( \case
  AddSubExpr add_sub_expr -> add_sub_expr_g add_sub_expr
  Equality equality -> equality_g equality
  ) :: PureOpExpr -> ValType -> Stateful Haskell

pure_op_expr_type_inf_g = ( \case
  AddSubExpr add_sub_expr -> add_sub_expr_type_inf_g add_sub_expr
  Equality equality -> equality_type_inf_g equality 
  ) :: PureOpExpr -> Stateful (Haskell, ValType)

-- InputOpExpr: input_operator_expression_g, input_op_expr_type_inf_g

input_operator_expression_g = (
  \(InputAndPureOpExpr input operator_expr) ->
  input_g input >=> \(output_type, input_hs) ->
  pure_operator_expression_g operator_expr output_type >>= \op_expr_hs ->
  return $ input_hs ++ op_expr_hs
  ) :: InputOpExpr -> ValType -> Stateful Haskell

input_op_expr_type_inf_g = ( \(InputAndPureOpExpr as opval) ->
  undefined
  ) :: InputOpExpr -> Stateful (Haskell, ValType)

-- OperatorExpression: operator_expression_g, input_op_expr_type_inference_g

operator_expression_g = ( \case
  InputOpExpr input_op_expr -> input_operator_expression_g input_op_expr
  PureOpExpr operator_expr -> pure_operator_expression_g operator_expr
  ) :: OperatorExpression -> ValType -> Stateful Haskell

op_expr_type_inf_g = ( \case
  InputOpExpr input_op_expr -> input_op_expr_type_inf_g input_op_expr
  PureOpExpr operator_expr -> pure_op_expr_type_inf_g operator_expr
  ) :: OperatorExpression -> Stateful (Haskell, ValType)

