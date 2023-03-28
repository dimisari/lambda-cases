module CodeGenerators.Values where

import Data.List (intercalate, splitAt)
import Control.Monad (foldM)
import Control.Monad.State ((>=>))
import Control.Monad.Trans.Except (throwE, catchE)

import Helpers (Haskell, Error, (==>), (.>), indent)

import HaskellTypes.LowLevel (ValueName(..), Abstraction(..))
import HaskellTypes.LowLevelTypes (TypeName(..))
import HaskellTypes.Types (ValueType(..))
import HaskellTypes.Values
import HaskellTypes.AfterParsing
import HaskellTypes.Generation

import CodeGenerators.ErrorMessages
import CodeGenerators.LowLevel
import CodeGenerators.TypeChecking (types_are_equivalent)

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- FunctionApplicationChain,
-- MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction
-- EqualityFactor, Equality
-- OperatorExpression, InputOpExpression, InputOpExprOrOpExpr
-- SpecificCase, Cases,
-- NameTypeAndValue, NameTypeAndValueLists, NTAVOrNTAVLists, NamesTypesAndValues
-- Where, CasesOrWhere, ValueExpression

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

tuple_g = ( \(Values val1 val2 vals) -> \case
  FunctionType' _ -> throwE "Tuple can't have function type"
  TypeName' type_name -> tuple_values_type_name_g (val1 : val2 : vals) type_name
  ProductType' types -> tuple_values_types_g (val1 : val2 : vals) types
  ) :: Tuple -> ValueType' -> Stateful Haskell

tuple_values_type_name_g = ( \values type_name -> type_map_get type_name >>= \case
  OrTypeCaseList _ -> throwE "Tuple can't be an or_type"
  FieldList fields -> tuple_values_fields_g values fields type_name
  ) :: [ InputOpExprOrOpExpr ] -> TypeName -> Stateful Haskell

tuple_values_fields_g = ( \values fields type_name ->
  case length values == length fields of 
    False -> throwE values_fields_lengths_dont_match_err
    True -> tuple_values_field_types_g values (map get_type fields) type_name
  ) :: [ InputOpExprOrOpExpr ] -> [ Field' ] -> TypeName -> Stateful Haskell

tuple_values_field_types_g = ( \values types type_name ->
  get_indent_level >>= \indent_level ->
  zipWith input_op_expr_or_op_expr_g values types==>sequence
    >>= concatMap ( \value_hs ->  " (" ++ value_hs ++ ")" ) .> \values_hs -> 
  return $ "\n" ++ indent (indent_level + 1) ++ show type_name ++ "C" ++ values_hs
  ) :: [ InputOpExprOrOpExpr ] -> [ ValueType' ] -> TypeName -> Stateful Haskell

tuple_values_types_g = ( \values types -> case length types == length values of
  False -> throwE "Length of tuple does not match length of product type"
  True -> 
    zipWith input_op_expr_or_op_expr_g values types==>sequence >>= \vals_hs ->
    return $ "(" ++ intercalate ", " vals_hs ++ ")"
  ) :: [ InputOpExprOrOpExpr ] -> [ ValueType' ] -> Stateful Haskell

tuple_type_inference_g = ( \(Values val1 val2 values) ->
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

application_type_inference_g = ( \(ApplicationTrees tree1 tree2) ->
  app_tree_type_inference_g tree1 >>= \(tree1_hs, tree1_t) ->
  case tree1_t of 
    FunctionType' func_type ->
      catchE
        (tree1_func_type_g tree1_hs func_type tree2)
        (application_handler tree1 tree2)
    _ -> throwE "Cannot apply something that is not a function"
  ) :: Application -> Stateful (Haskell, ValueType')

tree1_func_type_g = ( \tree1_hs (InAndOutType input_t output_t) tree2 -> 
  application_tree_g tree2 input_t >>= \tree2_hs ->
  let 
  tree2_hs' = case tree2 of 
    Application _ -> "(" ++ tree2_hs ++ ")"
    _ -> tree2_hs
    :: Haskell
  in
  return (tree1_hs ++ " " ++ tree2_hs', output_t) 
  ) :: Haskell -> FunctionType' -> ApplicationTree -> Stateful (Haskell, ValueType')

application_handler = ( \tree1 -> \case
  BaseValueLeaf (Tuple (Values val1 val2 vals)) -> \_ ->
    let
    tree1' = 
      Application $ ApplicationTrees
        tree1 $
        BaseValueLeaf $ Parenthesis $ InnerExpression val1
    tree2' = BaseValueLeaf $ case vals of 
      [] -> Parenthesis $ InnerExpression val2
      val3 : other_vals -> Tuple $ Values val2 val3 other_vals
    in
    application_type_inference_g $ ApplicationTrees tree1' tree2'
  _ -> undefined
  ) :: ApplicationTree -> ApplicationTree -> Error -> Stateful (Haskell, ValueType')

-- MultiplicationFactor: multiplication_factor_g

multiplication_factor_g = ( \case
  FuncAppChain func_app_chain -> func_app_chain_g func_app_chain 
  BaseValue base_value -> base_value_g base_value 
  ) :: MultiplicationFactor -> ValueType' -> Stateful Haskell

-- Multiplication: multiplication_g

multiplication_g = ( \(MulFactors mul_fac1 mul_fac2 mul_facs) val_type -> 
  mapM (flip multiplication_factor_g val_type) (mul_fac1 : mul_fac2 : mul_facs) >>=
  intercalate " * " .> return
  ) :: Multiplication -> ValueType' -> Stateful Haskell

-- SubtractionFactor: subtraction_factor_g

subtraction_factor_g = ( \case
  Multiplication multiplication -> multiplication_g multiplication
  MultiplicationFactor mul_fac -> multiplication_factor_g mul_fac
  ) :: SubtractionFactor -> ValueType' -> Stateful Haskell

-- Subtraction: subtraction_g

subtraction_g = ( \(SubFactors subtraction_factor1 subtraction_factor2) val_type ->
  subtraction_factor_g subtraction_factor1 val_type >>= \subtraction_factor1_hs ->
  subtraction_factor_g subtraction_factor2 val_type >>= \subtraction_factor2_hs ->
  return $ subtraction_factor1_hs ++ " - " ++ subtraction_factor2_hs
  ) :: Subtraction -> ValueType' -> Stateful Haskell

-- EqualityFactor: equality_factor_g

equality_factor_g = ( \case
  Subtraction subtraction -> subtraction_g subtraction
  SubtractionFactor subtraction_factor -> subtraction_factor_g subtraction_factor
  ) :: EqualityFactor -> ValueType' -> Stateful Haskell

-- Equality: equality_g

equality_g = ( \(EqualityFactors equality_factor1 equality_factor2) -> \case 
  (TypeName' (TN "Bool")) ->
    let int_t = (TypeName' (TN "Int")) in
    equality_factor_g equality_factor1 int_t >>= \equality_factor1_hs ->
    equality_factor_g equality_factor2 int_t >>= \equality_factor2_hs ->
    return $ equality_factor1_hs ++ " == " ++ equality_factor2_hs
  _ -> undefined
  ) :: Equality -> ValueType' -> Stateful Haskell

-- OperatorExpression: operator_expression_g, op_expr_type_inference_g

operator_expression_g = ( \case
  EqualityFactor equality_factor -> equality_factor_g equality_factor
  Equality equality -> equality_g equality
  ) :: OperatorExpression -> ValueType' -> Stateful Haskell

op_expr_type_inference_g = ( \operator_expr ->
  let
  pair = case operator_expr of
    Equality equality -> (equality_g equality val_type, val_type) where
      val_type = TypeName' $ TN "Bool"
    EqualityFactor equ_fac -> (equality_factor_g equ_fac val_type, val_type) where
      val_type = TypeName' $ TN "Int"
    :: (Stateful Haskell, ValueType')
  in
  pair ==> \(g, t) -> g >>= \hs -> return (hs, t)
  ) :: OperatorExpression -> Stateful (Haskell, ValueType')

-- InputOpExpression: input_op_expression_g, input_op_expr_type_inference_g

input_op_expression_g = ( \(InputAndOpResult input operator_expr) ->
  input_g input >=> \(output_type, input_hs) ->
  operator_expression_g operator_expr output_type >>= \op_expr_hs ->
  return $ input_hs ++ op_expr_hs
  ) :: InputOpExpression -> ValueType' -> Stateful Haskell

abs_op_expr_type_inference_g = ( \(InputAndOpResult as opval) ->
  undefined
  ) :: InputOpExpression -> Stateful (Haskell, ValueType')

-- InputOpExprOrOpExpr: input_op_expression_g, input_op_expr_type_inference_g

input_op_expr_or_op_expr_g = ( \case
  InputOpExpression input_op_expr -> input_op_expression_g input_op_expr
  OperatorExpression operator_expr -> operator_expression_g operator_expr
  ) :: InputOpExprOrOpExpr -> ValueType' -> Stateful Haskell

input_op_expr_or_op_expr_type_inf_g = ( \case
  InputOpExpression input_op_expr -> abs_op_expr_type_inference_g input_op_expr
  OperatorExpression operator_expr -> op_expr_type_inference_g operator_expr
  ) :: InputOpExprOrOpExpr -> Stateful (Haskell, ValueType')

-- LiteralOrValueName: literal_or_value_name_g, lit_or_val_name_type_inference_g

literal_or_value_name_g = ( \case
  Lit literal -> literal_g literal
  ValName value_name -> val_name_insert_and_return value_name
  ) :: LiteralOrValueName -> ValueType' -> Stateful Haskell

lit_or_val_name_type_inference_g = ( 
  undefined
  ) :: LiteralOrValueName -> Stateful (Haskell, ValueType')

-- SpecificCase: specific_case_g, abs_g, specific_case_type_inference_g

specific_case_g = ( \(SpecificCase lit_or_val_name value_expression) -> \case
  FunctionType' (InAndOutType input_t output_t) ->
    get_indent_level >>= \ind_lev ->
    literal_or_value_name_g lit_or_val_name input_t >>= \lit_or_val_name_hs ->
    value_expression_g value_expression output_t >>= \value_expr_hs ->
    return $ indent ind_lev ++ abs_g lit_or_val_name_hs ++ " -> " ++ value_expr_hs
  _ -> undefined
  ) :: SpecificCase -> ValueType' -> Stateful Haskell

abs_g = \case 
  "true" -> "True"
  "false" -> "False"
  g -> g
  :: String -> Haskell

specific_case_type_inference_g = ( \(SpecificCase lit_or_val_name val_expr) ->
  get_indent_level >>= \ind_lev ->
  lit_or_val_name_type_inference_g lit_or_val_name >>= \(lovn_g, lovn_t) ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, v_t) ->
  return
    ( indent ind_lev ++ lovn_g ++ " -> " ++ val_expr_hs
    , FunctionType' $ InAndOutType lovn_t v_t )
  ) :: SpecificCase -> Stateful (Haskell, ValueType')

-- DefaultCase: specific_case_g, specific_case_type_inference_g

default_case_g = ( \(DefaultCase value_expression) -> \case
  FunctionType' (InAndOutType input_t output_t) -> 
    get_indent_level >>= \ind_lev ->
    value_map_insert (VN "value") input_t >>
    value_expression_g value_expression output_t >>= \value_expression_hs ->
    return $ indent ind_lev ++ "value -> " ++ value_expression_hs
  _ -> undefined
  ) :: DefaultCase -> ValueType' -> Stateful Haskell

-- Cases: cases_g, cases_type_inference_g

cases_g = ( \cases val_type -> case cases of
  OneAndDefault specific_case default_case ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    specific_case_g specific_case val_type >>= \specific_case_hs ->
    default_case_g default_case val_type >>= \default_case_hs ->
    update_indent_level indent_level >>
    return ("\\case\n" ++ specific_case_hs ++ "\n" ++ default_case_hs)

  Many spec_case1 spec_case2 spec_cases mdc ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    mapM (flip specific_case_g val_type) specific_cases >>= \spec_cases_hs ->
    update_indent_level indent_level >>
    return ("\\case\n" ++ intercalate "\n" spec_cases_hs) where
    specific_cases = spec_case1 : spec_case2 : spec_cases
      :: [ SpecificCase ]
  ) :: Cases -> ValueType' -> Stateful Haskell

cases_type_inference_g = ( \case
  OneAndDefault specific_case default_case -> undefined
  Many spec_c1 spec_c2 spec_cs mdc ->
    get_indent_level >>= \indent_level ->
    update_indent_level (indent_level + 1) >>
    specific_case_type_inference_g spec_c1 >>= \(spec_c1_hs, val_type) ->
    mapM (flip specific_case_g val_type) (spec_c2 : spec_cs) >>= \spec_cs_hs ->
    update_indent_level indent_level >>
    return
      ("\\case\n" ++ spec_c1_hs ++ "\n" ++ intercalate "\n" spec_cs_hs, val_type)
  ) :: Cases -> Stateful (Haskell, ValueType')

-- NameTypeAndValue: name_type_and_value_g

name_type_and_value_g = ( \(NTAV value_name value_type value_expr) -> 
  get_indent_level >>= \ind_lev ->
  let 
  val_type = value_type_to_val_type value_type
    :: ValueType'
  in
  value_expression_g value_expr val_type >>= \val_expr_hs ->
  return $
    "\n" ++ indent ind_lev ++ show value_name ++ " :: " ++ show val_type ++
    "\n" ++ indent ind_lev ++ show value_name ++ " = " ++ val_expr_hs ++ "\n"
  ) :: NameTypeAndValue -> Stateful Haskell

-- NameTypeAndValueLists: ntav_lists_to_list_of_ntavs

ntav_lists_to_list_of_ntavs = ( \ntav_lists -> case ntav_lists of
  NTAVLists (val_name : val_names) (val_type : val_types) (val_expr : val_exprs) ->
    NTAV val_name val_type val_expr :
    ntav_lists_to_list_of_ntavs (NTAVLists val_names val_types val_exprs)
  NTAVLists [] [] [] -> []
  _ -> error $ name_type_and_value_lists_err ntav_lists
  ) :: NameTypeAndValueLists -> [ NameTypeAndValue ]

-- NTAVOrNTAVLists: ntav_or_ntav_lists_to_list_of_ntavs

ntav_or_ntav_lists_to_list_of_ntavs = ( \case 
  NameTypeAndValue name_type_and_value -> [ name_type_and_value ]
  NameTypeAndValueLists ntav_lists -> ntav_lists_to_list_of_ntavs ntav_lists
  ) :: NTAVOrNTAVLists -> [ NameTypeAndValue ]

-- NamesTypesAndValues:
-- names_types_and_values_g, ns_ts_and_vs_to_list_of_ntavs, list_of_ntavs_g,
-- ntav_map_insert

names_types_and_values_g =
  ns_ts_and_vs_to_list_of_ntavs .> list_of_ntavs_g
  :: NamesTypesAndValues -> Stateful Haskell

ns_ts_and_vs_to_list_of_ntavs = ( \(NTAVs ntavs) ->
  concatMap ntav_or_ntav_lists_to_list_of_ntavs ntavs
  ) :: NamesTypesAndValues -> [ NameTypeAndValue ]

list_of_ntavs_g = ( \list_of_ntavs ->
  mapM_ ntav_map_insert list_of_ntavs >>
  list_of_ntavs==>mapM name_type_and_value_g >>= concat .> return
  ) :: [ NameTypeAndValue ] -> Stateful Haskell

ntav_map_insert = ( \(NTAV value_name value_type value_expr) ->
  value_map_insert value_name $ value_type_to_val_type value_type
  ) :: NameTypeAndValue -> Stateful ()

-- Where: where_g, where_type_inference_g 

where_g = ( \(ValueWhereNTAVs val_expr ntavs) val_type ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_hs ->
  value_expression_g val_expr val_type >>= \val_expr_hs ->
  update_indent_level ind_lev >>
  return ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs)
  ) :: Where -> ValueType' -> Stateful Haskell

where_type_inference_g = ( \(ValueWhereNTAVs val_expr ntavs) ->
  get_indent_level >>= \ind_lev -> update_indent_level (ind_lev + 1) >>
  names_types_and_values_g ntavs >>= \ntavs_hs ->
  value_expression_type_inference_g val_expr >>= \(val_expr_hs, val_type) ->
  update_indent_level ind_lev >>
  return
    ("\n" ++ indent (ind_lev + 1) ++ val_expr_hs ++ " where" ++ ntavs_hs, val_type)
  ) :: Where -> Stateful (Haskell, ValueType')

-- CasesOrWhere: cases_or_where_g, cases_or_where_type_inference_g

cases_or_where_g = ( \case
  Cases cases -> cases_g cases
  Where where_ -> where_g where_
  ) :: CasesOrWhere -> ValueType' -> Stateful Haskell

cases_or_where_type_inference_g = ( \case
  Cases cases -> cases_type_inference_g cases
  Where where_ -> where_type_inference_g where_
  ) :: CasesOrWhere -> Stateful (Haskell, ValueType')

-- InputCasesOrWhere: abstraction_cases_or_where_g

abstraction_cases_or_where_g = ( \(InputAndCOWResult input cases_or_where) ->
  input_g input >=> \(output_type, input_hs) ->
  cases_or_where_g cases_or_where output_type >>= \cases_or_where_hs ->
  return $ input_hs ++ cases_or_where_hs
  ) :: InputCasesOrWhere -> ValueType' -> Stateful Haskell

abstraction_cow_type_inference_g = ( 
  undefined
  ) :: InputCasesOrWhere -> Stateful (Haskell, ValueType')

-- ValueExpression: value_expression_g, value_expression_type_inference_g

value_expression_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cases_or_where_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_g cases_or_where 
  InputOpExprOrOpExpr expr -> input_op_expr_or_op_expr_g expr
  ) :: ValueExpression -> ValueType' -> Stateful Haskell

value_expression_type_inference_g = ( \case
  InputCasesOrWhere input_cow -> abstraction_cow_type_inference_g input_cow
  CasesOrWhere cases_or_where -> cases_or_where_type_inference_g cases_or_where
  InputOpExprOrOpExpr expr -> input_op_expr_or_op_expr_type_inf_g expr
  ) :: ValueExpression -> Stateful (Haskell, ValueType')
