module Parsers.OperatorValues where

import Text.Parsec 
import Text.Parsec.String (Parser)
import ParsingTypes.OperatorValues
import Parsers.LowLevel (literal_p, value_name_p, input_p)

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue
-- ApplicationDirection, FuncAppChain, MultiplicationFactor, Multiplication,
-- AddSubTerm, PlusOrMinus, AddSubExpr, Equality, PureOpExpr, InputOpExpr,
-- OperatorExpression

-- Parenthesis: parenthesis_p

parenthesis_p =
  char '(' *> (InnerExpression <$> operator_expression_p) <* char ')'
  :: Parser Parenthesis

-- Tuple: tuple_p

tuple_p =
  char '(' >> operator_expression_p >>= \op_expr1 ->
  string ", " >> operator_expression_p >>= \op_expr2 ->
  many (string ", " >> operator_expression_p) >>= \op_exprs ->
  char ')' >> return (TupleExpressions op_expr1 op_expr2 op_exprs)
  :: Parser Tuple

-- MathApplication: math_application_p

math_application_p =  
  value_name_p >>= \value_name ->
  char '(' >> operator_expression_p >>= \op_expr ->
  many (string ", " >> operator_expression_p) >>= \op_exprs ->
  char ')' >> return (NameAndInputExprs value_name op_expr op_exprs)
  :: Parser MathApplication

-- BaseValue: base_value_p

base_value_p =
  Parenthesis <$> try parenthesis_p <|>
  Tuple <$> tuple_p <|>
  MathApplication <$> try math_application_p <|>
  Literal <$> literal_p <|>
  ValueName <$> value_name_p
  :: Parser BaseValue

-- ApplicationDirection: application_direction_p

application_direction_p = 
  string "<==" *> return LeftApplication <|>
  string "==>" *> return RightApplication
  :: Parser ApplicationDirection

-- FuncAppChain: func_app_chain_p, base_val_app_dir_p

func_app_chain_p =
  base_val_app_dir_p >>= \base_val_app_dir ->
  many (try base_val_app_dir_p) >>= \base_val_app_dirs ->
  base_value_p >>= \base_value ->
  return $ ValuesAndDirections base_val_app_dir base_val_app_dirs base_value
  :: Parser FuncAppChain

base_val_app_dir_p = 
  base_value_p >>= \base_value ->
  application_direction_p >>= \application_direction ->
  return (base_value, application_direction)
  :: Parser (BaseValue, ApplicationDirection)

-- MultiplicationFactor: multiplication_factor_p

multiplication_factor_p =
  FuncAppChain <$> try func_app_chain_p <|> BaseValue <$> base_value_p
  :: Parser MultiplicationFactor

-- Multiplication: multiplication_p

multiplication_p =
  multiplication_factor_p >>= \mul_factor1 ->
  string " * " >> multiplication_factor_p >>= \mul_factor2 ->
  many (try (string " * ") >> multiplication_factor_p) >>= \mul_factors ->
  return $ Factors mul_factor1 mul_factor2 mul_factors
  :: Parser Multiplication

-- AddSubTerm: add_sub_term_p

add_sub_term_p =
  Mult <$> try multiplication_p <|>
  MultFactor <$> multiplication_factor_p
  :: Parser AddSubTerm

-- PlusOrMinus: plus_or_minus_p

plus_or_minus_p =
  try (string " + ") *> return Plus <|> try (string " - ") *> return Minus
  :: Parser PlusOrMinus

-- AddSubExpr: add_sub_expr_p, add_sub_op_term_pair_p

add_sub_expr_p =
  add_sub_term_p >>= \term1 ->
  many add_sub_op_term_pair_p >>= \op_term_pairs ->
  return $ FirstAndOpTermPairs term1 op_term_pairs
  :: Parser AddSubExpr

add_sub_op_term_pair_p =
  plus_or_minus_p >>= \op -> add_sub_term_p >>= \term -> return (op, term)
  :: Parser (PlusOrMinus, AddSubTerm)

-- Equality: equality_p

equality_p =
  add_sub_expr_p >>= \add_sub_expr1 ->
  string " = " >> add_sub_expr_p >>= \add_sub_expr2 ->
  return $ EqualityTerms add_sub_expr1 add_sub_expr2
  :: Parser Equality

-- PureOpExpr: pure_op_expr_p

pure_op_expr_p =
  Equality <$> try equality_p <|> AddSubExpr <$> add_sub_expr_p
  :: Parser PureOpExpr

-- InputOpExpr: input_op_expr_p

input_op_expr_p =
  input_p >>= \input -> pure_op_expr_p >>= \pure_op_expr ->
  return $ InputAndPureOpExpr input pure_op_expr
  :: Parser InputOpExpr

-- OperatorExpression: operator_expression_p

operator_expression_p =
  InputOpExpr <$> try input_op_expr_p <|> PureOpExpr <$> pure_op_expr_p
  :: Parser OperatorExpression

