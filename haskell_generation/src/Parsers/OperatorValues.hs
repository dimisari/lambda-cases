module Parsers.OperatorValues where

import Text.Parsec 
import Text.Parsec.String (Parser)
import ParsingTypes.OperatorValues
import Parsers.LowLevel (literal_p, value_name_p, input_p)

-- All:
-- Parenthesis, Tuple, MathApplication, BaseValue,
-- ApplicationDirection, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- Equality, PureOpExpr, InputOpExpr, OperatorExpression

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
  try (string "<==") *> return LeftApplication <|>
  try (string "==>") *> return RightApplication
  :: Parser ApplicationDirection

-- FuncAppChain: func_app_chain_p, base_val_app_dir_p

func_app_chain_p =
  base_value_p >>= \base_value ->
  many app_dir_base_val_p >>= \app_dir_base_vals ->
  return $ ValuesAndDirections base_value app_dir_base_vals
  :: Parser FuncAppChain

app_dir_base_val_p = 
  application_direction_p >>= \application_direction ->
  base_value_p >>= \base_value ->
  return (application_direction, base_value)
  :: Parser (ApplicationDirection, BaseValue)

-- MultExpr: mult_expr_p

mult_expr_p = (
  func_app_chain_p >>= \func_app_chain1 ->
  many (try (string " * ") >> func_app_chain_p) >>= \func_app_chains ->
  return $ Factors func_app_chain1 func_app_chains
  ) :: Parser MultExpr

-- PlusOrMinus: plus_or_minus_p

plus_or_minus_p =
  try (string " + ") *> return Plus <|> try (string " - ") *> return Minus
  :: Parser PlusOrMinus

-- AddSubExpr: add_sub_expr_p, add_sub_op_term_pair_p

add_sub_expr'_p =
  mult_expr_p >>= \term1 ->
  many op_mult_expr_pair_p >>= \op_term_pairs ->
  return $ FirstAndOpTermPairs term1 op_term_pairs
  :: Parser AddSubExpr

op_mult_expr_pair_p =
  plus_or_minus_p >>= \op -> mult_expr_p >>= \term -> return (op, term)
  :: Parser (PlusOrMinus, MultExpr)

-- Equality: equality_p

equality_p =
  add_sub_expr'_p >>= \add_sub_expr1 ->
  string " = " >> add_sub_expr'_p >>= \add_sub_expr2 ->
  return $ EqualityTerms add_sub_expr1 add_sub_expr2
  :: Parser Equality

-- PureOpExpr: pure_op_expr_p

pure_op_expr_p =
  Equality <$> try equality_p <|> AddSubExpr <$> add_sub_expr'_p
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
