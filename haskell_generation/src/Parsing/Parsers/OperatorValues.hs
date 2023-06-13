module Parsing.Parsers.OperatorValues where

import Helpers 
import Text.Parsec 
import Text.Parsec.String (Parser)

import Parsing.Types.LowLevel
import Parsing.Types.OperatorValues

import Parsing.Parsers.LowLevel 

-- All:
-- ParenExpr, MathApp, BaseValue,
-- ApplicationDirection, FuncAppChain, MultExpr, PlusOrMinus, AddSubExpr,
-- EqualityExpr, InputOpExpr, OpExpr

-- ParenExpr: paren_expr_p

paren_expr_p = 
  char '(' >> operator_expression_p >>= \op_expr1 ->
  many (string ", " >> operator_expression_p) >>= \op_exprs ->
  char ')' >> return (ParenExprs op_expr1 op_exprs)
  :: Parser ParenExpr

-- MathApp: math_app_p

math_app_p = (
  add_pos_p value_name_p >>= \value_name ->
  optionMaybe (add_pos_p paren_expr_p) >>= \paren_expr ->
  return $ NameAndParenExpr2 value_name paren_expr
  ) :: Parser MathApp

-- BaseValue: base_value_p

base_value_p =
  ParenExpr <$> add_pos_p paren_expr_p <|>
  Literal <$> add_pos_p literal_p <|>
  MathApp <$> math_app_p
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
  add_pos_p func_app_chain_p >>= \func_app_chain1 ->
  many (try (string " * ") >> add_pos_p func_app_chain_p) >>= \func_app_chains ->
  return $ Factors func_app_chain1 func_app_chains
  ) :: Parser MultExpr

-- PlusOrMinus: plus_or_minus_p

plus_or_minus_p =
  try (string " + ") *> return Plus <|> try (string " - ") *> return Minus
  :: Parser PlusOrMinus

-- AddSubExpr: add_sub_expr_p, add_sub_op_term_pair_p

add_sub_expr_p =
  add_pos_p mult_expr_p >>= \term1 ->
  many op_mult_expr_pair_p >>= \op_term_pairs ->
  return $ FirstAndOpTermPairs term1 op_term_pairs
  :: Parser AddSubExpr

op_mult_expr_pair_p =
  plus_or_minus_p >>= \op ->
  add_pos_p mult_expr_p >>= \term ->
  return (op, term)
  :: Parser (PlusOrMinus, Pos MultExpr)

-- EqualityExpr: equality_expr_p

equality_expr_p = ( 
  add_pos_p add_sub_expr_p >>=
    \add_sub_expr ->
  optionMaybe (try (string " = ") >> add_pos_p add_sub_expr_p) >>=
    \maybe_add_sub_expr ->
  return $ EqExpr add_sub_expr maybe_add_sub_expr
  ) :: Parser EqualityExpr

-- InputOpExpr: input_op_expr_p

input_op_expr_p =
  input_p >>= \input -> add_pos_p equality_expr_p >>= \equality_expr ->
  return $ InputEqExpr input equality_expr
  :: Parser InputOpExpr

-- OpExpr: operator_expression_p

operator_expression_p =
  InputOpExpr <$> try input_op_expr_p <|>
  EqualityExpr <$> add_pos_p equality_expr_p
  :: Parser OpExpr
