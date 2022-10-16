{-# LANGUAGE LambdaCase #-}

module ValueExpressions where

import Prelude ( Eq, Show, String, show, undefined, (<$>), (<*), (*>), (<*>), (++) )
import Text.Parsec ( (<|>), try, char, sepBy1, string )
import Text.Parsec.String ( Parser )
import LowLevel.TypeExpression ( TypeExpression )
import LowLevel.AtomicExpression
  ( AtomicExpression, atomic_expression_p, NameExpression )

-- HighPrecedenceExpression

data HighPrecedenceExpression =
  Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving (Eq)

instance Show HighPrecedenceExpression where
  show = \case
    Parenthesis pe -> show pe
    Atomic ae -> show ae

high_precedence_expression_p =
  (Parenthesis <$> parenthesis_expression_p) <|> (Atomic <$> atomic_expression_p)
  :: Parser HighPrecedenceExpression

-- ApplicationExpression

data ApplicationExpression = 
  LeftApplication HighPrecedenceExpression ApplicationExpression |
  RightApplication HighPrecedenceExpression ApplicationExpression |
  HighPrecedence HighPrecedenceExpression
  deriving (Eq)

instance Show ApplicationExpression where
  show = \case
    LeftApplication hpe ae -> show hpe ++ " left_application " ++ show ae
    RightApplication hpe ae -> show hpe ++ " right_application " ++ show ae
    HighPrecedence hpe -> show hpe

application_expression_p =
  try left_application_expression_p <|> try right_application_expression_p <|>
  (HighPrecedence <$> high_precedence_expression_p)
  :: Parser ApplicationExpression

[ left_application_expression_p, right_application_expression_p ] = 
  [ LeftApplication
    <$> (high_precedence_expression_p <* string "<--") <*> application_expression_p
  , RightApplication
    <$> (high_precedence_expression_p <* string "-->") <*> application_expression_p ]
  :: [ Parser ApplicationExpression ]

-- MultiplicationExpression

data MultiplicationExpression =
  Multiplication ApplicationExpression MultiplicationExpression |
  Application ApplicationExpression
  deriving (Eq)

instance Show MultiplicationExpression where
  show = \case
    Multiplication ae me -> "(" ++ show ae ++ " mul " ++ show me ++ ")"
    Application ae -> show ae

[ multiplication_p, multiplication_expression_p ] =
  [ Multiplication
    <$> (application_expression_p <* string " * ") <*> multiplication_expression_p
  , try multiplication_p <|> (Application <$> application_expression_p)
  ]
  :: [ Parser MultiplicationExpression ]

-- SubtractionExpression

data SubtractionExpression =
  Subtraction MultiplicationExpression MultiplicationExpression |
  MultiplicationExp MultiplicationExpression
  deriving (Eq)

instance Show SubtractionExpression where
  show = \case
    Subtraction me1 me2 -> "(" ++ show me1 ++ " minus " ++ show me2 ++ ")"
    MultiplicationExp me -> show me

subtraction_expression_p =
  try subtraction_p <|> (MultiplicationExp <$> multiplication_expression_p)
  :: Parser SubtractionExpression

subtraction_p =
  Subtraction
    <$> (multiplication_expression_p <* string " - ") <*> multiplication_expression_p
  :: Parser SubtractionExpression

-- ValueExpression

type ResultExpression = SubtractionExpression

data AbstractionExpression =
  Abstraction NameExpression AbstractionExpression |
  Result ResultExpression

data CaseExpression = Case [ ( ResultExpression, ResultExpression ) ]

data ValueExpression =
  ApplicationExpression | CaseExpression | InteremediatesOutput [ NameTypeAndValue ]
  deriving (Eq, Show)

value_expression_p = undefined
  :: Parser ValueExpression

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving (Eq, Show)

[ parenthesis_expression_p, for_precedence_p, tuple_expression_p ] =
  [ char '(' *> (try for_precedence_p <|> tuple_expression_p) <* char ')'
  , ForPrecedence <$> value_expression_p
  , Tuple <$> ( char ' ' *> sepBy1 value_expression_p (string ", ") <* char ' ' ) ]
  :: [ Parser ParenthesisExpression ]

-- NameTypeAndValue

data NameTypeAndValue = NTAV
  { get_name :: String, get_type :: TypeExpression, get_value :: ValueExpression }
  deriving (Eq, Show)

--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ String ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValue ]
