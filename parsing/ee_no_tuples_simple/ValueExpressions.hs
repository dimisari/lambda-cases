{-# LANGUAGE LambdaCase #-}

module ValueExpressions where

import Prelude
  ( Eq, Show, String, show, undefined, (<$>), (<*), (*>), (<*>), (++), ($), return )
import Text.Parsec ( (<|>), try, char, many1, string )
import Text.Parsec.String ( Parser )
import LowLevel.TypeExpression ( TypeExpression )
import LowLevel.AtomicExpression
  ( AtomicExpression, atomic_expression_p, NameExpression, name_expression_p
  , TupleMatchingExpression, tuple_matching_expression_p )
import LowLevel.Helpers (seperated2)

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving (Eq)

instance Show ParenthesisExpression where
  show = \case
    ForPrecedence e -> "(" ++ show e ++ ")"
    Tuple es -> "Tuple: " ++ show es

[ parenthesis_expression_p, for_precedence_p, tuple_expression_p ] =
  [ try tuple_expression_p <|> for_precedence_p
  , ForPrecedence <$> (char '(' *> value_expression_p <* char ')')
  , Tuple <$> (string "( " *> seperated2 value_expression_p ", " <* string " )")
  ]
  :: [ Parser ParenthesisExpression ]

-- HighPrecedenceExpression

data HighPrecedenceExpression =
  Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving (Eq)

instance Show HighPrecedenceExpression where
  show = \case
    Parenthesis pe -> show pe
    Atomic ae -> show ae

high_precedence_expression_p =
  Parenthesis <$> parenthesis_expression_p <|> Atomic <$> atomic_expression_p
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
  high_precedence_p
  :: Parser ApplicationExpression

[ left_application_expression_p, right_application_expression_p, high_precedence_p ] = 
  [ LeftApplication
    <$> high_precedence_expression_p <*> (string "<--" *> application_expression_p)
  , RightApplication
    <$> high_precedence_expression_p <*> (string "-->" *> application_expression_p)
  , HighPrecedence <$> high_precedence_expression_p ]
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

[ multiplication_expression_p, multiplication_p ] =
  [ try multiplication_p <|> Application <$> application_expression_p
  , Multiplication
    <$> application_expression_p <*> (string " * " *> multiplication_expression_p) ]
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

subtraction_expression_p = try subtraction_p <|> multiplication_exp_p
  :: Parser SubtractionExpression

[ subtraction_p, multiplication_exp_p ] =
  [ Subtraction
    <$> (multiplication_expression_p <* string " - ") <*> multiplication_expression_p
  , MultiplicationExp <$> multiplication_expression_p ]
  :: [ Parser SubtractionExpression ]

-- ResultExpression

type ResultExpression = SubtractionExpression

-- AbstractionArgument

data AbstractionArgumentExpression =
  Name NameExpression | TupleMatching TupleMatchingExpression
  deriving (Eq, Show)

abstraction_argument_expression_p =
  Name <$> name_expression_p <|> TupleMatching <$> tuple_matching_expression_p
  :: Parser AbstractionArgumentExpression

-- AbstractionExpression

data AbstractionExpression =
  Abstraction AbstractionArgumentExpression AbstractionExpression |
  Result ResultExpression
  deriving (Eq)

instance Show AbstractionExpression where
  show = \case
    Abstraction aae ae -> show aae ++ " abstraction " ++ show ae
    Result e -> show e

[ abstraction_expression_p, abstraction_p, result_p ] =
  [ try abstraction_p <|> result_p
  , Abstraction <$> abstraction_argument_expression_p <*>
    (string " -> " *> abstraction_expression_p)
  , Result <$> subtraction_expression_p ]
  :: [ Parser AbstractionExpression ]

-- CaseExpression

newtype CaseExpression = Case [ ( AtomicExpression, ValueExpression ) ]
  deriving (Eq, Show)

case_expression_p = undefined
  :: Parser CaseExpression

-- InterOutputExpression

newtype InterOutputExpression = InteremediatesAndOutput [ NameTypeAndValue ]
  deriving (Eq, Show)

inter_output_expression_p = undefined
  :: Parser InterOutputExpression

-- ValueExpression

data ValueExpression =
  AbstractionExp AbstractionExpression | CaseExp CaseExpression |
  InterOutputExp InterOutputExpression 
  deriving (Eq)

instance Show ValueExpression where
  show = \case
    AbstractionExp e -> show e
    CaseExp e -> show e
    InterOutputExp e -> show e

value_expression_p =
  AbstractionExp <$> try abstraction_expression_p <|>
  CaseExp <$> try case_expression_p <|>
  InterOutputExp <$> inter_output_expression_p
  :: Parser ValueExpression

-- NameTypeAndValue

data NameTypeAndValue = NTAV
  { get_name :: NameExpression, get_type :: TypeExpression
  , get_value :: ValueExpression }
  deriving (Eq, Show)

name_type_and_value_p = undefined
  :: Parser NameTypeAndValueLists

--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ NameExpression ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValue ]
