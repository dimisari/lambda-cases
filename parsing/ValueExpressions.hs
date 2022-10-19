{-# LANGUAGE LambdaCase #-}

module ValueExpressions where

import Prelude
  ( Eq, Show, String, show, undefined, (<$>), (<*), (*>), (<*>), (++), ($), return
  , map, concat )
import Text.Parsec ( (<|>), try, char, many, many1, string, eof, skipMany1 )
import Text.Parsec.String ( Parser )
import LowLevel.TypeExpression ( TypeExpression, type_expression_p )
import LowLevel.AtomicExpression
  ( AtomicExpression, atomic_expression_p, NameExpression, name_expression_p
  , TupleMatchingExpression, tuple_matching_expression_p )
import LowLevel.Helpers ( seperated2, (-->), (.>) )

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving (Eq)

instance Show ParenthesisExpression where
  show = \case
    ForPrecedence e -> "(" ++ show e ++ ")"
    Tuple es -> "Tuple " ++ show es

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

-- SpecificCaseExpression

data SpecificCaseExpression = SpecificCase AtomicExpression ValueExpression 
  deriving ( Eq )
 
instance Show SpecificCaseExpression where
  show = \(SpecificCase ae ve) -> 
    "specific case: " ++ show ae ++ "\n" ++
    "result: " ++ show ve ++ "\n"

specific_case_expression_p = do 
  many (char ' ' <|> char '\t')
  ae <- atomic_expression_p
  string " ->"
  char ' ' <|> char '\n'
  ve <- value_expression_p
  return $ SpecificCase ae ve
  :: Parser SpecificCaseExpression

-- CaseExpression

newtype CaseExpression = Case [ SpecificCaseExpression ]
  deriving (Eq)

instance Show CaseExpression where
  show = \(Case sces) ->
    "case start\n\n" ++ sces --> map (show .> (++ "\n")) --> concat

case_expression_p =
  Case <$> (string "case\n" *> many1 (specific_case_expression_p <* char '\n'))
  :: Parser CaseExpression

-- NameTypeAndValueExpression

data NameTypeAndValueExpression =
  NameTypeAndValue NameExpression TypeExpression ValueExpression
  deriving (Eq)

instance Show NameTypeAndValueExpression where
  show = \(NameTypeAndValue ne te ve) -> 
    "name: " ++ show ne ++ "\n" ++
    "type: " ++ show te ++ "\n" ++
    "value: " ++ show ve ++ "\n"

name_type_and_value_expression_p = do 
  many (char ' ' <|> char '\t')
  ne <- name_expression_p
  string ": "
  te <- type_expression_p
  char '\n'
  many (char ' ' <|> char '\t')
  string "= "
  ve <- value_expression_p
  skipMany1 (char '\n') <|> eof
  return $ NameTypeAndValue ne te ve
  :: Parser NameTypeAndValueExpression

-- NameTypeAndValueExpressions

newtype NameTypeAndValueExpressions = NameTypeAndValueExps [ NameTypeAndValueExpression ]
  deriving (Eq)

instance Show NameTypeAndValueExpressions where
  show = \(NameTypeAndValueExps ntave) ->
    ntave --> map (show .> (++ "\n")) --> concat --> ( "\n" ++)

name_type_and_value_expressions_p = 
  NameTypeAndValueExps <$> many1 (try name_type_and_value_expression_p)
  :: Parser NameTypeAndValueExpressions

-- IntermediatesOutputExpression

data IntermediatesOutputExpression =
  IntermediatesOutputExpression NameTypeAndValueExpressions ValueExpression
  deriving (Eq)

instance Show IntermediatesOutputExpression where
  show = \(IntermediatesOutputExpression ntave ve) -> 
    "intermediates\n" ++ show ntave ++ "output\n" ++ show ve

intermediates_output_expression_p = do 
  many (char ' ' <|> char '\t')
  string "intermediates\n"
  ntave <- name_type_and_value_expressions_p
  many (char ' ' <|> char '\t')
  string "output\n"
  many (char ' ' <|> char '\t')
  ve <- value_expression_p
  return $ IntermediatesOutputExpression ntave ve
  :: Parser IntermediatesOutputExpression

-- AbstractionArgument

data AbstractionArgumentExpression =
  Name NameExpression | TupleMatching TupleMatchingExpression
  deriving (Eq, Show)

abstraction_argument_expression_p =
  Name <$> name_expression_p <|> TupleMatching <$> tuple_matching_expression_p
  :: Parser AbstractionArgumentExpression

-- ResultExpression

data ResultExpression =
  SubtractionExp SubtractionExpression | CaseExp CaseExpression |
  IntermediatesOutputExp IntermediatesOutputExpression
  deriving ( Eq )

instance Show ResultExpression where
  show = \case
    SubtractionExp e -> show e
    CaseExp e -> show e
    IntermediatesOutputExp e -> show e

result_expression_p =
  SubtractionExp <$> try subtraction_expression_p <|>
  CaseExp <$> try case_expression_p <|>
  IntermediatesOutputExp <$> intermediates_output_expression_p
  :: Parser ResultExpression

-- ValueExpression

data ValueExpression =
  Abstraction AbstractionArgumentExpression ValueExpression |
  Result ResultExpression
  deriving (Eq)

instance Show ValueExpression where
  show = \case
    Abstraction aae ae -> show aae ++ " abstraction " ++ show ae
    Result e -> show e

[ value_expression_p, abstraction_p, result_p ] =
  [ try abstraction_p <|> result_p
  , Abstraction <$> abstraction_argument_expression_p <*>
    (string " -> " *> value_expression_p)
  , Result <$> result_expression_p ]
  :: [ Parser ValueExpression ]

--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ NameExpression ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValueExpression ]
