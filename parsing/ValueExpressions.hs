{-# LANGUAGE LambdaCase #-}

module ValueExpressions where

import Prelude
  ( Eq, Show, String, show, undefined, (<$>), (<*), (*>), (<*>), (++), ($), (>>=), (>>)
  , return, map, concat, error )
import Text.Parsec ( (<|>), try, char, many, many1, string, eof, skipMany1 )
import Text.Parsec.String ( Parser )
import LowLevel.TypeExpression ( TypeExpression, type_expression_p )
import LowLevel.AtomicExpression
  ( AtomicExpression, atomic_expression_p, NameExpression, name_expression_p
  , TupleMatchingExpression, tuple_matching_expression_p )
import LowLevel.Helpers ( seperated2, (-->), (.>) )

{- 
All:
ParenthesisExpression, HighPrecedenceExpression,
ApplicationDirection, ApplicationExpression,
MultiplicationFactor, MultiplicationExpression,
SubtractionFactor, SubtractionExpression,
SpecificCaseExpression, CaseExpression,
NameTypeAndValueExpression, NameTypeAndValueExpressions, IntermediatesOutputExpression,
AbstractionArgument, NoAbstractionsExpression, ValueExpression
-}

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving ( Eq )

instance Show ParenthesisExpression where
  show = \case
    ForPrecedence e -> "(" ++ show e ++ ")"
    Tuple es -> "Tuple " ++ show es

[ parenthesis_expression_p, tuple_internals_p ] =
  [ char '(' *>
    (try tuple_internals_p <|> ForPrecedence <$> value_expression_p)
    <* char ')'
  , Tuple <$> (char ' ' *> seperated2 value_expression_p ", " <* char ' ') ]
  :: [ Parser ParenthesisExpression ]

-- HighPrecedenceExpression

data HighPrecedenceExpression =
  Parenthesis ParenthesisExpression | Atomic AtomicExpression
  deriving ( Eq )

instance Show HighPrecedenceExpression where
  show = \case
    Parenthesis pe -> show pe
    Atomic ae -> show ae

high_precedence_expression_p =
  Parenthesis <$> parenthesis_expression_p <|> Atomic <$> atomic_expression_p
  :: Parser HighPrecedenceExpression

-- ApplicationDirection

data ApplicationDirection = LeftApplication | RightApplication 
  deriving ( Eq, Show )

application_direction_p = 
  string "<--" *> return LeftApplication <|> string "-->" *> return RightApplication
  :: Parser ApplicationDirection

-- ApplicationExpression

data ApplicationExpression = 
  Application
    [ ( HighPrecedenceExpression, ApplicationDirection ) ] HighPrecedenceExpression
  deriving ( Eq )

instance Show ApplicationExpression where
  show = \(Application hpe_ad_s hpe) -> case hpe_ad_s of
    [] -> error "application expression should have at least one application direction"
    _ ->
      let
      show_hpe_ad = (\( hpe, ad ) -> show hpe ++ " " ++ show ad ++ " ")
        :: ( HighPrecedenceExpression, ApplicationDirection ) -> String
      in
      hpe_ad_s --> map show_hpe_ad --> concat --> (++ show hpe)

high_precedence_expression_and_application_direction_p = 
  high_precedence_expression_p >>= \hpe ->
  application_direction_p >>= \ad ->
  return ( hpe, ad )
  :: Parser ( HighPrecedenceExpression, ApplicationDirection )

application_expression_p =
  many1 (try high_precedence_expression_and_application_direction_p) >>= \hpe_ad_s -> 
  high_precedence_expression_p >>= \hpe ->
  return (Application hpe_ad_s hpe)
  :: Parser ApplicationExpression

-- MultiplicationFactor

data MultiplicationFactor =
  ApplicationMF ApplicationExpression | HighPrecedenceMF HighPrecedenceExpression
  deriving ( Eq )

instance Show MultiplicationFactor where
  show = \case
    ApplicationMF e -> show e
    HighPrecedenceMF e -> show e

multiplication_factor_p =
  ApplicationMF <$> application_expression_p <|>
  HighPrecedenceMF <$> high_precedence_expression_p
  :: Parser MultiplicationFactor

-- MultiplicationExpression

data MultiplicationExpression = Multiplication [ MultiplicationFactor ]
  deriving ( Eq )

instance Show MultiplicationExpression where
  show = \case
    Multiplication aes -> case aes of
      [] -> error "found less than 2 in multiplication"
      [ _ ] -> show (Multiplication [])
      [ ae1, ae2 ] -> "(" ++ show ae1 ++ " mul " ++ show ae2 ++ ")"
      (ae:aes) -> "(" ++ show ae ++ " mul " ++ show (Multiplication aes) ++ ")"

multiplication_expression_p = Multiplication <$> seperated2 multiplication_factor_p " * "
  :: Parser MultiplicationExpression

-- SubtractionFactor

data SubtractionFactor =
  MultiplicationSF MultiplicationExpression | ApplicationSF ApplicationExpression |
  HighPrecedenceSF HighPrecedenceExpression
  deriving ( Eq )

instance Show SubtractionFactor where
  show = \case
    ApplicationSF e -> show e
    HighPrecedenceSF e -> show e
    MultiplicationSF e -> show e

subtraction_factor_p =
  try (MultiplicationSF <$> multiplication_expression_p) <|>
  ApplicationSF <$> application_expression_p <|>
  HighPrecedenceSF <$> high_precedence_expression_p 
  :: Parser SubtractionFactor

-- SubtractionExpression

data SubtractionExpression =
  Subtraction SubtractionFactor SubtractionFactor 
  deriving ( Eq )

instance Show SubtractionExpression where
  show = \(Subtraction me1 me2) -> "(" ++ show me1 ++ " minus " ++ show me2 ++ ")"

subtraction_expression_p  =
  try (Subtraction <$> (subtraction_factor_p <* string " - ") <*> subtraction_factor_p)
  :: Parser SubtractionExpression

-- SpecificCaseExpression

data SpecificCaseExpression = SpecificCase AtomicExpression ValueExpression 
  deriving ( Eq )
 
instance Show SpecificCaseExpression where
  show = \(SpecificCase ae ve) -> 
    "specific case: " ++ show ae ++ "\n" ++
    "result: " ++ show ve ++ "\n"

specific_case_expression_p =
  many (char ' ' <|> char '\t') >> atomic_expression_p >>= \ae ->
  string " ->" >> (char ' ' <|> char '\n') >> value_expression_p >>= \ve ->
  return $ SpecificCase ae ve
  :: Parser SpecificCaseExpression

-- CaseExpression

newtype CaseExpression = Case [ SpecificCaseExpression ]
  deriving ( Eq )

instance Show CaseExpression where
  show = \(Case sces) ->
    "\ncase start\n\n" ++ (sces --> map (show .> (++ "\n")) --> concat)

case_expression_p =
  Case <$> (string "case\n" *> many1 (specific_case_expression_p <* char '\n'))
  :: Parser CaseExpression

-- NameTypeAndValueExpression

data NameTypeAndValueExpression =
  NameTypeAndValue NameExpression TypeExpression ValueExpression
  deriving ( Eq )

instance Show NameTypeAndValueExpression where
  show = \(NameTypeAndValue ne te ve) -> 
    "name: " ++ show ne ++ "\n" ++
    "type: " ++ show te ++ "\n" ++
    "value: " ++ show ve ++ "\n"

name_type_and_value_expression_p =
  many (char ' ' <|> char '\t') >> name_expression_p >>= \ne ->
  string ": " >> type_expression_p >>= \te ->
  char '\n' >> many (char ' ' <|> char '\t') >> string "= " >>
  value_expression_p >>= \ve ->
  (skipMany1 (char '\n') <|> eof) >>
  return (NameTypeAndValue ne te ve)
  :: Parser NameTypeAndValueExpression

-- NameTypeAndValueExpressions

newtype NameTypeAndValueExpressions = NameTypeAndValueExps [ NameTypeAndValueExpression ]
  deriving ( Eq )

instance Show NameTypeAndValueExpressions where
  show = \(NameTypeAndValueExps ntave) ->
    ntave-->map (show .> (++ "\n"))-->concat-->( "\n" ++)

name_type_and_value_expressions_p = 
  NameTypeAndValueExps <$> many1 (try name_type_and_value_expression_p)
  :: Parser NameTypeAndValueExpressions

-- IntermediatesOutputExpression

data IntermediatesOutputExpression =
  IntermediatesOutputExpression NameTypeAndValueExpressions ValueExpression
  deriving ( Eq )

instance Show IntermediatesOutputExpression where
  show = \(IntermediatesOutputExpression ntave ve) -> 
    "intermediates\n" ++ show ntave ++ "output\n" ++ show ve

intermediates_output_expression_p = 
  many (char ' ' <|> char '\t') >> string "intermediates\n" >>
  name_type_and_value_expressions_p >>= \ntave ->
  many (char ' ' <|> char '\t') >> string "output\n" >>
  many (char ' ' <|> char '\t') >> value_expression_p >>= \ve ->
  return $ IntermediatesOutputExpression ntave ve
  :: Parser IntermediatesOutputExpression

-- AbstractionArgumentExpression

data AbstractionArgumentExpression =
  Name NameExpression | TupleMatching TupleMatchingExpression
  deriving ( Eq )

instance Show AbstractionArgumentExpression where
  show = \case
    Name e -> show e
    TupleMatching e -> show e

abstraction_argument_expression_p =
  Name <$> name_expression_p <|> TupleMatching <$> tuple_matching_expression_p
  :: Parser AbstractionArgumentExpression

-- NoAbstractionsExpression

data NoAbstractionsExpression =
  SubtractionExp SubtractionExpression | MultiplicationExp MultiplicationExpression |
  ApplicationExp ApplicationExpression | HighPrecedenceExp HighPrecedenceExpression |
  CaseExp CaseExpression | IntermediatesOutputExp IntermediatesOutputExpression
  deriving ( Eq )

instance Show NoAbstractionsExpression where
  show = \case
    SubtractionExp e -> show e
    MultiplicationExp e -> show e
    ApplicationExp e -> show e
    HighPrecedenceExp e -> show e
    CaseExp e -> show e
    IntermediatesOutputExp e -> show e

no_abstraction_expression_p =
  SubtractionExp <$> try subtraction_expression_p <|>
  MultiplicationExp <$> try multiplication_expression_p <|>
  ApplicationExp <$> try application_expression_p <|>
  HighPrecedenceExp <$> try high_precedence_expression_p <|>
  CaseExp <$> case_expression_p <|>
  IntermediatesOutputExp <$> intermediates_output_expression_p
  :: Parser NoAbstractionsExpression

-- ValueExpression

data ValueExpression =
  Abstraction AbstractionArgumentExpression ValueExpression |
  NoAbstraction NoAbstractionsExpression
  deriving ( Eq )

instance Show ValueExpression where
  show = \case
    Abstraction aae ae -> show aae ++ " abstraction " ++ show ae
    NoAbstraction e -> show e

[ value_expression_p, abstraction_p ] =
  [ try abstraction_p <|> NoAbstraction <$> no_abstraction_expression_p
  , Abstraction <$>
    abstraction_argument_expression_p <*> (string " -> " *> value_expression_p) ]
  :: [ Parser ValueExpression ]
