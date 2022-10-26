{-# LANGUAGE LambdaCase #-}

module Parsers.ValueExpressions where

import Prelude
  ( Eq, Show, String, Bool( True ), show, undefined, (<$>), (<*), (*>), (<*>), (++), ($)
  , (>>=), (>>), (==), (&&), return, map, length, concat, error, fmap )
import Text.Parsec
  ( (<|>), try, char, many, many1, string, eof, skipMany1, parserFail, optional )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), (.>), seperated2, comma_seperated2, spaces_tabs, new_line_space_surrounded )
import Parsers.LowLevel
  ( AtomicExpression, atomic_expression_p, NameExpression, name_expression_p
  , TupleMatchingExpression, tuple_matching_expression_p, ApplicationDirection
  , application_direction_p, TypeExpression, type_expression_p )

{- 
  All:
  ParenthesisExpression, HighPrecedenceExpression, ApplicationExpression,
  MultiplicationFactor, MultiplicationExpression,
  SubtractionFactor, SubtractionExpression,
  SpecificCaseExpression, CasesExpression,
  NameTypeAndValueExpression, NameTypeAndValueListsExpression,
  NameTypeAndValueOrListsExpression, NameTypeAndValueExpressions,
  IntermediatesOutputExpression,
  AbstractionArgument, NoAbstractionsValueExpression, ValueExpression
-}

-- ParenthesisExpression

data ParenthesisExpression = ForPrecedence ValueExpression | Tuple [ ValueExpression ]
  deriving ( Eq )

instance Show ParenthesisExpression where
  show = \case
    ForPrecedence e -> "(" ++ show e ++ ")"
    Tuple es -> "Tuple " ++ show es

[ parenthesis_expression_p, tuple_internals_p, for_precedence_internals_p ] =
  [ char '(' *> (try tuple_internals_p <|> for_precedence_internals_p) <* char ')'
  , fmap Tuple $ char ' ' *> comma_seperated2 value_expression_p <* char ' '
  , ForPrecedence <$> value_expression_p ]
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

-- ApplicationExpression

data ApplicationExpression = 
  Application
    [ ( HighPrecedenceExpression, ApplicationDirection ) ] HighPrecedenceExpression
  deriving ( Eq )

instance Show ApplicationExpression where
  show = \(Application hpe_ad_s hpe) -> case hpe_ad_s of
    [] -> error "application expression should have at least one application direction"
    _ ->
      hpe_ad_s-->map (\( hpe, ad ) -> show hpe ++ " " ++ show ad ++ " ")-->concat
      ++ show hpe

application_expression_p =
  many1 (try high_precedence_expression_and_application_direction_p) >>= \hpe_ad_s -> 
  high_precedence_expression_p >>= \hpe ->
  return $ Application hpe_ad_s hpe
  :: Parser ApplicationExpression

high_precedence_expression_and_application_direction_p = 
  high_precedence_expression_p >>= \hpe ->
  application_direction_p >>= \ad ->
  return ( hpe, ad )
  :: Parser ( HighPrecedenceExpression, ApplicationDirection )

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
  show = \(Multiplication aes) -> case aes of
      [] -> error "found less than 2 in multiplication"
      [ _ ] -> show (Multiplication [])
      [ ae1, ae2 ] -> "(" ++ show ae1 ++ " mul " ++ show ae2 ++ ")"
      (ae:aes) -> "(" ++ show ae ++ " mul " ++ show (Multiplication aes) ++ ")"

multiplication_expression_p =
  Multiplication <$> seperated2 multiplication_factor_p " * "
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

data SubtractionExpression = Subtraction SubtractionFactor SubtractionFactor 
  deriving ( Eq )

instance Show SubtractionExpression where
  show = \(Subtraction sf1 sf2) -> "(" ++ show sf1 ++ " minus " ++ show sf2 ++ ")"

subtraction_expression_p =
  subtraction_factor_p >>= \sf1 ->
  string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Subtraction sf1 sf2
  :: Parser SubtractionExpression

-- SpecificCaseExpression

data SpecificCaseExpression = SpecificCase AtomicExpression ValueExpression 
  deriving ( Eq )
 
instance Show SpecificCaseExpression where
  show = \(SpecificCase ae ve) -> 
    "specific case: " ++ show ae ++ "\n" ++
    "result: " ++ show ve ++ "\n"

specific_case_expression_p =
  spaces_tabs >> atomic_expression_p >>= \ae ->
  string " ->" >> (char ' ' <|> char '\n') >> value_expression_p >>= \ve ->
  return $ SpecificCase ae ve
  :: Parser SpecificCaseExpression

-- CasesExpression

newtype CasesExpression = Cases [ SpecificCaseExpression ]
  deriving ( Eq )

instance Show CasesExpression where
  show = \(Cases sces) ->
    ("\ncase start\n\n" ++) $ sces --> map (show .> (++ "\n")) --> concat

cases_expression_p =
  fmap Cases $ string "cases\n" *> many1 (specific_case_expression_p <* char '\n')
  :: Parser CasesExpression

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
  name_expression_p >>= \ne ->
  string ": " >> type_expression_p >>= \te ->
  new_line_space_surrounded >>
  string "= " >> value_expression_p >>= \ve ->
  (eof <|> skipMany1 new_line_space_surrounded) >>
  return (NameTypeAndValue ne te ve)
  :: Parser NameTypeAndValueExpression

-- NameTypeAndValueListsExpression

data NameTypeAndValueListsExpression =
  NameTypeAndValueLists [ NameExpression ] [ TypeExpression ] [ ValueExpression ]
  deriving (Eq)

instance Show NameTypeAndValueListsExpression where
  show = \(NameTypeAndValueLists nes tes ves) -> 
    "names: " ++  nes --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "types: " ++ tes --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "values: " ++ ves --> map (show .> (++ ", ")) --> concat ++ "\n"

name_type_and_value_lists_expression_p = 
  spaces_tabs >> comma_seperated2 name_expression_p >>= \nes ->
  string ": " >> comma_seperated2 type_expression_p >>= \tes ->
  new_line_space_surrounded >>
  string "= " >> comma_seperated2 value_expression_p >>= \ves ->
  (eof <|> skipMany1 new_line_space_surrounded) >>
  NameTypeAndValueLists nes tes ves --> return
  :: Parser NameTypeAndValueListsExpression
  
-- NameTypeAndValueOrListsExpression

data NameTypeAndValueOrListsExpression =
  NameTypeAndValueExp NameTypeAndValueExpression |
  NameTypeAndValueListsExp NameTypeAndValueListsExpression
  deriving (Eq)

instance Show NameTypeAndValueOrListsExpression where
  show = \case
    NameTypeAndValueExp ntave -> show ntave
    NameTypeAndValueListsExp ntavle -> show ntavle

name_type_and_value_or_lists_expression_p = 
  NameTypeAndValueListsExp <$> try name_type_and_value_lists_expression_p <|>
  NameTypeAndValueExp <$> name_type_and_value_expression_p
  :: Parser NameTypeAndValueOrListsExpression

-- NameTypeAndValueExpressions

newtype NameTypeAndValueExpressions =
  NameTypeAndValues [ NameTypeAndValueOrListsExpression ]
  deriving ( Eq )

instance Show NameTypeAndValueExpressions where
  show = \(NameTypeAndValues ntaves) ->
    "\n" ++ ntaves-->map (show .> (++ "\n"))-->concat

name_type_and_value_expressions_p = 
  NameTypeAndValues <$> try name_type_and_value_or_lists_expression_p-->many1
  :: Parser NameTypeAndValueExpressions

-- IntermediatesOutputExpression

data IntermediatesOutputExpression =
  IntermediatesOutputExpression NameTypeAndValueExpressions ValueExpression
  deriving ( Eq )

instance Show IntermediatesOutputExpression where
  show = \(IntermediatesOutputExpression ntave ve) -> 
    "intermediates\n" ++ show ntave ++ "output\n" ++ show ve

intermediates_output_expression_p = 
  spaces_tabs >> string "intermediates" >> new_line_space_surrounded >>
  name_type_and_value_expressions_p >>= \ntave ->
  spaces_tabs >> string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \ve ->
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

-- NoAbstractionsValueExpression

data NoAbstractionsValueExpression =
  SubtractionExp SubtractionExpression | MultiplicationExp MultiplicationExpression |
  ApplicationExp ApplicationExpression | HighPrecedenceExp HighPrecedenceExpression |
  CasesExp CasesExpression | IntermediatesOutputExp IntermediatesOutputExpression
  deriving ( Eq )

instance Show NoAbstractionsValueExpression where
  show = \case
    SubtractionExp e -> show e
    MultiplicationExp e -> show e
    ApplicationExp e -> show e
    HighPrecedenceExp e -> show e
    CasesExp e -> show e
    IntermediatesOutputExp e -> show e

no_abstraction_expression_p =
  SubtractionExp <$> try subtraction_expression_p <|>
  MultiplicationExp <$> try multiplication_expression_p <|>
  ApplicationExp <$> try application_expression_p <|>
  HighPrecedenceExp <$> try high_precedence_expression_p <|>
  CasesExp <$> cases_expression_p <|>
  IntermediatesOutputExp <$> intermediates_output_expression_p
  :: Parser NoAbstractionsValueExpression

-- ValueExpression

data ValueExpression =
  Value [ AbstractionArgumentExpression ] NoAbstractionsValueExpression
  deriving ( Eq )

instance Show ValueExpression where
  show = \(Value aaes nae) ->
    aaes-->map (show .> (++ " abstraction "))-->concat ++ show nae

value_expression_p =
  many (try $ abstraction_argument_expression_p <* string " -> ") >>= \aaes ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value aaes nae
  :: Parser ValueExpression
