{-# LANGUAGE LambdaCase #-}

module Parsers.ValueExpressions where

import Prelude
  ( Eq, Show, (<$>), (<*), (*>), (++), ($), (>>=), (>>), (==), (&&), return, show
  , map, length, concat, error, fmap )
import Text.Parsec
  ( (<|>), try, char, many, many1, string, eof, skipMany1, parserFail, optional )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), (.>), seperated2, comma_seperated2, spaces_tabs, new_line_space_surrounded )
import Parsers.LowLevel
  ( ValueName, AtomicExpression, ApplicationDirection
  , TupleMatchingExpression
  , AbstractionArgumentExpression
  , AbstractionArgumentsExpression ( AbstractionArguments )
  , TypeExpression
  , value_name_p, atomic_expression_p, application_direction_p
  , tuple_matching_expression_p
  , abstraction_argument_expression_p
  , abstraction_arguments_expression_p
  , type_expression_p )

{- 
  All:
  ParenthesisExpression, HighPrecedenceExpression, OneArgumentApplicationsExpression,
  MultiplicationFactor, MultiplicationExpression,
  SubtractionFactor, SubtractionExpression,
  SpecificCaseExpression, CasesExpression,
  NameTypeAndValueExpression, NameTypeAndValueListsExpression,
  NameTypeAndValueOrListsExpression, NameTypeAndValueExpressions,
  IntermediatesOutputExpression,
  NoAbstractionsValueExpression1, ArgumentValueExpression,
  ManyArgumentsApplicationExpression,
  NoAbstractionsValueExpression, ValueExpression
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

-- OneArgumentApplicationsExpression

data OneArgumentApplicationsExpression = 
  OneArgumentApplications
    HighPrecedenceExpression [ ( ApplicationDirection, HighPrecedenceExpression ) ]
  deriving ( Eq )

instance Show OneArgumentApplicationsExpression where
  show = \(OneArgumentApplications hpe ad_hpe_s) -> case ad_hpe_s of
    [] -> error "application expression should have at least one application direction"
    _ ->
      show hpe ++
      ad_hpe_s-->map ( \( ad, hpe ) -> show ad ++ " " ++ show hpe ++ " " )-->concat

one_arg_applications_expression_p =
  high_precedence_expression_p >>= \hpe ->
  many1 ad_hpe_p >>= \ad_hpe_s -> 
  return $ OneArgumentApplications hpe ad_hpe_s
  :: Parser OneArgumentApplicationsExpression

ad_hpe_p = 
  application_direction_p >>= \ad ->
  high_precedence_expression_p >>= \hpe ->
  return ( ad, hpe )
  :: Parser ( ApplicationDirection,  HighPrecedenceExpression )

-- MultiplicationFactor

data MultiplicationFactor =
  OneArgApplicationMF OneArgumentApplicationsExpression |
  HighPrecedenceMF HighPrecedenceExpression
  deriving ( Eq )

instance Show MultiplicationFactor where
  show = \case
    OneArgApplicationMF e -> show e
    HighPrecedenceMF e -> show e

multiplication_factor_p =
  OneArgApplicationMF <$> try one_arg_applications_expression_p <|>
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
  MultiplicationSF MultiplicationExpression |
  OneArgApplicationSF OneArgumentApplicationsExpression |
  HighPrecedenceSF HighPrecedenceExpression
  deriving ( Eq )

instance Show SubtractionFactor where
  show = \case
    OneArgApplicationSF e -> show e
    HighPrecedenceSF e -> show e
    MultiplicationSF e -> show e

subtraction_factor_p =
  MultiplicationSF <$> try multiplication_expression_p <|>
  OneArgApplicationSF <$> try one_arg_applications_expression_p <|>
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
  NameTypeAndValue ValueName TypeExpression ValueExpression
  deriving ( Eq )

instance Show NameTypeAndValueExpression where
  show = \(NameTypeAndValue ne te ve) -> 
    "name: " ++ show ne ++ "\n" ++
    "type: " ++ show te ++ "\n" ++
    "value: " ++ show ve ++ "\n"

name_type_and_value_expression_p =
  value_name_p >>= \ne ->
  string ": " >> type_expression_p >>= \te ->
  new_line_space_surrounded >>
  string "= " >> value_expression_p >>= \ve ->
  (eof <|> skipMany1 new_line_space_surrounded) >>
  return (NameTypeAndValue ne te ve)
  :: Parser NameTypeAndValueExpression

-- NameTypeAndValueListsExpression

data NameTypeAndValueListsExpression =
  NameTypeAndValueLists [ ValueName ] [ TypeExpression ] [ ValueExpression ]
  deriving (Eq)

instance Show NameTypeAndValueListsExpression where
  show = \(NameTypeAndValueLists nes tes ves) -> 
    "names: " ++  nes --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "types: " ++ tes --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "values: " ++ ves --> map (show .> (++ ", ")) --> concat ++ "\n"

name_type_and_value_lists_expression_p = 
  spaces_tabs >> comma_seperated2 value_name_p >>= \nes ->
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
  show = \(NameTypeAndValues ntavoles) ->
    "\n" ++ ntavoles-->map (show .> (++ "\n"))-->concat

name_type_and_value_expressions_p = 
  NameTypeAndValues <$> try name_type_and_value_or_lists_expression_p-->many1
  :: Parser NameTypeAndValueExpressions

-- IntermediatesOutputExpression

data IntermediatesOutputExpression =
  IntermediatesOutput NameTypeAndValueExpressions ValueExpression
  deriving ( Eq )

instance Show IntermediatesOutputExpression where
  show = \(IntermediatesOutput ntave ve) -> 
    "intermediates\n" ++ show ntave ++ "output\n" ++ show ve

intermediates_output_expression_p = 
  spaces_tabs >> string "intermediates" >> new_line_space_surrounded >>
  name_type_and_value_expressions_p >>= \ntave ->
  spaces_tabs >> string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \ve ->
  return $ IntermediatesOutput ntave ve
  :: Parser IntermediatesOutputExpression

-- NoAbstractionsValueExpression1

data NoAbstractionsValueExpression1 =
  SubtractionExp SubtractionExpression | MultiplicationExp MultiplicationExpression |
  OneArgApplicationsExp OneArgumentApplicationsExpression |
  HighPrecedenceExp HighPrecedenceExpression 
  deriving ( Eq )

instance Show NoAbstractionsValueExpression1 where
  show = \case
    SubtractionExp e -> show e
    MultiplicationExp e -> show e
    OneArgApplicationsExp e -> show e
    HighPrecedenceExp e -> show e

no_abstraction_expression1_p =
  SubtractionExp <$> try subtraction_expression_p <|>
  MultiplicationExp <$> try multiplication_expression_p <|>
  OneArgApplicationsExp <$> try one_arg_applications_expression_p <|>
  HighPrecedenceExp <$> high_precedence_expression_p
  :: Parser NoAbstractionsValueExpression1

-- ArgumentValueExpression

data ArgumentValueExpression =
  ArgumentValue AbstractionArgumentsExpression NoAbstractionsValueExpression1
  deriving ( Eq )

instance Show ArgumentValueExpression where
  show = \(ArgumentValue aae nave) -> show aae ++ show nave

argument_value_expression_p =
  try argument_value_expression2_p <|> argument_value_expression1_p
  :: Parser ArgumentValueExpression

argument_value_expression1_p =
  abstraction_arguments_expression_p >>= \aae ->
  no_abstraction_expression1_p >>= \nae1 ->
  return $ ArgumentValue aae nae1
  :: Parser ArgumentValueExpression

argument_value_expression2_p =
  comma_seperated2 abstraction_argument_expression_p >>= \aaes1 ->
  string " :> " >>
  argument_value_expression1_p >>= \(ArgumentValue (AbstractionArguments aaes2) nave) ->
  return $ ArgumentValue (AbstractionArguments $ aaes1 ++ aaes2) nave
  :: Parser ArgumentValueExpression

-- ManyArgumentsApplicationExpression

data ManyArgumentsApplicationExpression = 
  ManyArgumentsApplication [ ArgumentValueExpression ] ValueName
  deriving ( Eq, Show )

many_arguments_application_p =
  comma_seperated2 argument_value_expression_p >>= \aves ->
  string " :-> " >> value_name_p >>= \ne ->
  return $ ManyArgumentsApplication aves ne
  :: Parser ManyArgumentsApplicationExpression

-- NoAbstractionsValueExpression

data NoAbstractionsValueExpression =
  ManyArgsApplicationExp ManyArgumentsApplicationExpression |
  NoAbstractionsValueExp1 NoAbstractionsValueExpression1 |
  CasesExp CasesExpression |
  IntermediatesOutputExp IntermediatesOutputExpression
  deriving ( Eq )

instance Show NoAbstractionsValueExpression where
  show = \case
    ManyArgsApplicationExp e -> show e
    NoAbstractionsValueExp1 e -> show e
    CasesExp e -> show e
    IntermediatesOutputExp e -> show e

no_abstraction_expression_p =
  ManyArgsApplicationExp <$> try many_arguments_application_p <|>
  CasesExp <$> try cases_expression_p <|>
  IntermediatesOutputExp <$> try intermediates_output_expression_p <|>
  NoAbstractionsValueExp1 <$> try no_abstraction_expression1_p 
  :: Parser NoAbstractionsValueExpression

-- ValueExpression

data ValueExpression =
  Value AbstractionArgumentsExpression NoAbstractionsValueExpression
  deriving ( Eq )

instance Show ValueExpression where
  show = \(Value aae nave) -> show aae ++ show nave

value_expression_p = try value_expression2_p <|> value_expression1_p
  :: Parser ValueExpression

value_expression1_p =
  abstraction_arguments_expression_p >>= \aae ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value aae nae
  :: Parser ValueExpression

value_expression2_p =
  comma_seperated2 abstraction_argument_expression_p >>= \aaes1 ->
  string " :> " >> value_expression1_p >>= \(Value (AbstractionArguments aaes2) nave) ->
  return $ Value (AbstractionArguments $ aaes1 ++ aaes2) nave
  :: Parser ValueExpression
