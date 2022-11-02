{-# LANGUAGE LambdaCase #-}

module Parsers.Values where

import Prelude
  ( Show, (<$>), (<*), (*>), (++), ($), (>>=), (>>), (==), (&&), return, show
  , map, length, concat, error, fmap )
import Text.Parsec
  ( (<|>), try, char, many, many1, string, eof, skipMany1, parserFail, optional )
import Text.Parsec.String ( Parser )

import Helpers
  ( (-->), (.>), seperated2, comma_seperated2, spaces_tabs, new_line_space_surrounded )
import Parsers.LowLevel
  ( ValueName, LiteralOrValueName, ApplicationDirection, TupleMatching
  , Abstraction, Abstractions ( Abstractions ), ValueType
  , value_name_p, literal_or_value_name_p, application_direction_p, tuple_matching_p
  , abstraction_p, abstractions_p , value_type_p )

{- 
  All:
  ParenthesisValue, ParenLitOrName, OneArgFunctionApplications,
  MultiplicationFactor, Multiplication, SubtractionFactor, Subtraction,
  NoAbstractionsValue1, ManyArgsAppArgValue, ManyArgsApplication,
  SpecificCase, Cases,
  NameTypeAndValue, NameTypeAndValueLists,
  NTAVOrNTAVLists, NamesTypesAndValues,
  IntermediatesOutput,
  NoAbstractionsValue, Value
-}

-- ParenthesisValue

data ParenthesisValue = Parenthesis Value | Tuple [ Value ]

instance Show ParenthesisValue where
  show = \case
    Parenthesis v -> "(" ++ show v ++ ")"
    Tuple vs -> "Tuple " ++ show vs

[ parenthesis_value_p, tuple_internals_p, parenthesis_internals_p ] =
  [ char '(' *> (try tuple_internals_p <|> parenthesis_internals_p) <* char ')'
  , fmap Tuple $ char ' ' *> comma_seperated2 value_expression_p <* char ' '
  , Parenthesis <$> value_expression_p ]
  :: [ Parser ParenthesisValue ]

-- ParenLitOrName

data ParenLitOrName =
  ParenthesisValue ParenthesisValue | LiteralOrValueName LiteralOrValueName

instance Show ParenLitOrName where
  show = \case
    ParenthesisValue pv -> show pv
    LiteralOrValueName lovn -> show lovn

paren_lit_or_name_p =
  ParenthesisValue <$> parenthesis_value_p <|>
  LiteralOrValueName <$> literal_or_value_name_p
  :: Parser ParenLitOrName

-- OneArgFunctionApplications

data OneArgFunctionApplications = 
  OneArgFunctionApplications ParenLitOrName [ ( ApplicationDirection, ParenLitOrName ) ]

instance Show OneArgFunctionApplications where
  show = \(OneArgFunctionApplications plon ad_plon_s) -> case ad_plon_s of
    [] -> error "application expression should have at least one application direction"
    _ ->
      show plon ++
      ad_plon_s-->map ( \( ad, plon ) -> show ad ++ " " ++ show plon ++ " " )-->concat

one_arg_function_applications_p =
  paren_lit_or_name_p >>= \plon ->
  many1 ad_plon_p >>= \ad_plon_s -> 
  return $ OneArgFunctionApplications plon ad_plon_s
  :: Parser OneArgFunctionApplications

ad_plon_p = 
  application_direction_p >>= \ad ->
  paren_lit_or_name_p >>= \plon ->
  return ( ad, plon )
  :: Parser ( ApplicationDirection, ParenLitOrName )

-- MultiplicationFactor

data MultiplicationFactor =
  OneArgApplicationsMF OneArgFunctionApplications | ParenLitOrNameMF ParenLitOrName

instance Show MultiplicationFactor where
  show = \case
    OneArgApplicationsMF oafas -> show oafas
    ParenLitOrNameMF plon -> show plon

multiplication_factor_p =
  OneArgApplicationsMF <$> try one_arg_function_applications_p <|>
  ParenLitOrNameMF <$> paren_lit_or_name_p
  :: Parser MultiplicationFactor

-- Multiplication

data Multiplication = Multiplication [ MultiplicationFactor ]

instance Show Multiplication where
  show = \(Multiplication mfs) -> case mfs of
      [] -> error "found less than 2 mfs in multiplication"
      [ _ ] -> show (Multiplication [])
      [ mf1, mf2 ] -> "(" ++ show mf1 ++ " mul " ++ show mf2 ++ ")"
      (mf:mfs) -> "(" ++ show mf ++ " mul " ++ show (Multiplication mfs) ++ ")"

multiplication_p = Multiplication <$> seperated2 multiplication_factor_p " * "
  :: Parser Multiplication

-- SubtractionFactor

data SubtractionFactor =
  MultiplicationSF Multiplication | OneArgApplicationsSF OneArgFunctionApplications |
  ParenLitOrNameSF ParenLitOrName

instance Show SubtractionFactor where
  show = \case
    MultiplicationSF m -> show m
    OneArgApplicationsSF oafas -> show oafas
    ParenLitOrNameSF plon -> show plon

subtraction_factor_p =
  MultiplicationSF <$> try multiplication_p <|>
  OneArgApplicationsSF <$> try one_arg_function_applications_p <|>
  ParenLitOrNameSF <$> paren_lit_or_name_p 
  :: Parser SubtractionFactor

-- Subtraction

data Subtraction = Subtraction SubtractionFactor SubtractionFactor 

instance Show Subtraction where
  show = \(Subtraction sf1 sf2) -> "(" ++ show sf1 ++ " minus " ++ show sf2 ++ ")"

subtraction_p =
  subtraction_factor_p >>= \sf1 ->
  string " - " >> subtraction_factor_p >>= \sf2 ->
  return $ Subtraction sf1 sf2
  :: Parser Subtraction

-- NoAbstractionsValue1

data NoAbstractionsValue1 =
  Sub Subtraction | Mul Multiplication | OneArgApps OneArgFunctionApplications |
  PLON ParenLitOrName 

instance Show NoAbstractionsValue1 where
  show = \case
    Sub sub -> show sub
    Mul mul -> show mul
    OneArgApps oaas -> show oaas
    PLON plon -> show plon

no_abstractions_value_1_p =
  Sub <$> try subtraction_p <|> Mul <$> try multiplication_p <|>
  OneArgApps <$> try one_arg_function_applications_p <|> PLON <$> paren_lit_or_name_p
  :: Parser NoAbstractionsValue1

-- ManyArgsAppArgValue

data ManyArgsAppArgValue = ManyArgsAppArgValue Abstractions NoAbstractionsValue1

instance Show ManyArgsAppArgValue where
  show = \(ManyArgsAppArgValue as nav1) -> show as ++ show nav1

many_args_app_arg_value_p = try many_args_app_arg_value2_p <|> many_args_app_arg_value1_p
  :: Parser ManyArgsAppArgValue

many_args_app_arg_value1_p =
  abstractions_p >>= \as ->
  no_abstractions_value_1_p >>= \nae1 ->
  return $ ManyArgsAppArgValue as nae1
  :: Parser ManyArgsAppArgValue

many_args_app_arg_value2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >>
  many_args_app_arg_value1_p >>= \(ManyArgsAppArgValue (Abstractions as2) nav1) ->
  return $ ManyArgsAppArgValue (Abstractions $ as1 ++ as2) nav1
  :: Parser ManyArgsAppArgValue

-- ManyArgsApplication

data ManyArgsApplication = ManyArgsApplication [ ManyArgsAppArgValue ] ValueName
  deriving ( Show )

many_args_application_p =
  comma_seperated2 many_args_app_arg_value_p >>= \maaavs ->
  string " :-> " >> value_name_p >>= \vn ->
  return $ ManyArgsApplication maaavs vn
  :: Parser ManyArgsApplication

-- SpecificCase

data SpecificCase = SpecificCase LiteralOrValueName Value 
 
instance Show SpecificCase where
  show = \(SpecificCase lovn v) -> 
    "specific case: " ++ show lovn ++ "\n" ++
    "result: " ++ show v ++ "\n"

specific_case_p =
  spaces_tabs >> literal_or_value_name_p >>= \lovn ->
  string " ->" >> (char ' ' <|> char '\n') >> value_expression_p >>= \v ->
  return $ SpecificCase lovn v
  :: Parser SpecificCase

-- Cases

newtype Cases = Cases_ [ SpecificCase ]

instance Show Cases where
  show = \(Cases_ scs) ->
    ("\ncase start\n\n" ++) $ scs --> map (show .> (++ "\n")) --> concat

cases_p = fmap Cases_ $ string "cases\n" *> many1 (specific_case_p <* char '\n')
  :: Parser Cases

-- NameTypeAndValue

data NameTypeAndValue = NameTypeAndValue ValueName ValueType Value

instance Show NameTypeAndValue where
  show = \(NameTypeAndValue vn vt v) -> 
    "name: " ++ show vn ++ "\n" ++
    "type: " ++ show vt ++ "\n" ++
    "value: " ++ show v ++ "\n"

name_type_and_value_p =
  value_name_p >>= \vn ->
  string ": " >> value_type_p >>= \vt ->
  new_line_space_surrounded >> string "= " >> value_expression_p >>= \v ->
  (eof <|> skipMany1 new_line_space_surrounded) >> NameTypeAndValue vn vt v-->return
  :: Parser NameTypeAndValue

-- NameTypeAndValueLists

data NameTypeAndValueLists = NameTypeAndValueLists [ ValueName ] [ ValueType ] [ Value ]

instance Show NameTypeAndValueLists where
  show = \(NameTypeAndValueLists vns vts vs) -> 
    "names: " ++  vns --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "types: " ++ vts --> map (show .> (++ ", ")) --> concat ++ "\n" ++
    "values: " ++ vs --> map (show .> (++ ", ")) --> concat ++ "\n"

name_type_and_value_lists_p = 
  spaces_tabs >> comma_seperated2 value_name_p >>= \vns ->
  string ": " >> comma_seperated2 value_type_p >>= \vts ->
  new_line_space_surrounded >>
  string "= " >> comma_seperated2 value_expression_p >>= \vs ->
  (eof <|> skipMany1 new_line_space_surrounded) >>
  NameTypeAndValueLists vns vts vs-->return
  :: Parser NameTypeAndValueLists
  
-- NTAVOrNTAVLists

data NTAVOrNTAVLists = NTAV NameTypeAndValue | NTAVLists NameTypeAndValueLists

instance Show NTAVOrNTAVLists where
  show = \case
    NTAV ntav -> show ntav
    NTAVLists ntavl -> show ntavl

ntav_or_ntav_lists_p = 
  NTAVLists <$> try name_type_and_value_lists_p <|> NTAV <$> name_type_and_value_p
  :: Parser NTAVOrNTAVLists

-- NamesTypesAndValues

newtype NamesTypesAndValues = NamesTypesAndValues [ NTAVOrNTAVLists ]

instance Show NamesTypesAndValues where
  show = \(NamesTypesAndValues ns_ts_and_vs) ->
    "\n" ++ ns_ts_and_vs-->map (show .> (++ "\n"))-->concat

names_types_and_values_p = NamesTypesAndValues <$> try ntav_or_ntav_lists_p-->many1
  :: Parser NamesTypesAndValues

-- IntermediatesOutput

data IntermediatesOutput = IntermediatesOutput_ NamesTypesAndValues Value

instance Show IntermediatesOutput where
  show = \(IntermediatesOutput_ ns_ts_and_vs v) -> 
    "intermediates\n" ++ show ns_ts_and_vs ++ "output\n" ++ show v

intermediates_output_p = 
  spaces_tabs >> string "intermediates" >> new_line_space_surrounded >>
  names_types_and_values_p >>= \ns_ts_and_vs ->
  spaces_tabs >> string "output" >> new_line_space_surrounded >>
  value_expression_p >>= \v ->
  return $ IntermediatesOutput_ ns_ts_and_vs v
  :: Parser IntermediatesOutput

-- NoAbstractionsValue

data NoAbstractionsValue =
  ManyArgsApp ManyArgsApplication | Cases Cases |
  IntermediatesOutput IntermediatesOutput | NoAbstractionsValue1 NoAbstractionsValue1

instance Show NoAbstractionsValue where
  show = \case
    ManyArgsApp maa -> show maa
    Cases cs -> show cs
    IntermediatesOutput io -> show io
    NoAbstractionsValue1 nav1 -> show nav1

no_abstraction_expression_p =
  ManyArgsApp <$> try many_args_application_p <|> Cases <$> try cases_p <|>
  IntermediatesOutput <$> try intermediates_output_p <|>
  NoAbstractionsValue1 <$> no_abstractions_value_1_p 
  :: Parser NoAbstractionsValue

-- Value

data Value = Value Abstractions NoAbstractionsValue

instance Show Value where
  show = \(Value as nav) -> show as ++ show nav

value_expression_p = try value_expression2_p <|> value_expression1_p
  :: Parser Value

value_expression1_p =
  abstractions_p >>= \as ->
  no_abstraction_expression_p >>= \nae ->
  return $ Value as nae
  :: Parser Value

value_expression2_p =
  comma_seperated2 abstraction_p >>= \as1 ->
  string " :> " >> value_expression1_p >>= \(Value (Abstractions as2) nav) ->
  return $ Value (Abstractions $ as1 ++ as2) nav
  :: Parser Value
