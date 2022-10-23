{-# LANGUAGE LambdaCase #-}

module CodeGenerators.ValueExpressions where

import Prelude ( String, (++), ($), undefined, map, concat, foldl, error )
import Data.List ( intercalate )

import Helpers ( (-->), (.>), parenthesis_comma_sep_g )

import Parsers.LowLevel ( ApplicationDirection( LeftApplication, RightApplication ) )
import Parsers.ValueExpressions
  ( ParenthesisExpression( ForPrecedence, Tuple )
  , HighPrecedenceExpression( Parenthesis, Atomic )
  , ApplicationExpression( Application )
  , MultiplicationFactor( ApplicationMF, HighPrecedenceMF )
  , MultiplicationExpression( Multiplication )
  , SubtractionFactor( MultiplicationSF, ApplicationSF, HighPrecedenceSF )
  , SubtractionExpression( Subtraction )
  , SpecificCaseExpression( SpecificCase )
  , CasesExpression( Cases )
  , NameTypeAndValueExpression( NameTypeAndValue )
  , NameTypeAndValueExpressions( NameTypeAndValueExps )
  , IntermediatesOutputExpression( IntermediatesOutputExpression )
  , AbstractionArgumentExpression( Name, TupleMatching )
  , NoAbstractionsValueExpression
    ( SubtractionExp, MultiplicationExp, ApplicationExp, HighPrecedenceExp, CasesExp
    , IntermediatesOutputExp )
  , ValueExpression(Value)
  )

import CodeGenerators.LowLevel
  ( tuple_matching_expression_g, name_expression_g, type_expression_g
  , atomic_expression_g )

{- 
All:
ParenthesisExpression, HighPrecedenceExpression, ApplicationExpression
MultiplicationFactor, MultiplicationExpression, SubtractionFactor, SubtractionExpression
SpecificCaseExpression, CasesExpression
NameTypeAndValueExpression, NameTypeAndValueExpressions, IntermediatesOutputExpression
AbstractionArgument, NoAbstractionsValueExpression, ValueExpression
-}

type HaskellSource = String

-- ParenthesisExpression

parenthesis_expression_g = ( \case
  ForPrecedence ve -> "(" ++ value_expression_g ve ++ ")"
  Tuple ves -> parenthesis_comma_sep_g value_expression_g ves
  ) :: ParenthesisExpression -> HaskellSource

-- HighPrecedenceExpression

high_precedence_expression_g = ( \case
  Parenthesis pe -> parenthesis_expression_g pe
  Atomic ae -> atomic_expression_g ae
  ) :: HighPrecedenceExpression -> HaskellSource

-- ApplicationExpression

application_direction_g = ( \generated_so_far -> \generated_hpe -> \case 
  LeftApplication -> generated_so_far ++ " " ++ generated_hpe
  RightApplication -> generated_hpe ++ " " ++ generated_so_far
  ) :: HaskellSource -> HaskellSource -> ApplicationDirection -> HaskellSource

application_expression_g = ( \(Application hpe_ad_s hpe) ->
  let
  hpe_generated_ad_s =
    map ( \( hpe, ad ) -> ( high_precedence_expression_g hpe, ad ) ) hpe_ad_s
    :: [ ( HaskellSource, ApplicationDirection ) ]
  application_expression_help_g = ( \case
    [] -> error "application expression should have at least one application direction"
    [ ( generated_so_far, ad ) ] ->
      application_direction_g generated_so_far (high_precedence_expression_g hpe) ad
    ( generated_so_far, ad1 ):( hpe_generated, ad2 ):the_rest ->
      let
      generated_so_far_next = application_direction_g generated_so_far hpe_generated ad1
        :: HaskellSource
      in
      application_expression_help_g $ ( generated_so_far_next, ad2 ):the_rest
    ) :: [ ( HaskellSource, ApplicationDirection ) ] -> HaskellSource
  in
  application_expression_help_g hpe_generated_ad_s
  ) :: ApplicationExpression -> HaskellSource

-- MultiplicationFactor

multiplication_factor_g = ( \case
    ApplicationMF ae -> application_expression_g ae
    HighPrecedenceMF hpe -> high_precedence_expression_g hpe
  ) :: MultiplicationFactor -> HaskellSource

-- MultiplicationExpression

multiplication_expression_g = ( \(Multiplication mfs) -> 
  map multiplication_factor_g mfs-->intercalate " * "
  ) :: MultiplicationExpression -> HaskellSource

-- SubtractionFactor

subtraction_factor_g = ( \case
  MultiplicationSF me -> multiplication_expression_g me
  ApplicationSF ae -> application_expression_g ae
  HighPrecedenceSF hpe -> high_precedence_expression_g hpe
  ) :: SubtractionFactor -> HaskellSource

-- SubtractionExpression

subtraction_expression_g = ( \(Subtraction sf1 sf2) ->
  subtraction_factor_g sf1 ++ " - " ++ subtraction_factor_g sf2
  ) :: SubtractionExpression -> HaskellSource

-- SpecificCaseExpression

specific_case_expression_g = ( \(SpecificCase ae ve) ->
  "\t" ++ atomic_expression_g ae -->
    (++ " -> ") --> (++ value_expression_g ve) --> (++ "\n")
  ) :: SpecificCaseExpression -> HaskellSource

-- CasesExpression

cases_expression_g = ( \(Cases sces) ->
  "\\case\n" ++ sces --> map specific_case_expression_g --> concat
  ) :: CasesExpression -> HaskellSource

-- NameTypeAndValueExpression

name_type_and_value_expression_g = ( \(NameTypeAndValue ne te ve) -> 
  let
  combine value_begin type_begin =
    name_expression_g ne ++ " = " ++
    value_begin ++ value_expression_g ve ++ "\n" ++
    type_begin ++ type_expression_g te ++ "\n\n"
  in
  case ve of
    (Value [] nae) -> combine "" "\t:: "
    _ -> combine "( " "\t) :: "
  ) :: NameTypeAndValueExpression -> HaskellSource

-- NameTypeAndValueExpressions

name_type_and_value_expressions_g = ( \(NameTypeAndValueExps ntaves) ->
  map name_type_and_value_expression_g ntaves-->concat
  ) :: NameTypeAndValueExpressions -> HaskellSource

ntaves_in_let_g = ( \(NameTypeAndValueExps ntaves) ->
  map (name_type_and_value_expression_g .> ("\t\t" ++)) ntaves-->concat
  ) :: NameTypeAndValueExpressions -> HaskellSource

-- IntermediatesOutputExpression

intermediates_output_expression_g = ( \(IntermediatesOutputExpression ntaves ve) ->
  "\n\t\tlet\n" ++ ntaves_in_let_g ntaves ++
  "\n\t\tin\n" ++ value_expression_g ve
  ) :: IntermediatesOutputExpression -> HaskellSource

-- AbstractionArgumentExpression

abstraction_argument_expression_g = ( \case
  Name n -> name_expression_g n
  TupleMatching tm -> tuple_matching_expression_g tm
  ) :: AbstractionArgumentExpression -> HaskellSource

-- NoAbstractionsValueExpression

no_abstraction_expression_g = ( \case
    SubtractionExp se -> subtraction_expression_g se
    MultiplicationExp me -> multiplication_expression_g me
    ApplicationExp ae -> application_expression_g ae
    HighPrecedenceExp hpe -> high_precedence_expression_g hpe
    CasesExp ce -> cases_expression_g ce
    IntermediatesOutputExp ioe -> intermediates_output_expression_g ioe 
  ) :: NoAbstractionsValueExpression -> HaskellSource

-- ValueExpression

value_expression_g = ( \(Value aaes nae) ->
  aaes-->map ( abstraction_argument_expression_g .> ("\\" ++) .> (++ " -> "))-->concat
  ++ no_abstraction_expression_g nae
  ) :: ValueExpression -> HaskellSource
