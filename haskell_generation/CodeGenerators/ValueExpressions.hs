{-# LANGUAGE LambdaCase #-}

module CodeGenerators.ValueExpressions where

import Prelude
  ( String, Int, (>>=), (>>), (-), (+), (*), (++), ($), undefined, map, concat, return
  , error, mapM, init, last )
import Data.List ( intercalate, replicate )
import Control.Monad.State ( (>=>) )
import Control.Monad.State ( State, get, put, modify )
import Helpers ( (-->), (.>) )

import Parsers.LowLevel
  ( ApplicationDirection( LeftApplication, RightApplication ), NameExpression
  , TypeExpression )
import Parsers.ValueExpressions
  ( ParenthesisExpression( ForPrecedence, Tuple )
  , HighPrecedenceExpression( Parenthesis, Atomic )
  , ApplicationExpression( Application )
  , MultiplicationFactor( ApplicationMF, HighPrecedenceMF )
  , MultiplicationExpression( Multiplication )
  , SubtractionFactor( MultiplicationSF, ApplicationSF, HighPrecedenceSF )
  , SubtractionExpression( Subtraction )
  , SpecificCaseExpression( SpecificCase ), CasesExpression( Cases )
  , NameTypeAndValueExpression( NameTypeAndValue )
  , NameTypeAndValueListsExpression( NameTypeAndValueLists )
  , NameTypeAndValueOrListsExpression( NameTypeAndValueExp, NameTypeAndValueListsExp )
  , NameTypeAndValueExpressions( NameTypeAndValues )
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
  ParenthesisExpression, HighPrecedenceExpression, ApplicationExpression,
  MultiplicationFactor, MultiplicationExpression,
  SubtractionFactor, SubtractionExpression,
  SpecificCaseExpression, CasesExpression,
  NameTypeAndValueExpression, NameTypeAndValueListsExpression,
  NameTypeAndValueOrListsExpression, NameTypeAndValueExpressions,
  IntermediatesOutputExpression,
  AbstractionArgument, NoAbstractionsValueExpression, ValueExpression
-}

type IndentState = State Int
type HaskellSource = String

indent = ( \i -> replicate (2 * i) ' ' )
  :: Int -> String

-- ParenthesisExpression

parenthesis_expression_g = ( ( \case
  ForPrecedence ve -> value_expression_g ve
  Tuple ves -> 
    mapM value_expression_g ves >>= \l ->
    return $ " " ++ init l-->map (++ ", ")-->concat ++ l-->last ++ " "
  ) >=> ("(" ++) .> (++ ")") .> return
  ) :: ParenthesisExpression -> IndentState HaskellSource

-- HighPrecedenceExpression

high_precedence_expression_g = ( \case
  Parenthesis pe -> parenthesis_expression_g pe
  Atomic ae -> return $ atomic_expression_g ae
  ) :: HighPrecedenceExpression -> IndentState HaskellSource

-- ApplicationExpression

application_direction_g = ( \generate_so_far -> \generate_hpe -> \ad ->
  generate_so_far >>= \sf ->
  generate_hpe >>= \hpe ->
  return $ case ad of
    LeftApplication -> sf ++ " " ++ hpe
    RightApplication -> hpe ++ " " ++ sf
  ) :: IndentState HaskellSource -> IndentState HaskellSource -> ApplicationDirection
         -> IndentState HaskellSource

application_expression_g = ( \(Application hpe_ad_s hpe) ->
  let
  generate_hpe_ad_s =
    map ( \( hpe, ad ) -> ( high_precedence_expression_g hpe, ad ) ) hpe_ad_s
    :: [ ( IndentState HaskellSource, ApplicationDirection ) ]
  application_expression_help_g = ( \case
    [] -> error "application expression should have at least one application direction"
    [ ( generate_so_far, ad ) ] ->
      application_direction_g generate_so_far (high_precedence_expression_g hpe) ad
    ( generate_so_far, ad1 ):( generate_hpe, ad2 ):the_rest ->
      let
      generate_so_far_next = application_direction_g generate_so_far generate_hpe ad1
        :: IndentState HaskellSource
      in
      application_expression_help_g $ ( generate_so_far_next, ad2 ):the_rest
    ) :: [ ( IndentState HaskellSource, ApplicationDirection ) ]
          -> IndentState HaskellSource
  in
  application_expression_help_g generate_hpe_ad_s
  ) :: ApplicationExpression -> IndentState HaskellSource

-- MultiplicationFactor

multiplication_factor_g = ( \case
  ApplicationMF ae -> application_expression_g ae
  HighPrecedenceMF hpe -> high_precedence_expression_g hpe
  ) :: MultiplicationFactor -> IndentState HaskellSource

-- MultiplicationExpression

multiplication_expression_g = ( \(Multiplication mfs) -> 
  mapM multiplication_factor_g mfs >>= intercalate " * " .> return
  ) :: MultiplicationExpression -> IndentState HaskellSource

-- SubtractionFactor

subtraction_factor_g = ( \case
  MultiplicationSF me -> multiplication_expression_g me
  ApplicationSF ae -> application_expression_g ae
  HighPrecedenceSF hpe -> high_precedence_expression_g hpe
  ) :: SubtractionFactor -> IndentState HaskellSource

-- SubtractionExpression

subtraction_expression_g = ( \(Subtraction sf1 sf2) ->
  subtraction_factor_g sf1 >>= \sf1_g ->
  subtraction_factor_g sf2 >>= \sf2_g ->
  return $ sf1_g ++ " - " ++ sf2_g
  ) :: SubtractionExpression -> IndentState HaskellSource

-- SpecificCaseExpression

specific_case_expression_g = ( \(SpecificCase ae ve) ->
  value_expression_g ve >>= \ve_g ->
  get >>= \i ->
  return $ indent i ++ atomic_expression_g ae ++ " -> " ++ ve_g
  ) :: SpecificCaseExpression -> IndentState HaskellSource

-- CasesExpression

cases_expression_g = ( \(Cases sces) ->
  get >>= \i ->
  put (i + 1) >>
  mapM specific_case_expression_g sces>>= \sces_g ->
  let
  hs_source =
    "\\case\n" ++ init sces_g-->map (++ "\n")-->(++ [last sces_g])-->concat
    :: HaskellSource
  in
  put i >> return hs_source
  ) :: CasesExpression -> IndentState HaskellSource

-- NameTypeAndValueExpression

name_type_and_value_expression_g = ( \(NameTypeAndValue ne te ve) -> 
  value_expression_g ve >>= \ve_g ->
  get >>= \i ->
  let
  combine value_begin value_end =
    indent i  ++ name_expression_g ne ++ " = " ++
    value_begin ++ ve_g ++ value_end ++ "\n" ++
    indent (i + 1) ++ ":: " ++ type_expression_g te ++ "\n"
  in
  return $ case ve of
    (Value [] nae) -> combine "" ""
    _ -> combine "( " " )"
  ) :: NameTypeAndValueExpression -> IndentState HaskellSource

-- NameTypeAndValueListsExpression

name_type_and_value_lists_expression_g = ( \(NameTypeAndValueLists nes tes ves) -> 
  let
  zip3 = ( \case
    ( ne:nes, te:tes, ve:ves) -> NameTypeAndValue ne te ve:zip3 ( nes, tes, ves )
    ( [], [], [] ) -> []
    _ ->
      error "name_type_and_value_lists_expression_g: lists must have the same length"
    ) :: ( [ NameExpression ], [ TypeExpression ], [ ValueExpression ] ) ->
           [ NameTypeAndValueExpression ]
  in
  zip3 ( nes, tes, ves )-->mapM name_type_and_value_expression_g >>= concat .> return
  ) :: NameTypeAndValueListsExpression -> IndentState HaskellSource

-- NameTypeAndValueOrListsExpression

name_type_and_value_or_lists_expression_g = ( \case 
  NameTypeAndValueExp ntave -> name_type_and_value_expression_g ntave
  NameTypeAndValueListsExp ntavle -> name_type_and_value_lists_expression_g ntavle
  ) :: NameTypeAndValueOrListsExpression -> IndentState HaskellSource

-- NameTypeAndValueExpressions

name_type_and_value_expressions_g = ( \(NameTypeAndValues ntaves) ->
  ntaves-->mapM name_type_and_value_or_lists_expression_g >>= concat .> return
  ) :: NameTypeAndValueExpressions -> IndentState HaskellSource

-- IntermediatesOutputExpression

intermediates_output_expression_g = ( \(IntermediatesOutputExpression ntaves ve) ->
  get >>= \i ->
  put (i + 1) >>
  name_type_and_value_expressions_g ntaves >>= \ntaves_g ->
  value_expression_g ve >>= \ve_g ->
  let
  hs_source =
    "\n" ++ indent (i + 1) ++ "let\n" ++ ntaves_g ++
    indent (i + 1) ++ "in\n" ++ indent (i + 1) ++ ve_g
    :: HaskellSource
  in
  put i >> return hs_source
  ) :: IntermediatesOutputExpression -> IndentState HaskellSource

-- AbstractionArgumentExpression

abstraction_argument_expression_g = ( \case
  Name n -> name_expression_g n
  TupleMatching tm -> tuple_matching_expression_g tm
  ) :: AbstractionArgumentExpression -> String

-- NoAbstractionsValueExpression

no_abstraction_expression_g = ( \case
  SubtractionExp se -> subtraction_expression_g se
  MultiplicationExp me -> multiplication_expression_g me
  ApplicationExp ae -> application_expression_g ae
  HighPrecedenceExp hpe -> high_precedence_expression_g hpe
  CasesExp ce -> cases_expression_g ce
  IntermediatesOutputExp ioe -> intermediates_output_expression_g ioe 
  ) :: NoAbstractionsValueExpression -> IndentState HaskellSource

-- ValueExpression

value_expression_g = ( \(Value aaes nae) ->
  let
  aaes_g =
    aaes-->map ( abstraction_argument_expression_g .> ("\\" ++) .> (++ " -> "))-->concat
    :: HaskellSource 
  in
  no_abstraction_expression_g nae >>= (aaes_g ++) .> return 
  ) :: ValueExpression -> IndentState HaskellSource
