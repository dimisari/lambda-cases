{-# LANGUAGE LambdaCase #-}

module CodeGenerators.ValueExpressions where

import Prelude
  ( String, Int, Bool( True ), (>>=), (>>), (-), (+), (*), (++), ($), undefined, map
  , concat, return, error, mapM, init, last )
import Data.List ( intercalate, replicate )
import Control.Monad.State ( (>=>) )
import Control.Monad.State ( State, get, put, modify )
import Helpers ( (-->), (.>) )

import Parsers.LowLevel
  ( ApplicationDirection( LeftApplication, RightApplication ), ValueName
  , TypeExpression, AbstractionArgumentsExpression( AbstractionArguments ) )
import Parsers.ValueExpressions
  ( ParenthesisExpression( ForPrecedence, Tuple )
  , HighPrecedenceExpression( Parenthesis, Atomic )
  , OneArgumentApplicationsExpression( OneArgumentApplications )
  , MultiplicationFactor( OneArgApplicationMF, HighPrecedenceMF )
  , MultiplicationExpression( Multiplication )
  , SubtractionFactor( MultiplicationSF, OneArgApplicationSF, HighPrecedenceSF )
  , SubtractionExpression( Subtraction )
  , SpecificCaseExpression( SpecificCase ), CasesExpression( Cases )
  , NameTypeAndValueExpression( NameTypeAndValue )
  , NameTypeAndValueListsExpression( NameTypeAndValueLists )
  , NameTypeAndValueOrListsExpression( NameTypeAndValueExp, NameTypeAndValueListsExp )
  , NameTypeAndValueExpressions( NameTypeAndValues )
  , IntermediatesOutputExpression( IntermediatesOutput )
  , NoAbstractionsValueExpression1
    ( SubtractionExp, MultiplicationExp, OneArgApplicationsExp
    , HighPrecedenceExp )
  , ArgumentValueExpression( ArgumentValue )
  , ManyArgumentsApplicationExpression( ManyArgumentsApplication )
  , NoAbstractionsValueExpression
    ( ManyArgsApplicationExp, NoAbstractionsValueExp1, CasesExp, IntermediatesOutputExp )
  , ValueExpression(Value)
  )

import CodeGenerators.LowLevel
  ( tuple_matching_expression_g, name_expression_g, type_expression_g
  , atomic_expression_g, abstraction_arguments_g )

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

-- OneArgumentApplicationsExpression

one_arg_applications_expression_g = ( \(OneArgumentApplications init_hpe ad_hpe_s) ->
  case ad_hpe_s of
    [] -> error "application expression should have at least one application direction"
    _ -> 
      let
      recursive_f = ( \generate_so_far -> \case 
        ( ad, hpe ):rest ->
          generate_so_far >>= \sf ->
          high_precedence_expression_g hpe >>= \hpe_g ->
          let
            combine =
              return $ case ad of
                LeftApplication -> sf ++ " " ++ hpe_g
                RightApplication -> hpe_g ++ " " ++ sf
              :: IndentState HaskellSource
          in
          recursive_f combine rest
        [] -> generate_so_far
        ) :: IndentState HaskellSource ->
             [ ( ApplicationDirection, HighPrecedenceExpression) ] ->
             IndentState HaskellSource
      in
      recursive_f (high_precedence_expression_g init_hpe) ad_hpe_s
  ) :: OneArgumentApplicationsExpression -> IndentState HaskellSource

-- MultiplicationFactor

multiplication_factor_g = ( \case
  OneArgApplicationMF ae -> one_arg_applications_expression_g ae
  HighPrecedenceMF hpe -> high_precedence_expression_g hpe
  ) :: MultiplicationFactor -> IndentState HaskellSource

-- MultiplicationExpression

multiplication_expression_g = ( \(Multiplication mfs) -> 
  mapM multiplication_factor_g mfs >>= intercalate " * " .> return
  ) :: MultiplicationExpression -> IndentState HaskellSource

-- SubtractionFactor

subtraction_factor_g = ( \case
  MultiplicationSF me -> multiplication_expression_g me
  OneArgApplicationSF ae -> one_arg_applications_expression_g ae
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
    (Value (AbstractionArguments []) nae) -> combine "" ""
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
    ) :: ( [ ValueName ], [ TypeExpression ], [ ValueExpression ] ) ->
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

intermediates_output_expression_g = ( \(IntermediatesOutput ntaves ve) ->
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

-- NoAbstractionsValueExpression1

no_abstractions_value_expression1_g = ( \case
  SubtractionExp se -> subtraction_expression_g se
  MultiplicationExp me -> multiplication_expression_g me
  OneArgApplicationsExp ae -> one_arg_applications_expression_g ae
  HighPrecedenceExp hpe -> high_precedence_expression_g hpe
  ) :: NoAbstractionsValueExpression1 -> IndentState HaskellSource

-- ArgumentValueExpression

argument_value_expression_g = ( \(ArgumentValue aae nave1) -> 
  no_abstractions_value_expression1_g nave1 >>= \nave1_g ->
  return $ abstraction_arguments_g aae ++ nave1_g
  ) :: ArgumentValueExpression -> IndentState HaskellSource

-- ManyArgumentsApplicationExpression

many_arguments_application_g = ( \(ManyArgumentsApplication ves ne) -> 
  let
  argument_value_expression_help_g = \case 
    (ArgumentValue (AbstractionArguments []) (HighPrecedenceExp hpe)) ->
      high_precedence_expression_g hpe
    ve -> argument_value_expression_g ve >>= \ve_g -> return $ "(" ++ ve_g ++ ")"
    :: ArgumentValueExpression -> IndentState HaskellSource
  in
  mapM argument_value_expression_help_g ves >>= \ves_g ->
  return $ name_expression_g ne ++ ves_g-->map (" " ++)-->concat
  ) :: ManyArgumentsApplicationExpression -> IndentState HaskellSource

-- NoAbstractionsValueExpression

no_abstractions_value_expression_g = ( \case
  ManyArgsApplicationExp e -> many_arguments_application_g e
  NoAbstractionsValueExp1 e -> no_abstractions_value_expression1_g e
  CasesExp ce -> cases_expression_g ce
  IntermediatesOutputExp ioe -> intermediates_output_expression_g ioe 
  ) :: NoAbstractionsValueExpression -> IndentState HaskellSource

-- ValueExpression

value_expression_g = ( \(Value aae nave) ->
  no_abstractions_value_expression_g nave >>= \nave_g ->
  return $ abstraction_arguments_g aae ++ nave_g
  ) :: ValueExpression -> IndentState HaskellSource
