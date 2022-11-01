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
  , ValueType, AbstractionArguments( AbstractionArguments ) )
import Parsers.ValueExpressions
  ( ParenthesisValue( Parenthesis, Tuple )
  , ParenLitOrName( ParenthesisValue, LiteralOrValueName )
  , OneArgFunctionApplications( OneArgFunctionApplications )
  , MultiplicationFactor( OneArgApplicationsMF, ParenLitOrNameMF )
  , Multiplication( Multiplication )
  , SubtractionFactor( MultiplicationSF, OneArgApplicationsSF, ParenLitOrNameSF )
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
  ( tuple_matching_g, value_name_g, value_type_g
  , literal_or_value_name_g, abstraction_arguments_g )

{- 
  All:
  ParenthesisValue, ParenLitOrName, ApplicationExpression,
  MultiplicationFactor, Multiplication,
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

-- ParenthesisValue

parenthesis_expression_g = ( ( \case
  Parenthesis ve -> value_expression_g ve
  Tuple ves -> 
    mapM value_expression_g ves >>= \l ->
    return $ " " ++ init l-->map (++ ", ")-->concat ++ l-->last ++ " "
  ) >=> ("(" ++) .> (++ ")") .> return
  ) :: ParenthesisValue -> IndentState HaskellSource

-- ParenLitOrName

high_precedence_expression_g = ( \case
  ParenthesisValue pe -> parenthesis_expression_g pe
  LiteralOrValueName ae -> return $ literal_or_value_name_g ae
  ) :: ParenLitOrName -> IndentState HaskellSource

-- OneArgFunctionApplications

one_arg_applications_expression_g = ( \(OneArgFunctionApplications init_hpe ad_hpe_s) ->
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
             [ ( ApplicationDirection, ParenLitOrName) ] ->
             IndentState HaskellSource
      in
      recursive_f (high_precedence_expression_g init_hpe) ad_hpe_s
  ) :: OneArgFunctionApplications -> IndentState HaskellSource

-- MultiplicationFactor

multiplication_factor_g = ( \case
  OneArgApplicationsMF ae -> one_arg_applications_expression_g ae
  ParenLitOrNameMF hpe -> high_precedence_expression_g hpe
  ) :: MultiplicationFactor -> IndentState HaskellSource

-- Multiplication

multiplication_expression_g = ( \(Multiplication mfs) -> 
  mapM multiplication_factor_g mfs >>= intercalate " * " .> return
  ) :: Multiplication -> IndentState HaskellSource

-- SubtractionFactor

subtraction_factor_g = ( \case
  MultiplicationSF me -> multiplication_expression_g me
  OneArgApplicationsSF ae -> one_arg_applications_expression_g ae
  ParenLitOrNameSF hpe -> high_precedence_expression_g hpe
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
  return $ indent i ++ literal_or_value_name_g ae ++ " -> " ++ ve_g
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
    indent i  ++ value_name_g ne ++ " = " ++
    value_begin ++ ve_g ++ value_end ++ "\n" ++
    indent (i + 1) ++ ":: " ++ value_type_g te ++ "\n"
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
    ) :: ( [ ValueName ], [ ValueType ], [ ValueExpression ] ) ->
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
  return $ value_name_g ne ++ ves_g-->map (" " ++)-->concat
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
