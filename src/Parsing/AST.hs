{-
This file implements a parser for every type of the AST by implementing an
instance of the HasParser type class
-}

{-# language LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Parsing.AST where

import Prelude ((<$>), (<*), (*>), ($), (>>=), (>>), (>), (++), (+))
import Prelude qualified as P
import Control.Monad ((>=>))
import Text.Parsec ((<|>), (<?>))
import Text.Parsec qualified as TP

import ASTTypes qualified as T
import Helpers ((++<), (++++<), (+++<), (>++<),(>$>), (>:<))
import Helpers qualified as H

import Parsing.TypesAndHelpers qualified as PT

-- HasParser class + parse function

class HasParser a where
  parser :: PT.Parser a

parse :: HasParser a => P.String -> P.Either TP.ParseError a
parse = TP.runParser (parser <* TP.eof) (0, P.False) ""

-- HasParser instances
--   Literal

instance HasParser P.Integer where
  parser = P.read <$> TP.option "" (TP.string "-") >++< PT.digits

instance HasParser P.Double where
  parser =
    P.read <$> int_p >++< TP.string "." >++< PT.digits >++<
    TP.option "" exponent_p
    where
    int_p :: PT.Parser P.String
    int_p = P.show <$> (parser :: PT.Parser P.Integer)

    exponent_p :: PT.Parser P.String
    exponent_p = (TP.char 'e' <|> TP.char 'E') >:< int_p

instance HasParser P.Char where
  parser = PT.charLiteral

instance HasParser P.String where
  parser = PT.stringLiteral

instance HasParser T.Literal where
  parser =
    T.R <$> TP.try parser <|> T.Int <$> parser <|> T.Ch <$> parser <|>
    T.S <$> parser <?> "Literal"

--  Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

instance HasParser T.Identifier where
  parser =
    id_p >>= check_not_reserved
    where
    id_p :: PT.Parser T.Identifier
    id_p =
      TP.optionMaybe parser >>= \muip1 ->
      parser >>= \id_start ->
      TP.many (TP.try parser) >>= \id_conts ->
      TP.optionMaybe TP.digit >>= \mdigit ->
      TP.optionMaybe parser >>= \muip2 ->
      P.return $ T.Id (muip1, id_start, id_conts, mdigit, muip2)

    check_not_reserved :: T.Identifier -> PT.Parser T.Identifier
    check_not_reserved = \id -> case id of
      T.Id (P.Nothing, T.IS id_str, [], P.Nothing, P.Nothing) ->
        PT.err_if_reserved id_str >> P.return id
      _ -> P.return id

instance HasParser T.SimpleId where
  parser =
    sid_p >>= check_not_reserved
    where
    sid_p :: PT.Parser T.SimpleId
    sid_p = T.SId <$> parser ++< TP.optionMaybe TP.digit

    check_not_reserved :: T.SimpleId -> PT.Parser T.SimpleId
    check_not_reserved = \sid -> case sid of
      T.SId (T.IS id_str, P.Nothing) -> PT.err_if_reserved id_str >> P.return sid
      _ -> P.return sid

instance HasParser T.IdStart where
  parser = T.IS <$> TP.lower >:< TP.many PT.lower_under

instance HasParser T.IdCont where
  parser = T.IC <$> parser ++< TP.many1 PT.lower_under

instance HasParser T.UndersInParen where
  parser =
    (TP.string "(_" *> TP.many (PT.comma *> PT.underscore) <* TP.char ')') >$>
    \interm_unders -> T.UIP $ P.length interm_unders + 1

instance HasParser T.ParenExpr where
  parser = T.PE <$> PT.in_paren parser

instance HasParser T.InsideParenExpr where
  parser = T.LOE1 <$> TP.try parser <|> T.LFE1 <$> parser

instance HasParser T.Tuple where
  parser = T.T <$> PT.in_paren (parser ++< (PT.comma *> parser))

instance HasParser T.LineExprOrUnders where
  parser = T.LEOUs <$> parser ++< TP.many (PT.comma *> parser)

instance HasParser T.LineExprOrUnder where
  parser = T.LE1 <$> TP.try parser <|> PT.underscore *> P.return T.Underscore1

instance HasParser T.LineExpr where
  parser =
    T.LFE2 <$> TP.try parser <|> T.LOE2 <$> TP.try parser <|> T.BOAE1 <$> parser

instance HasParser T.BasicOrAppExpr where
  parser =
    T.PrFA1 <$> TP.try parser <|> T.PoFA1 <$> TP.try parser <|> T.BE3 <$> parser

instance HasParser T.BasicExpr where
  parser =
    T.PFAOI1 <$> TP.try parser <|> T.SI1 <$> TP.try parser <|>
    T.Lit1 <$> parser <|> T.T1 <$> parser <|> T.L1 <$> parser <?> expecting_msg
    where
    expecting_msg :: P.String
    expecting_msg =
      "Basic expression: Literal, Identifier, Tuple, List, " ++
      "Parenthesis Function Application, SpecialId"

instance HasParser T.BigTuple where
  parser =
    T.BT <$>
    leou_p ++< split_p +++< leous_p ++++< leous_l_p <* PT.nl_indent <*
    TP.char ')'
    where
    leou_p :: PT.Parser T.LineExprOrUnder
    leou_p = TP.char '(' *> PT.opt_space *> parser

    split_p :: PT.Parser T.BigTupleSplit
    split_p = TP.try PT.nl_indent *> P.return T.Split <|> P.return T.NoSplit

    leous_p :: PT.Parser T.LineExprOrUnders
    leous_p = PT.comma *> parser

    leous_l_p :: PT.Parser [T.LineExprOrUnders]
    leous_l_p = TP.many $ TP.try (PT.nl_indent *> PT.comma) *> parser

instance HasParser T.List where
  parser =
    T.L <$>
      (TP.char '[' *> PT.opt_space_around (TP.optionMaybe parser) <* TP.char ']')

instance HasParser T.BigList where
  parser =
    T.BL <$> leous_p ++< leous_l_p <* PT.nl_indent <* TP.char ']'
    where
    leous_p :: PT.Parser T.LineExprOrUnders
    leous_p = TP.char '[' *> PT.opt_space *> parser

    leous_l_p :: PT.Parser [T.LineExprOrUnders]
    leous_l_p = TP.many $ TP.try (PT.nl_indent *> PT.comma) *> parser

instance HasParser T.ArgsStr where
  parser = parser ++< TP.many1 PT.lower_under

instance HasParser T.ParenFuncAppOrId where
  parser =
    TP.optionMaybe parser >>= \margs1 ->
    parser >>= \id_start ->
    TP.many (TP.try parser) >>= \arg_str_pairs ->
    TP.optionMaybe TP.digit >>= \mdigit ->
    TP.optionMaybe parser >>= \margs2 ->
    P.return $ T.PFAOI (margs1, id_start, arg_str_pairs, mdigit, margs2)

instance HasParser T.Arguments where
  parser = T.As <$> PT.in_paren parser

--  PreFunc, PostFunc, BasicExpr, Change

instance HasParser T.PreFunc where
  parser = T.PF <$> parser <* TP.string "--"

instance HasParser T.PreFuncApp where
  parser = T.PrFA <$> parser ++< parser

instance HasParser T.PostFunc where
  parser =
    TP.try (TP.char '.' *> TP.notFollowedBy (TP.string "change{")) *>
    (T.SId1 <$> parser <|> T.SI2 <$> parser)

instance HasParser T.SpecialId where
  parser =
    TP.string "1st" *> P.return T.First <|>
    TP.string "2nd" *> P.return T.Second <|>
    TP.string "3rd" *> P.return T.Third <|>
    TP.string "4th" *> P.return T.Fourth <|>
    TP.string "5th" *> P.return T.Fifth

instance HasParser T.PostFuncApp where
  parser = T.PoFA <$> parser ++< parser

instance HasParser T.PostFuncArg where
  parser =
    T.PE2 <$> TP.try parser <|> T.BE2 <$> parser <|>
    PT.underscore *> P.return T.Underscore2

instance HasParser T.PostFuncAppEnd where
  parser =
    T.DC1 <$> parser <|> T.PFsMDC <$> TP.many1 parser ++< TP.optionMaybe parser

instance HasParser T.DotChange where
  parser =
    T.DC <$>
    ( TP.try (TP.string ".change{") *> PT.opt_space_around field_changes_p <*
      TP.char '}'
    )
    where
    field_changes_p :: PT.Parser (T.FieldChange, [T.FieldChange])
    field_changes_p = field_change_p ++< TP.many (PT.comma *> field_change_p)

    field_change_p :: PT.Parser T.FieldChange
    field_change_p = T.FC <$> field_p ++< (PT.equals *> parser)

    field_p :: PT.Parser T.Field
    field_p = T.SId2 <$> parser <|> T.SI3 <$> parser

--  OpExpr

instance HasParser T.OpExpr where
  parser = T.BOE1 <$> TP.try parser <|> T.LOE3 <$> parser

instance HasParser T.OpExprStart where
  parser = T.OES <$> TP.many1 (TP.try $ parser ++< parser)

instance HasParser T.LineOpExpr where
  parser = T.LOE <$> TP.try parser ++< parser

instance HasParser T.LineOpExprEnd where
  parser = T.LFE3 <$> TP.try parser <|> T.O1 <$> parser

instance HasParser T.BigOpExpr where
  parser = T.BOEOS1 <$> TP.try parser <|> T.BOEFS1 <$> parser

instance HasParser T.BigOpExprOpSplit where
  parser =
    T.BOEOS <$>
      parser >:<
      (PT.set_in_equal_line P.False *> TP.many (TP.try parser)) ++<
      TP.optionMaybe (TP.try parser) +++<
      parser

instance HasParser T.OpSplitLine where
  parser =
    ( T.OFCO1 <$> TP.try parser <|>
      T.OESMOFCO <$> (parser ++< mofco_parser)
    ) <* PT.indent
    where
    mofco_parser :: PT.Parser (P.Maybe T.OperFCO)
    mofco_parser = TP.try PT.nl *> P.return P.Nothing <|> P.Just <$> parser

instance HasParser T.OperFCO where
  parser = T.OFCO <$> parser ++< (TP.char ' ' *> parser <* TP.char '\n')

instance HasParser T.OpSplitEnd where
  parser = T.FE1 <$> TP.try parser <|> T.O2 <$> parser

instance HasParser T.BigOpExprFuncSplit where
  parser = T.BOEFS <$> parser ++< parser

instance HasParser T.BigOrCasesFuncExpr where
  parser = T.BFE1 <$> TP.try parser <|> T.CFE1 <$> parser

instance HasParser T.Operand where
  parser =
    T.BOAE2 <$> TP.try parser <|> T.PE3 <$> TP.try parser <|>
    PT.underscore *> P.return T.Underscore3

instance HasParser T.Op where
  parser =
    T.FCO3 <$> TP.try (TP.char ' ' *> parser <* TP.char ' ') <|>
    T.OSO <$> PT.opt_space_around parser

instance HasParser T.FuncCompOp where
  parser =
    TP.string "o>" *> P.return T.RightComp <|>
    TP.try (TP.string "<o") *> P.return T.LeftComp

instance HasParser T.OptionalSpacesOp where
  parser =
    TP.try (TP.string "->") *> P.return T.RightApp <|>
    TP.try (TP.string "<-") *> P.return T.LeftApp <|>
    TP.string "^" *> P.return T.Power <|>
    TP.string "*" *> P.return T.Mult <|>
    TP.string "/" *> P.return T.Div <|>
    TP.string "+" *> P.return T.Plus <|>
    TP.string "-" *> P.return T.Minus <|>
    TP.string "==" *> P.return T.Equal <|>
    TP.string "!=" *> P.return T.NotEqual <|>
    TP.try (TP.string ">=") *> P.return T.GrEq <|>
    TP.try (TP.string "<=") *> P.return T.LeEq <|>
    TP.try (TP.string ">>") *> P.return T.Use <|>
    TP.string ">" *> P.return T.Greater <|>
    TP.string "<" *> P.return T.Less <|>
    TP.string "&" *> P.return T.And <|>
    TP.string "|" *> P.return T.Or <|>
    TP.string ";" *> P.return T.Then

--  FuncExpr

instance HasParser T.FuncExpr where
  parser =
    T.CFE2 <$> TP.try parser <|> T.BFE2 <$> TP.try parser <|> T.LFE4 <$> parser

instance HasParser T.LineFuncExpr where
  parser = T.LFE <$> parser ++< (PT.func_arr *> parser)

instance HasParser T.BigFuncExpr where
  parser = T.BFE <$> parser ++< (PT.func_arr *> parser)

instance HasParser T.Parameters where
  parser =
    T.ParamId <$> TP.try parser <|> TP.char '*' *> P.return T.Star1 <|>
    T.Params <$> PT.in_paren (parser ++< TP.many1 (PT.comma *> parser))

instance HasParser T.LineFuncBody where
  parser =
    PT.opt_space *>
    ( T.LOE4 <$> TP.try parser <|> T.BOAE3 <$> TP.try parser <|>
      T.LFE5 <$> (TP.char '(' *> PT.opt_space_around parser <* TP.char ')')
    )

instance HasParser T.BigFuncBody where
  parser =
    PT.nl_indent *> PT.set_in_equal_line P.False *>
    ( T.OE1 <$> TP.try parser <|> T.BOAE4 <$> TP.try parser <|>
      T.LFE6 <$> (TP.char '(' *> PT.opt_space_around parser <* TP.char ')')
    )

instance HasParser T.CasesFuncExpr where
  parser =
    parser >>= \cases_params ->

    check_at_least_one_cases_keyword cases_params >>

    PT.deeper_if_not_in_equal_line all_cases_p >>= \(cases, maybe_end_case) ->
    P.return $ T.CFE (cases_params, cases, maybe_end_case)
    where
    all_cases_p :: PT.Parser ([T.Case], P.Maybe T.EndCase)
    all_cases_p =
      PT.set_in_equal_line P.False *> TP.many1 parser ++< TP.optionMaybe parser

    check_at_least_one_cases_keyword :: T.CasesParams -> PT.Parser ()
    check_at_least_one_cases_keyword =
      count_cases_keywords >=> \case
        0 -> TP.unexpected "0 cases keywords in cases func expr"
        n ->
          case n > 0 of
            P.True -> H.do_nothing
            P.False -> P.error "Should be impossible"

    count_cases_keywords :: T.CasesParams -> PT.Parser P.Int
    count_cases_keywords = \case
      T.CParamId _ -> P.return 0
      T.QuestionMark -> P.return 1
      T.Star2 -> P.return 0
      T.CParams (cp, cps) -> P.mapM count_cases_keywords (cp : cps) >$> P.sum

instance HasParser T.CasesParams where
  parser =
    TP.try (TP.char '?') *> P.return T.QuestionMark <|>
    TP.char '*' *> P.return T.Star2 <|> T.CParamId <$> TP.try parser <|>
    T.CParams <$> PT.in_paren (parser ++< TP.many1 (PT.comma *> parser))

instance HasParser T.Case where
  parser =
    T.Ca <$> TP.try (PT.nl_indent *> parser) ++<
    (PT.func_arr *> PT.deeper parser)

instance HasParser T.EndCase where
  parser =
    T.EC <$> TP.try (PT.nl_indent *> parser) ++<
    (PT.func_arr *> PT.deeper parser)

instance HasParser T.OuterMatching where
  parser = T.M1 <$> TP.try parser <|> T.SId3 <$> parser

instance HasParser T.EndCaseParam where
  parser = T.Id1 <$> parser <|> TP.string "..." *> P.return T.Ellipsis

instance HasParser T.Matching where
  parser =
    T.Lit2 <$> parser <|> T.PFM <$> TP.try (parser ++< parser) <|>
    T.TM1 <$> parser <|> T.LM1 <$> parser

instance HasParser T.InnerMatching where
  parser =
    T.M2 <$> TP.try parser <|> T.Id2 <$> parser <|>
    TP.char '*' *> P.return T.Star

instance HasParser T.TupleMatching where
  parser = T.TM <$> PT.in_paren (parser ++< TP.many1 (PT.comma *> parser))

instance HasParser T.ListMatching where
  parser =
    T.LM <$> (TP.char '[' *> PT.opt_space_around inside_list_p <* TP.char ']')
    where
    inside_list_p
      :: PT.Parser
           (P.Maybe
             (T.InnerMatching, [T.InnerMatching], P.Maybe T.RestListMatching)
           )
    inside_list_p = TP.optionMaybe $ parser ++< ims_p +++< TP.optionMaybe parser

    ims_p :: PT.Parser [T.InnerMatching]
    ims_p =
      TP.many $ TP.try $ PT.comma *> parser <*
      TP.notFollowedBy (PT.equals *> TP.string "...")

instance HasParser T.RestListMatching where
  parser =
    T.RLM <$> (PT.comma *> TP.optionMaybe (parser <* PT.equals) <*
    TP.string "...")

instance HasParser T.CaseBody where
  parser =
    T.BFB1 <$> TP.try parser ++< TP.optionMaybe (TP.try parser) <|>
    T.LFB1 <$> parser

--  ValueDef, WhereExpr

instance HasParser T.ValueDef where
  parser =
    PT.indent *> parser >>= \identifier ->

    PT.increase_il_by 1 >>

    PT.has_type_symbol *> parser >>= \type_ ->
    PT.nl_indent *> TP.string "= " *>

    PT.set_in_equal_line P.True >>
    PT.increase_il_by 1 >>

    parser >>= \value_expr ->

    PT.set_in_equal_line P.False >>

    TP.optionMaybe (TP.try parser) >>= \maybe_where_expr ->

    PT.decrease_il_by 2 >>

    P.return (T.VD (identifier, type_, value_expr, maybe_where_expr))

instance HasParser T.ValueExpr where
  parser =
    T.OE2 <$> TP.try parser <|> T.FE2 <$> TP.try parser <|>
    T.BT1 <$> TP.try parser <|> T.BL1 <$> TP.try parser <|>
    T.BOAE5 <$> parser <?> "value expression"

instance HasParser T.GroupedValueDefs where
  parser =
    PT.indent *> parser >>= \id ->
    TP.many1 (PT.comma *> parser) >>= \ids ->
    PT.deeper (types_p ++< equal_les_p +++< les_l_p) >>=
      \(ts, equal_les, les_l) ->
    P.return $ T.GVDs (id, ids, ts, equal_les, les_l)
    where
    types_p :: PT.Parser T.Types
    types_p = PT.has_type_symbol *> parser

    equal_les_p :: PT.Parser T.LineExprs
    equal_les_p = PT.nl_indent *> TP.string "= " *> parser

    les_l_p :: PT.Parser [T.LineExprs]
    les_l_p = TP.many $ TP.try (PT.nl_indent *> PT.comma) *> parser

instance HasParser T.Types where
  parser =
    T.Ts <$> (parser ++< TP.many1 (PT.comma *> parser)) <|>
    T.All <$> (TP.string "all " *> parser)

instance HasParser T.LineExprs where
  parser = T.LEs <$> parser ++< TP.many (PT.comma *> parser)

instance HasParser T.WhereExpr where
  parser =
    T.WE <$>
      (TP.try (PT.nl_indent *> TP.string "where") *> PT.nl *> where_def_expr_p)
      ++<
      TP.many (TP.try $ PT.nl *> PT.nl *> where_def_expr_p)
    where
    where_def_expr_p :: PT.Parser T.WhereDefExpr
    where_def_expr_p = T.VD1 <$> TP.try parser <|> T.GVDs1 <$> parser

--  Type

instance HasParser T.Type where
  parser = T.Ty <$> (TP.optionMaybe $ TP.try parser) ++< parser

instance HasParser T.SimpleType where
  parser =
    T.FT1 <$> TP.try parser <|> T.PT1 <$> TP.try parser <|>
    T.PoT1 <$> TP.try parser <|> T.PTV1 <$> TP.try parser <|>
    T.TAIOA1 <$> parser

instance HasParser T.TypeId where
  parser = T.TId <$> TP.upper >:< TP.many (TP.upper <|> TP.lower)

instance HasParser T.ParamTVar where
  parser = T.PTV <$> (TP.char 'T' *> H.mapf TP.digit (\d -> P.read [d]))

instance HasParser T.AdHocTVar where
  parser = T.AHTV <$> (TP.char '@' *> TP.upper)

instance HasParser T.TypeAppIdOrAHTV where
  parser =
    T.TAIOA <$> TP.optionMaybe parser ++< parser +++< TP.optionMaybe parser

instance HasParser T.TAIOAMiddle where
  parser =
    T.AHTV2 <$> parser <|>
    T.TIdStart1 <$> parser ++<
    TP.many (TP.try $ parser ++< TP.many1 (TP.lower <|> TP.upper))

instance HasParser T.TypesInParen where
  parser = T.TIP <$> PT.in_paren (parser ++< TP.many (PT.comma *> parser))

instance HasParser T.ProdType where
  parser = T.PT <$> parser ++< (TP.many1 $ TP.try $ TP.string " x " *> parser)

instance HasParser T.FieldType where
  parser = T.PoT2 <$> TP.try parser <|> T.PBT1 <$> parser

instance HasParser T.PowerBaseType where
  parser =
    T.PTV2 <$> TP.try parser <|> T.TAIOA2 <$> TP.try parser <|> T.IPT <$> parser

instance HasParser T.InParenT where
  parser =
    PT.in_paren $
      T.FT3 <$> TP.try parser <|> T.PT3 <$> TP.try parser <|>
      T.PoT3 <$> TP.try parser

instance HasParser T.PowerType where
  parser =
    T.PoT <$> parser ++< (TP.string "^" *> parser >>= PT.err_if_less_than_2)

instance HasParser T.FuncType where
  parser = T.FT <$> parser ++< (TP.string " => " *> parser)

instance HasParser T.InOrOutType where
  parser =
    T.PT2 <$> TP.try parser <|> T.FT2 <$> TP.try (PT.in_paren parser) <|>
    T.PoT4 <$> TP.try parser <|> T.PTV3 <$> TP.try parser <|> T.TAIOA3 <$> parser

instance HasParser T.Condition where
  parser = T.Co <$> (parser <* TP.string " --> ")

--  TypeDef, TypeNickname

instance HasParser T.TypeDef where
  parser = T.TTD1 <$> parser <|> T.OTD1 <$> parser

instance HasParser T.TupleTypeDef where
  parser =
    T.TTD <$>
      (TP.try (TP.string "tuple type:") *> PT.opt_space *> parser)
      ++<
      (PT.opt_space_around (TP.char '=') *> parser)
      +++<
      (PT.nl *> TP.string "field names:" *> PT.space_or_nl *> parser)

instance HasParser T.ProdOrPowerType where
  parser = T.PT4 <$> TP.try parser <|> T.PoT5 <$> parser

instance HasParser T.TypeName where
  parser =
    T.TN <$>
      TP.optionMaybe parser ++<
      parser +++<
      TP.many (TP.try $ parser ++< (TP.many1 $ TP.lower <|> TP.upper)) ++++<
      TP.optionMaybe parser

instance HasParser T.ParamVarsInParen where
  parser = T.PVIP <$> PT.in_paren (parser ++< TP.many (PT.comma *> parser))

instance HasParser T.FieldNames where
  parser = T.PCSIs <$> PT.in_paren (parser ++< TP.many1 (PT.comma *> parser))

instance HasParser T.OrTypeDef where
  parser =
    T.OTD <$>
      (TP.try (TP.string "or type:") *> PT.opt_space *> parser) ++<
      (PT.nl *> TP.string "values:" *> PT.space_or_nl *> parser) +++<
      TP.many (PT.opt_space_around (TP.string "|") *> parser)

instance HasParser T.PossibleValue where
  parser =
    T.PV <$>
      parser ++<
      ( TP.optionMaybe $
        (TP.string "--<" *> parser <* PT.opt_space_around (TP.char ':')) ++<
        (parser <* TP.char '>')
      )

instance HasParser T.TypeNickname where
  parser =
    T.TNN <$>
      (TP.try (TP.string "type nickname:") *> PT.opt_space *> parser) ++<
      (PT.equals *> parser)

--  TypePropDef

instance HasParser T.TypePropDef where
  parser = T.APD1 <$> TP.try parser <|> T.RPD1 <$> parser

instance HasParser T.AtomPropDef where
  parser =
    T.APD <$>
      parser ++<
      (PT.nl *> TP.string "needed" *> PT.space_or_nl *> parser) +++<
      (PT.opt_space_around (TP.string ":") *> parser)

instance HasParser T.RenamingPropDef where
  parser =
    T.RPD <$>
      parser ++<
      (PT.nl *> TP.string "equivalent" *> PT.space_or_nl *> parser) +++<
      TP.many (PT.comma *> parser)

instance HasParser T.PropNameLine where
  parser =
    T.PNL <$> (TP.try (TP.string "type proposition:") *> PT.opt_space *> parser)

instance HasParser T.PropName where
  parser =
    T.NPStart1 <$> np_start_p <|> T.TIPStart <$> tip_start_p
    where
    np_start_p
      :: PT.Parser (P.Char, [(T.NamePart, T.TypesInParen)], P.Maybe T.NamePart)
    np_start_p =
      TP.upper ++< TP.many1 (TP.try $ parser ++< parser) +++<
      TP.optionMaybe parser

    tip_start_p
      :: PT.Parser ([(T.TypesInParen, T.NamePart)], P.Maybe T.TypesInParen)
    tip_start_p =
      (TP.many1 $ TP.try $ parser ++< parser) ++< TP.optionMaybe parser

instance HasParser T.NamePart where
  parser =
    T.NP <$> P.concat <$> TP.many1 (lower_or_upper <|> under_upper)
    where
    lower_or_upper :: PT.Parser P.String
    lower_or_upper = P.fmap (:[]) (TP.lower <|> TP.upper)

    under_upper :: PT.Parser P.String
    under_upper = PT.underscore >:< P.fmap (:[]) TP.upper

--  TypeTheo

instance HasParser T.TypeTheo where
  parser =
    T.TT <$> pnws_p ++< mpnws_p +++< proof_p
    where
    pnws_p :: PT.Parser [T.PropNameWithSubs]
    pnws_p = TP.try (TP.string "type_theorem ") *> parser >$> (\x -> [x])

    mpnws_p :: PT.Parser (P.Maybe T.PropNameWithSubs)
    mpnws_p = TP.optionMaybe (TP.string " --> " *> parser)

    proof_p :: PT.Parser T.Proof
    proof_p = PT.nl *> TP.string "proof" *> parser

instance HasParser T.PropNameWithSubs where
  parser =
    T.NPStart2 <$> np_start_p <|> T.SIPStart <$> sip_start_p
    where
    np_start_p
      :: PT.Parser (P.Char, [(T.NamePart, T.SubsInParen)], P.Maybe T.NamePart)
    np_start_p =
      TP.upper ++< TP.many1 (TP.try $ parser ++< parser) +++<
      TP.optionMaybe parser

    sip_start_p
      :: PT.Parser ([(T.SubsInParen, T.NamePart)], P.Maybe T.SubsInParen)
    sip_start_p =
      (TP.many1 $ TP.try $ parser ++< parser) ++< TP.optionMaybe parser

instance HasParser T.SubsInParen where
  parser = T.SIP <$> PT.in_paren (parser ++< TP.many (PT.comma *> parser))

instance HasParser T.TVarSub where
  parser =
    T.FTS1 <$> TP.try parser <|> T.PTS1 <$> TP.try parser <|>
    T.PoTS1 <$> TP.try parser <|> T.PTV4 <$> TP.try parser <|>
    T.TAIOAS1 <$> parser

instance HasParser T.TypeAppIdOrAHTVSub where
  parser =
    T.TAIOAS <$> TP.optionMaybe parser ++< parser +++< TP.optionMaybe parser

instance HasParser T.TAIOASMiddle where
  parser =
    T.AHTV3 <$> parser <|>
    T.TIdStart2 <$> parser ++<
    TP.many (TP.try $ parser ++< TP.many1 (TP.lower <|> TP.upper))

instance HasParser T.SubsOrUndersInParen where
  parser = T.SOUIP <$> PT.in_paren (parser ++< TP.many (PT.comma *> parser))

instance HasParser T.SubOrUnder where
  parser = T.TVS1 <$> TP.try parser <|> PT.underscore *> P.return T.Underscore4

instance HasParser T.PowerTypeSub where
  parser =
    T.PoTS <$> parser ++< (TP.string "^" *> parser >>= PT.err_if_less_than_2)

instance HasParser T.PowerBaseTypeSub where
  parser =
    T.IPTS <$> TP.try (PT.in_paren parser) <|> T.TAIOAS2 <$> TP.try parser <|>
    T.PTV5 <$> parser <|> PT.underscore *> P.return T.Underscore5

instance HasParser T.InParenTSub where
  parser = T.FTS2 <$> TP.try parser <|> T.PTS2 <$> parser

instance HasParser T.ProdTypeSub where
  parser = T.PTS <$> parser ++< (TP.many1 $ TP.try (TP.string " x ") *> parser)

instance HasParser T.FieldTypeSub where
  parser = T.PoTS2 <$> TP.try parser <|> T.PBTS1 <$> parser

instance HasParser T.FuncTypeSub where
  parser = T.FTS <$> parser ++< (TP.string " => " *> parser)

instance HasParser T.InOrOutTypeSub where
  parser =
    T.FTS3 <$> TP.try (PT.in_paren parser) <|> T.PTS3 <$> TP.try parser <|>
    T.PoTS3 <$> TP.try parser <|> T.TAIOAS3 <$> TP.try parser <|>
    T.PTV6 <$> parser <|> PT.underscore *> P.return T.Underscore6

instance HasParser T.Proof where
  parser =
    T.P1 <$> (TP.char ' ' *> parser) ++< (TP.char ' ' *> parser) <|>
    T.P2 <$> (PT.nl *> TP.string "  " *> parser) ++< parser

instance HasParser T.IdOrOpEq where
  parser =
    T.IOOE <$> parser ++< TP.optionMaybe (TP.try $ parser ++< parser) <*
    TP.string " ="

instance HasParser T.TTValueExpr where
  parser =
    T.VEMWE <$> vemwe_p <|> T.LE2 <$> (TP.char ' ' *> parser)
    where
    vemwe_p :: PT.Parser (T.ValueExpr, P.Maybe T.WhereExpr)
    vemwe_p =
      PT.twice_deeper (TP.try PT.nl_indent *> parser ++< TP.optionMaybe parser)

--  Program

instance HasParser T.Program where
  parser =
    T.P <$>
      (TP.many PT.nl *> parser) ++<
      TP.many (TP.try $ PT.nl *> PT.nl *> parser) <* TP.spaces <* TP.eof

instance HasParser T.ProgramPart where
  parser =
    T.TD <$> parser <|> T.TNN1 <$> parser <|> T.TT1 <$> parser <|>
    T.TPD <$> parser <|> T.GVDs2 <$> TP.try parser <|> T.VD2 <$> parser

