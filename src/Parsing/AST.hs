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

import Parsing.TypesAndClasses qualified as PTC
import Parsing.Helpers qualified as PH

-- HasParser class + parse function

parse :: PTC.HasParser a => P.String -> P.Either TP.ParseError a
parse = TP.runParser (PTC.parser <* TP.eof) (0, P.False) ""

-- HasParser instances
--   Literal

instance PTC.HasParser P.Integer where
  parser = P.read <$> TP.option "" (TP.string "-") >++< PH.digits

instance PTC.HasParser P.Double where
  parser =
    P.read <$> int_p >++< TP.string "." >++< PH.digits >++<
    TP.option "" exponent_p
    where
    int_p :: PTC.Parser P.String
    int_p = P.show <$> (PTC.parser :: PTC.Parser P.Integer)

    exponent_p :: PTC.Parser P.String
    exponent_p = (TP.char 'e' <|> TP.char 'E') >:< int_p

instance PTC.HasParser P.Char where
  parser = PH.charLiteral

instance PTC.HasParser P.String where
  parser = PH.stringLiteral

instance PTC.HasParser T.Literal where
  parser =
    T.R <$> TP.try PTC.parser <|> T.Int <$> PTC.parser <|>
    T.Ch <$> PTC.parser <|> T.S <$> PTC.parser <?> "Literal"

--  Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

instance PTC.HasParser T.Identifier where
  parser =
    id_p >>= check_not_reserved
    where
    id_p :: PTC.Parser T.Identifier
    id_p =
      TP.optionMaybe PTC.parser >>= \muip1 ->
      PTC.parser >>= \id_start ->
      TP.many (TP.try PTC.parser) >>= \id_conts ->
      TP.optionMaybe TP.digit >>= \mdigit ->
      TP.optionMaybe PTC.parser >>= \muip2 ->
      P.return $ T.Id (muip1, id_start, id_conts, mdigit, muip2)

    check_not_reserved :: T.Identifier -> PTC.Parser T.Identifier
    check_not_reserved = \id -> case id of
      T.Id (P.Nothing, T.IS id_str, [], P.Nothing, P.Nothing) ->
        PH.err_if_reserved id_str >> P.return id
      _ -> P.return id

instance PTC.HasParser T.SimpleId where
  parser =
    sid_p >>= check_not_reserved
    where
    sid_p :: PTC.Parser T.SimpleId
    sid_p = T.SId <$> PTC.parser ++< TP.optionMaybe TP.digit

    check_not_reserved :: T.SimpleId -> PTC.Parser T.SimpleId
    check_not_reserved = \sid -> case sid of
      T.SId (T.IS id_str, P.Nothing) -> PH.err_if_reserved id_str >> P.return sid
      _ -> P.return sid

instance PTC.HasParser T.IdStart where
  parser = T.IS <$> TP.lower >:< TP.many PH.lower_under

instance PTC.HasParser T.IdCont where
  parser = T.IC <$> PTC.parser ++< TP.many1 PH.lower_under

instance PTC.HasParser T.UndersInParen where
  parser =
    (TP.string "(_" *> TP.many (PH.comma *> PH.underscore) <* TP.char ')') >$>
    \interm_unders -> T.UIP $ P.length interm_unders + 1

instance PTC.HasParser T.ParenExpr where
  parser = T.PE <$> PH.in_paren PTC.parser

instance PTC.HasParser T.InsideParenExpr where
  parser = T.LOE1 <$> TP.try PTC.parser <|> T.LFE1 <$> PTC.parser

instance PTC.HasParser T.Tuple where
  parser = T.T <$> PH.in_paren (PTC.parser ++< (PH.comma *> PTC.parser))

instance PTC.HasParser T.LineExprOrUnders where
  parser = T.LEOUs <$> PTC.parser ++< TP.many (PH.comma *> PTC.parser)

instance PTC.HasParser T.LineExprOrUnder where
  parser =
    T.LE1 <$> TP.try PTC.parser <|> PH.underscore *> P.return T.Underscore1

instance PTC.HasParser T.LineExpr where
  parser =
    T.LFE2 <$> TP.try PTC.parser <|> T.LOE2 <$> TP.try PTC.parser <|>
    T.BOAE1 <$> PTC.parser

instance PTC.HasParser T.BasicOrAppExpr where
  parser =
    T.PrFA1 <$> TP.try PTC.parser <|> T.PoFA1 <$> TP.try PTC.parser <|>
    T.BE3 <$> PTC.parser

instance PTC.HasParser T.BasicExpr where
  parser =
    T.PFAOI1 <$> TP.try PTC.parser <|> T.SI1 <$> TP.try PTC.parser <|>
    T.Lit1 <$> PTC.parser <|> T.T1 <$> PTC.parser <|>
    T.L1 <$> PTC.parser <?> expecting_msg
    where
    expecting_msg :: P.String
    expecting_msg =
      "Basic expression: Literal, Identifier, Tuple, List, " ++
      "Parenthesis Function Application, SpecialId"

instance PTC.HasParser T.BigTuple where
  parser =
    T.BT <$>
    leou_p ++< split_p +++< leous_p ++++< leous_l_p <* PH.nl_indent <*
    TP.char ')'
    where
    leou_p :: PTC.Parser T.LineExprOrUnder
    leou_p = TP.char '(' *> PH.opt_space *> PTC.parser

    split_p :: PTC.Parser T.BigTupleSplit
    split_p = TP.try PH.nl_indent *> P.return T.Split <|> P.return T.NoSplit

    leous_p :: PTC.Parser T.LineExprOrUnders
    leous_p = PH.comma *> PTC.parser

    leous_l_p :: PTC.Parser [T.LineExprOrUnders]
    leous_l_p = TP.many $ TP.try (PH.nl_indent *> PH.comma) *> PTC.parser

instance PTC.HasParser T.List where
  parser =
    T.L <$>
      ( TP.char '[' *> PH.opt_space_around (TP.optionMaybe PTC.parser) <*
        TP.char ']'
      )

instance PTC.HasParser T.BigList where
  parser =
    T.BL <$> leous_p ++< leous_l_p <* PH.nl_indent <* TP.char ']'
    where
    leous_p :: PTC.Parser T.LineExprOrUnders
    leous_p = TP.char '[' *> PH.opt_space *> PTC.parser

    leous_l_p :: PTC.Parser [T.LineExprOrUnders]
    leous_l_p = TP.many $ TP.try (PH.nl_indent *> PH.comma) *> PTC.parser

instance PTC.HasParser T.ArgsStr where
  parser = PTC.parser ++< TP.many1 PH.lower_under

instance PTC.HasParser T.ParenFuncAppOrId where
  parser =
    TP.optionMaybe PTC.parser >>= \margs1 ->
    PTC.parser >>= \id_start ->
    TP.many (TP.try PTC.parser) >>= \arg_str_pairs ->
    TP.optionMaybe TP.digit >>= \mdigit ->
    TP.optionMaybe PTC.parser >>= \margs2 ->
    P.return $ T.PFAOI (margs1, id_start, arg_str_pairs, mdigit, margs2)

instance PTC.HasParser T.Arguments where
  parser = T.As <$> PH.in_paren PTC.parser

--  PreFunc, PostFunc, BasicExpr, Change

instance PTC.HasParser T.PreFunc where
  parser = T.PF <$> PTC.parser <* TP.string "--"

instance PTC.HasParser T.PreFuncApp where
  parser = T.PrFA <$> PTC.parser ++< PTC.parser

instance PTC.HasParser T.PostFunc where
  parser =
    P.fmap T.PoF $
      TP.try (TP.char '.' *> TP.notFollowedBy (TP.string "change{")) *>
      PTC.parser

instance PTC.HasParser T.IdOrSpecialId where
  parser =
    T.Id1 <$> PTC.parser <|> T.SI2 <$> PTC.parser

instance PTC.HasParser T.SpecialId where
  parser =
    TP.string "1st" *> P.return T.First <|>
    TP.string "2nd" *> P.return T.Second <|>
    TP.string "3rd" *> P.return T.Third <|>
    TP.string "4th" *> P.return T.Fourth <|>
    TP.string "5th" *> P.return T.Fifth

instance PTC.HasParser T.PostFuncApp where
  parser = T.PoFA <$> PTC.parser ++< PTC.parser

instance PTC.HasParser T.PostFuncArg where
  parser =
    T.PE2 <$> TP.try PTC.parser <|> T.BE2 <$> PTC.parser <|>
    PH.underscore *> P.return T.Underscore2

instance PTC.HasParser T.PostFuncAppEnd where
  parser =
    T.DC1 <$> PTC.parser <|>
    T.PFsMDC <$> TP.many1 PTC.parser ++< TP.optionMaybe PTC.parser

instance PTC.HasParser T.DotChange where
  parser =
    T.DC <$>
    ( TP.try (TP.string ".change{") *> PH.opt_space_around field_changes_p <*
      TP.char '}'
    )
    where
    field_changes_p :: PTC.Parser (T.FieldChange, [T.FieldChange])
    field_changes_p = field_change_p ++< TP.many (PH.comma *> field_change_p)

    field_change_p :: PTC.Parser T.FieldChange
    field_change_p = T.FC <$> PTC.parser ++< (PH.equals *> PTC.parser)

--  OpExpr

instance PTC.HasParser T.OpExpr where
  parser = T.BOE1 <$> TP.try PTC.parser <|> T.LOE3 <$> PTC.parser

instance PTC.HasParser T.OpExprStart where
  parser = T.OES <$> TP.many1 (TP.try $ PTC.parser ++< PTC.parser)

instance PTC.HasParser T.LineOpExpr where
  parser = T.LOE <$> TP.try PTC.parser ++< PTC.parser

instance PTC.HasParser T.LineOpExprEnd where
  parser = T.LFE3 <$> TP.try PTC.parser <|> T.O1 <$> PTC.parser

instance PTC.HasParser T.BigOpExpr where
  parser = T.BOEOS1 <$> TP.try PTC.parser <|> T.BOEFS1 <$> PTC.parser

instance PTC.HasParser T.BigOpExprOpSplit where
  parser =
    T.BOEOS <$>
      PTC.parser >:<
      (PH.set_in_equal_line P.False *> TP.many (TP.try PTC.parser)) ++<
      TP.optionMaybe (TP.try PTC.parser) +++<
      PTC.parser

instance PTC.HasParser T.OpSplitLine where
  parser =
    ( T.OFCO1 <$> TP.try PTC.parser <|>
      T.OESMOFCO <$> (PTC.parser ++< mofco_parser)
    ) <* PH.indent
    where
    mofco_parser :: PTC.Parser (P.Maybe T.OperFCO)
    mofco_parser = TP.try PH.nl *> P.return P.Nothing <|> P.Just <$> PTC.parser

instance PTC.HasParser T.OperFCO where
  parser = T.OFCO <$> PTC.parser ++< (TP.char ' ' *> PTC.parser <* TP.char '\n')

instance PTC.HasParser T.OpSplitEnd where
  parser = T.FE1 <$> TP.try PTC.parser <|> T.O2 <$> PTC.parser

instance PTC.HasParser T.BigOpExprFuncSplit where
  parser = T.BOEFS <$> PTC.parser ++< PTC.parser

instance PTC.HasParser T.BigOrCasesFuncExpr where
  parser = T.BFE1 <$> TP.try PTC.parser <|> T.CFE1 <$> PTC.parser

instance PTC.HasParser T.Operand where
  parser =
    T.BOAE2 <$> TP.try PTC.parser <|> T.PE3 <$> TP.try PTC.parser <|>
    PH.underscore *> P.return T.Underscore3

instance PTC.HasParser T.Op where
  parser =
    T.FCO3 <$> TP.try (TP.char ' ' *> PTC.parser <* TP.char ' ') <|>
    T.OSO <$> PH.opt_space_around PTC.parser

instance PTC.HasParser T.FuncCompOp where
  parser =
    TP.string "o>" *> P.return T.RightComp <|>
    TP.try (TP.string "<o") *> P.return T.LeftComp

instance PTC.HasParser T.OptionalSpacesOp where
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

instance PTC.HasParser T.FuncExpr where
  parser =
    T.CFE2 <$> TP.try PTC.parser <|> T.BFE2 <$> TP.try PTC.parser <|>
    T.LFE4 <$> PTC.parser

instance PTC.HasParser T.LineFuncExpr where
  parser = T.LFE <$> PTC.parser ++< (PH.func_arr *> PTC.parser)

instance PTC.HasParser T.BigFuncExpr where
  parser = T.BFE <$> PTC.parser ++< (PH.func_arr *> PTC.parser)

instance PTC.HasParser T.Parameters where
  parser =
    T.ParamId <$> TP.try PTC.parser <|> TP.char '*' *> P.return T.Star1 <|>
    T.Params <$> PH.in_paren (PTC.parser ++< TP.many1 (PH.comma *> PTC.parser))

instance PTC.HasParser T.LineFuncBody where
  parser =
    T.LOE4 <$> TP.try PTC.parser <|> T.BOAE3 <$> TP.try PTC.parser <|>
    T.PLFE1 <$> PTC.parser

instance PTC.HasParser T.ParenLineFuncExpr where
  parser = PH.in_paren (PH.opt_space_around PTC.parser)

instance PTC.HasParser T.BigFuncBody where
  parser =
    PH.nl_indent *> PH.set_in_equal_line P.False *>
    ( T.OE1 <$> TP.try PTC.parser <|> T.BOAE4 <$> TP.try PTC.parser <|>
      T.PLFE2 <$> PTC.parser
    )

instance PTC.HasParser T.CasesFuncExpr where
  parser =
    PTC.parser >>= \cases_params ->

    check_at_least_one_cases_keyword cases_params >>

    PH.deeper_if_not_in_equal_line all_cases_p >>= \(cases, maybe_end_case) ->
    P.return $ T.CFE (cases_params, cases, maybe_end_case)
    where
    all_cases_p :: PTC.Parser ([T.Case], P.Maybe T.EndCase)
    all_cases_p =
      PH.set_in_equal_line P.False *> TP.many1 PTC.parser ++<
      TP.optionMaybe PTC.parser

    check_at_least_one_cases_keyword :: T.CasesParams -> PTC.Parser ()
    check_at_least_one_cases_keyword =
      count_cases_keywords >=> \case
        0 -> TP.unexpected "0 cases keywords in cases func expr"
        n ->
          case n > 0 of
            P.True -> H.do_nothing
            P.False -> P.error "Should be impossible"

    count_cases_keywords :: T.CasesParams -> PTC.Parser P.Int
    count_cases_keywords = \case
      T.CParamId _ -> P.return 0
      T.QuestionMark -> P.return 1
      T.Star2 -> P.return 0
      T.CParams (cp, cps) -> P.mapM count_cases_keywords (cp : cps) >$> P.sum

instance PTC.HasParser T.CasesParams where
  parser =
    TP.try (TP.char '?') *> P.return T.QuestionMark <|>
    TP.char '*' *> P.return T.Star2 <|> T.CParamId <$> TP.try PTC.parser <|>
    T.CParams <$> PH.in_paren (PTC.parser ++< TP.many1 (PH.comma *> PTC.parser))

instance PTC.HasParser T.Case where
  parser =
    T.Ca <$> TP.try (PH.nl_indent *> PTC.parser) ++<
    (PH.func_arr *> PH.deeper PTC.parser)

instance PTC.HasParser T.EndCase where
  parser =
    T.EC <$> TP.try (PH.nl_indent *> PTC.parser) ++<
    (PH.func_arr *> PH.deeper PTC.parser)

instance PTC.HasParser T.OuterMatching where
  parser = T.M1 <$> TP.try PTC.parser <|> T.SId3 <$> PTC.parser

instance PTC.HasParser T.EndCaseParam where
  parser = T.Id2 <$> PTC.parser <|> TP.string "..." *> P.return T.Ellipsis

instance PTC.HasParser T.Matching where
  parser =
    T.Lit2 <$> PTC.parser <|> T.PFM <$> TP.try (PTC.parser ++< PTC.parser) <|>
    T.TM1 <$> PTC.parser <|> T.LM1 <$> PTC.parser

instance PTC.HasParser T.InnerMatching where
  parser =
    T.M2 <$> TP.try PTC.parser <|> T.Id3 <$> PTC.parser <|>
    TP.char '*' *> P.return T.Star

instance PTC.HasParser T.TupleMatching where
  parser =
    T.TM <$> PH.in_paren (PTC.parser ++< TP.many1 (PH.comma *> PTC.parser))

instance PTC.HasParser T.ListMatching where
  parser =
    T.LM <$> (TP.char '[' *> PH.opt_space_around inside_list_p <* TP.char ']')
    where
    inside_list_p
      :: PTC.Parser
           (P.Maybe
             (T.InnerMatching, [T.InnerMatching], P.Maybe T.RestListMatching)
           )
    inside_list_p =
      TP.optionMaybe $ PTC.parser ++< ims_p +++< TP.optionMaybe PTC.parser

    ims_p :: PTC.Parser [T.InnerMatching]
    ims_p =
      TP.many $ TP.try $ PH.comma *> PTC.parser <*
      TP.notFollowedBy (PH.equals *> TP.string "...")

instance PTC.HasParser T.RestListMatching where
  parser =
    T.RLM <$> (PH.comma *> TP.optionMaybe (PTC.parser <* PH.equals) <*
    TP.string "...")

instance PTC.HasParser T.CaseBody where
  parser =
    T.BFB1 <$> TP.try PTC.parser ++< TP.optionMaybe (TP.try PTC.parser) <|>
    T.LFB1 <$> PTC.parser

--  ValueDef, WhereExpr

instance PTC.HasParser T.ValueDef where
  parser =
    PH.indent *> PTC.parser >>= \identifier ->

    PH.increase_il_by 1 >>

    PH.has_type *> PTC.parser >>= \type_ ->

    TP.optionMaybe PTC.parser >>= \maybe_ve ->

    PH.decrease_il_by 1 >>

    P.pure (T.VD (identifier, type_, maybe_ve))

instance PTC.HasParser T.ValueEquals where
  parser =
    TP.try (PH.nl_indent *> TP.string "= ") *>

    PH.set_in_equal_line P.True >>
    PH.increase_il_by 1 >>

    PTC.parser >>= \value_expr ->

    PH.set_in_equal_line P.False >>

    TP.optionMaybe (TP.try PTC.parser) >>= \maybe_we ->

    PH.decrease_il_by 1 >>

    P.pure (T.VE (value_expr, maybe_we))

instance PTC.HasParser T.ValueExpr where
  parser =
    T.OE2 <$> TP.try PTC.parser <|> T.FE2 <$> TP.try PTC.parser <|>
    T.BT1 <$> TP.try PTC.parser <|> T.BL1 <$> TP.try PTC.parser <|>
    T.BOAE5 <$> PTC.parser <?> "value expression"

instance PTC.HasParser T.GroupedValueDefs where
  parser =
    PH.indent *> PTC.parser >>= \ids ->
    PH.deeper (types_p ++< equal_les_p +++< les_l_p) >>=
      \(ts, equal_les, les_l) ->
    P.return $ T.GVDs (ids, ts, equal_les, les_l)
    where
    types_p :: PTC.Parser T.Types
    types_p = PH.has_type *> PTC.parser

    equal_les_p :: PTC.Parser T.LineExprs
    equal_les_p = PH.nl_indent *> TP.string "= " *> PTC.parser

    les_l_p :: PTC.Parser [T.LineExprs]
    les_l_p = TP.many $ TP.try (PH.nl_indent *> PH.comma) *> PTC.parser

instance PTC.HasParser T.Types where
  parser =
    T.Ts <$> (PTC.parser ++< TP.many1 (PH.comma *> PTC.parser)) <|>
    T.All <$> (TP.string "all " *> PTC.parser)

instance PTC.HasParser T.LineExprs where
  parser = T.LEs <$> PTC.parser ++< TP.many (PH.comma *> PTC.parser)

instance PTC.HasParser T.WhereExpr where
  parser =
    T.WE <$>
      (TP.try (PH.nl_indent *> TP.string "where") *> PH.nl *> where_def_expr_p)
      ++<
      TP.many (TP.try $ PH.nl *> PH.nl *> where_def_expr_p)
    where
    where_def_expr_p :: PTC.Parser T.WhereDefExpr
    where_def_expr_p = T.VD1 <$> TP.try PTC.parser <|> T.GVDs1 <$> PTC.parser

--  Type

instance PTC.HasParser T.Type where
  parser = T.Ty <$> (TP.optionMaybe $ TP.try PTC.parser) ++< PTC.parser

instance PTC.HasParser T.SimpleType where
  parser =
    T.FT1 <$> TP.try PTC.parser <|> T.POPT1 <$> TP.try PTC.parser <|>
    T.TAIOA1 <$> PTC.parser

instance PTC.HasParser T.ProdOrPowerType where
  parser = T.PT4 <$> TP.try PTC.parser <|> T.PoT5 <$> PTC.parser

instance PTC.HasParser T.TypeId where
  parser = T.TId <$> TP.upper >:< TP.many (TP.upper <|> TP.lower)

instance PTC.HasParser T.ParamTVar where
  parser = T.PTV <$> (TP.char 'T' *> H.mapf TP.digit (\d -> P.read [d]))

instance PTC.HasParser T.AdHocTVar where
  parser = T.AHTV <$> (TP.char '@' *> TP.upper)

instance PTC.HasParser T.TypeAppIdOrTV where
  parser =
    T.PTV1 <$> TP.try PTC.parser <|>
    T.TAIOA <$> TP.optionMaybe PTC.parser ++< PTC.parser +++<
    TP.optionMaybe PTC.parser

instance PTC.HasParser T.TAIOAMiddle where
  parser =
    T.AHTV1 <$> PTC.parser <|>
    T.TIdStart1 <$> PTC.parser ++<
    TP.many (TP.try $ PTC.parser ++< TP.many1 (TP.lower <|> TP.upper))

instance PTC.HasParser T.TypesInParen where
  parser =
    T.TIP <$> PH.in_paren (PTC.parser ++< TP.many (PH.comma *> PTC.parser))

instance PTC.HasParser T.ProdType where
  parser =
    T.PT <$> PTC.parser ++< (TP.many1 $ TP.try $ TP.string " x " *> PTC.parser)

instance PTC.HasParser T.FieldType where
  parser = T.PoT2 <$> TP.try PTC.parser <|> T.PBT1 <$> PTC.parser

instance PTC.HasParser T.PowerBaseType where
  parser =
    T.TAIOA2 <$> TP.try PTC.parser <|> T.IPT <$> PTC.parser

instance PTC.HasParser T.InParenT where
  parser =
    PH.in_paren $
      T.FT3 <$> TP.try PTC.parser <|> T.PT3 <$> TP.try PTC.parser <|>
      T.PoT3 <$> TP.try PTC.parser

instance PTC.HasParser T.PowerType where
  parser =
    T.PoT <$>
      PTC.parser ++< (TP.string "^" *> PTC.parser >>= PH.err_if_less_than_2)

instance PTC.HasParser T.FuncType where
  parser = T.FT <$> PTC.parser ++< (TP.string " => " *> PTC.parser)

instance PTC.HasParser T.InOrOutType where
  parser =
    T.POPT2 <$> TP.try PTC.parser <|>
    T.FT2 <$> TP.try (PH.in_paren PTC.parser) <|>
    T.TAIOA3 <$> PTC.parser

instance PTC.HasParser T.Condition where
  parser = T.Co <$> (PTC.parser <* TP.string " --> ")

--  TypeDef, TypeNickname

instance PTC.HasParser T.TypeDef where
  parser = T.TTD1 <$> PTC.parser <|> T.OTD1 <$> PTC.parser

instance PTC.HasParser T.TupleTypeDef where
  parser =
    T.TTD <$>
      (TP.try (TP.string "tuple type:") *> PH.opt_space *> PTC.parser)
      ++<
      (PH.equals *> PTC.parser)
      +++<
      (PH.nl *> TP.string "field names:" *> PH.space_or_nl *> PTC.parser)

instance PTC.HasParser T.TypeName where
  parser =
    T.TN <$>
      TP.optionMaybe PTC.parser ++<
      PTC.parser +++<
      TP.many (TP.try $ PTC.parser ++< (TP.many1 $ TP.lower <|> TP.upper)) ++++<
      TP.optionMaybe PTC.parser

instance PTC.HasParser T.ParamVarsInParen where
  parser =
    T.PVIP <$> PH.in_paren (PTC.parser ++< TP.many (PH.comma *> PTC.parser))

instance PTC.HasParser T.FieldNames where
  parser = T.PCSIs <$> PH.in_paren PTC.parser

instance PTC.HasParser T.Identifiers where
  parser = T.Ids <$> PTC.parser ++< TP.many1 (PH.comma *> PTC.parser)

instance PTC.HasParser T.OrTypeDef where
  parser =
    T.OTD <$>
      (TP.try (TP.string "or type:") *> PH.opt_space *> PTC.parser) ++<
      (PH.nl *> TP.string "values:" *> PTC.parser)

instance PTC.HasParser T.OrTypeValues where
  parser =
    T.VL <$> TP.try (PH.opt_space *> PTC.parser) <|> T.Ls <$> PTC.parser

instance PTC.HasParser T.OrTypeValuesLine where
  parser =
    T.OTVL <$>
      PTC.parser ++<
      TP.many (TP.try $ PH.opt_space_around (TP.char '|') *> PTC.parser)

instance PTC.HasParser T.OrTypeValuesLines where
  parser =
    T.OTVLs <$>
      (PH.nl_d_sp *> PTC.parser) ++<
      TP.many (PH.opt_space *> TP.char '|' *> PH.nl_d_sp *> PTC.parser)

instance PTC.HasParser T.OrTypeValue where
  parser = T.OTV <$> PTC.parser ++< TP.optionMaybe PTC.parser

instance PTC.HasParser T.InternalValue where
  parser =
    T.IV <$> (TP.string "--<" *> PH.opt_space_around pair_p <* TP.char '>')
    where
    pair_p :: PTC.Parser (T.Identifier, T.SimpleType)
    pair_p = (PTC.parser <* PH.opt_space_around (TP.char ':')) ++< PTC.parser

instance PTC.HasParser T.TypeNickname where
  parser =
    T.TNN <$>
      (TP.try (TP.string "type nickname:") *> PH.opt_space *> PTC.parser) ++<
      (PH.equals *> PTC.parser)

--  TypePropDef

instance PTC.HasParser T.TypePropDef where
  parser = T.APD1 <$> TP.try PTC.parser <|> T.RPD1 <$> PTC.parser

instance PTC.HasParser T.AtomPropDef where
  parser =
    T.APD <$>
      PTC.parser ++<
      (PH.nl *> TP.string "needed" *> PH.space_or_nl *> PTC.parser) +++<
      (PH.opt_space_around (TP.string ":") *> PTC.parser)

instance PTC.HasParser T.RenamingPropDef where
  parser =
    T.RPD <$>
      PTC.parser ++<
      (PH.nl *> TP.string "equivalent" *> PH.space_or_nl *> PTC.parser) +++<
      TP.many (PH.comma *> PTC.parser)

instance PTC.HasParser T.PropNameLine where
  parser =
    T.PNL <$>
      (TP.try (TP.string "type proposition:") *> PH.opt_space *> PTC.parser)

instance PTC.HasParser T.PropName where
  parser =
    T.NPStart1 <$> np_start_p <|> T.TIPStart <$> tip_start_p
    where
    np_start_p
      :: PTC.Parser (P.Char, [(T.NamePart, T.TypesInParen)], P.Maybe T.NamePart)
    np_start_p =
      TP.upper ++< TP.many1 (TP.try $ PTC.parser ++< PTC.parser) +++<
      TP.optionMaybe PTC.parser

    tip_start_p
      :: PTC.Parser ([(T.TypesInParen, T.NamePart)], P.Maybe T.TypesInParen)
    tip_start_p =
      (TP.many1 $ TP.try $ PTC.parser ++< PTC.parser) ++< TP.optionMaybe PTC.parser

instance PTC.HasParser T.NamePart where
  parser =
    T.NP <$> P.concat <$> TP.many1 (lower_or_upper <|> under_upper)
    where
    lower_or_upper :: PTC.Parser P.String
    lower_or_upper = P.fmap (:[]) (TP.lower <|> TP.upper)

    under_upper :: PTC.Parser P.String
    under_upper = PH.underscore >:< P.fmap (:[]) TP.upper

--  TypeTheo

instance PTC.HasParser T.TypeTheo where
  parser =
    T.TT <$> pnws_p ++< mpnws_p +++< proof_p
    where
    pnws_p :: PTC.Parser [T.PropNameWithSubs]
    pnws_p = TP.try (TP.string "type_theorem ") *> PTC.parser >$> (\x -> [x])

    mpnws_p :: PTC.Parser (P.Maybe T.PropNameWithSubs)
    mpnws_p = TP.optionMaybe (TP.string " --> " *> PTC.parser)

    proof_p :: PTC.Parser T.Proof
    proof_p = PH.nl *> TP.string "proof" *> PTC.parser

instance PTC.HasParser T.PropNameWithSubs where
  parser =
    T.NPStart2 <$> np_start_p <|> T.SIPStart <$> sip_start_p
    where
    np_start_p
      :: PTC.Parser (P.Char, [(T.NamePart, T.SubsInParen)], P.Maybe T.NamePart)
    np_start_p =
      TP.upper ++< TP.many1 (TP.try $ PTC.parser ++< PTC.parser) +++<
      TP.optionMaybe PTC.parser

    sip_start_p
      :: PTC.Parser ([(T.SubsInParen, T.NamePart)], P.Maybe T.SubsInParen)
    sip_start_p =
      (TP.many1 $ TP.try $ PTC.parser ++< PTC.parser) ++<
      TP.optionMaybe PTC.parser

instance PTC.HasParser T.SubsInParen where
  parser =
    T.SIP <$> PH.in_paren (PTC.parser ++< TP.many (PH.comma *> PTC.parser))

instance PTC.HasParser T.TVarSub where
  parser =
    T.FTS1 <$> TP.try PTC.parser <|> T.POPTS1 <$> TP.try PTC.parser <|>
    T.TAIOAS1 <$> PTC.parser

instance PTC.HasParser T.ProdOrPowerTypeSub where
  parser =
    T.PTS1 <$> TP.try PTC.parser <|> T.PoTS1 <$> PTC.parser

instance PTC.HasParser T.TypeAppIdOrTVSub where
  parser =
    T.TAIOAS <$>
      TP.optionMaybe PTC.parser ++< PTC.parser +++< TP.optionMaybe PTC.parser

instance PTC.HasParser T.TAIOASMiddle where
  parser =
    T.AHTV2 <$> PTC.parser <|>
    T.TIdStart2 <$> PTC.parser ++<
    TP.many (TP.try $ PTC.parser ++< TP.many1 (TP.lower <|> TP.upper))

instance PTC.HasParser T.SubsOrUndersInParen where
  parser =
    T.SOUIP <$> PH.in_paren (PTC.parser ++< TP.many (PH.comma *> PTC.parser))

instance PTC.HasParser T.SubOrUnder where
  parser =
    T.TVS1 <$> TP.try PTC.parser <|> PH.underscore *> P.return T.Underscore4

instance PTC.HasParser T.PowerTypeSub where
  parser =
    T.PoTS <$>
      PTC.parser ++< (TP.string "^" *> PTC.parser >>= PH.err_if_less_than_2)

instance PTC.HasParser T.PowerBaseTypeSub where
  parser =
    T.IPTS <$> TP.try (PH.in_paren PTC.parser) <|>
    T.TAIOAS2 <$> TP.try PTC.parser <|>
    PH.underscore *> P.return T.Underscore5

instance PTC.HasParser T.InParenTSub where
  parser = T.FTS2 <$> TP.try PTC.parser <|> T.PTS2 <$> PTC.parser

instance PTC.HasParser T.ProdTypeSub where
  parser =
    T.PTS <$> PTC.parser ++< (TP.many1 $ TP.try (TP.string " x ") *> PTC.parser)

instance PTC.HasParser T.FieldTypeSub where
  parser = T.PoTS2 <$> TP.try PTC.parser <|> T.PBTS1 <$> PTC.parser

instance PTC.HasParser T.FuncTypeSub where
  parser = T.FTS <$> PTC.parser ++< (TP.string " => " *> PTC.parser)

instance PTC.HasParser T.InOrOutTypeSub where
  parser =
    T.FTS3 <$> TP.try (PH.in_paren PTC.parser) <|>
    T.POPTS2 <$> TP.try PTC.parser <|>
    T.TAIOAS3 <$> TP.try PTC.parser  <|>
    PH.underscore *> P.return T.Underscore6

instance PTC.HasParser T.Proof where
  parser =
    T.P1 <$> (TP.char ' ' *> PTC.parser) ++< (TP.char ' ' *> PTC.parser) <|>
    T.P2 <$> (PH.nl *> TP.string "  " *> PTC.parser) ++< PTC.parser

instance PTC.HasParser T.IdOrOpEq where
  parser =
    T.IOOE <$>
      PTC.parser ++< TP.optionMaybe (TP.try $ PTC.parser ++< PTC.parser) <*
      TP.string " ="

instance PTC.HasParser T.TTValueExpr where
  parser =
    T.VEMWE <$> vemwe_p <|> T.LE2 <$> (TP.char ' ' *> PTC.parser)
    where
    vemwe_p :: PTC.Parser (T.ValueExpr, P.Maybe T.WhereExpr)
    vemwe_p =
      PH.twice_deeper
      (TP.try PH.nl_indent *> PTC.parser ++< TP.optionMaybe PTC.parser)

--  Program

instance PTC.HasParser T.Program where
  parser =
    T.P <$>
      (TP.many PH.nl *> PTC.parser) ++<
      TP.many (TP.try $ PH.nl *> PH.nl *> PTC.parser) <* TP.spaces <* TP.eof

instance PTC.HasParser T.ProgramPart where
  parser =
    T.TD <$> PTC.parser <|> T.TNN1 <$> PTC.parser <|> T.TT1 <$> PTC.parser <|>
    T.TPD <$> PTC.parser <|> T.GVDs2 <$> TP.try PTC.parser <|>
    T.VD2 <$> PTC.parser

-- Helpers.hs
