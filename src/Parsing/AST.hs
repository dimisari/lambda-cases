{-
This file implements a parser for every type of the AST by implementing an
instance of the HasParser type class
-}

{-# language LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Parsing.AST where

import Control.Monad
import Text.Parsec

import ASTTypes
import Helpers

import Parsing.TypesAndHelpers

-- HasParser class + parse function

class HasParser a where
  parser :: Parser a

parse :: HasParser a => String -> Either ParseError a
parse = runParser (parser <* eof) (0, False) ""

-- HasParser instances
--   Literal

instance HasParser Integer where
  parser = read <$> option "" (string "-") >++< digits

instance HasParser Double where
  parser =
    read <$> int_p >++< string "." >++< digits >++< option "" exponent_p
    where
    int_p :: Parser String
    int_p = show <$> (parser :: Parser Integer)

    exponent_p :: Parser String
    exponent_p = (char 'e' <|> char 'E') >:< int_p

instance HasParser Char where
  parser = charLiteral

instance HasParser String where
  parser = stringLiteral

instance HasParser Literal where
  parser =
    R <$> try parser <|> Int <$> parser <|> Ch <$> parser <|> S <$> parser <?>
    "Literal"

--  Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

instance HasParser Identifier where
  parser =
    id_p >>= check_not_reserved
    where
    id_p :: Parser Identifier
    id_p =
      optionMaybe parser >>= \muip1 ->
      parser >>= \id_start ->
      many (try parser) >>= \id_conts ->
      optionMaybe digit >>= \mdigit ->
      optionMaybe parser >>= \muip2 ->
      return $ Id (muip1, id_start, id_conts, mdigit, muip2)

    check_not_reserved :: Identifier -> Parser Identifier
    check_not_reserved = \id -> case id of
      Id (Nothing, IS id_str, [], Nothing, Nothing) ->
        err_if_reserved id_str >> return id
      _ -> return id

instance HasParser SimpleId where
  parser =
    sid_p >>= check_not_reserved
    where
    sid_p :: Parser SimpleId
    sid_p = SId <$> parser ++< optionMaybe digit

    check_not_reserved :: SimpleId -> Parser SimpleId
    check_not_reserved = \sid -> case sid of
      SId (IS id_str, Nothing) -> err_if_reserved id_str >> return sid
      _ -> return sid

instance HasParser IdStart where
  parser = IS <$> lower >:< many lower_under

instance HasParser IdCont where
  parser = IC <$> parser ++< many1 lower_under

instance HasParser UndersInParen where
  parser =
    (string "(_" *> many (comma *> underscore) <* char ')') >$>
    \interm_unders -> UIP $ length interm_unders + 1

instance HasParser ParenExpr where
  parser = PE <$> in_paren parser

instance HasParser InsideParenExpr where
  parser = LOE1 <$> try parser <|> LFE1 <$> parser

instance HasParser Tuple where
  parser = T <$> in_paren (parser ++< (comma *> parser))

instance HasParser LineExprOrUnders where
  parser = LEOUs <$> parser ++< many (comma *> parser)

instance HasParser LineExprOrUnder where
  parser = LE1 <$> try parser <|> underscore *> return Underscore1

instance HasParser LineExpr where
  parser = LFE2 <$> try parser <|> LOE2 <$> try parser <|> BOAE1 <$> parser

instance HasParser BasicOrAppExpr where
  parser = PrFA1 <$> try parser <|> PoFA1 <$> try parser <|> BE3 <$> parser

instance HasParser BasicExpr where
  parser =
    PFAOI1 <$> try parser <|> SI1 <$> try parser <|> Lit1 <$> parser <|>
    T1 <$> parser <|> L1 <$> parser <?> expecting_msg
    where
    expecting_msg :: String
    expecting_msg =
      "Basic expression: Literal, Identifier, Tuple, List, " ++
      "Parenthesis Function Application, SpecialId"

instance HasParser BigTuple where
  parser =
    BT <$>
    leou_p ++< split_p +++< leous_p ++++< leous_l_p <* nl_indent <* char ')'
    where
    leou_p :: Parser LineExprOrUnder
    leou_p = char '(' *> opt_space *> parser

    split_p :: Parser BigTupleSplit
    split_p = try nl_indent *> return Split <|> return NoSplit

    leous_p :: Parser LineExprOrUnders
    leous_p = comma *> parser

    leous_l_p :: Parser [LineExprOrUnders]
    leous_l_p = many $ try (nl_indent *> comma) *> parser

instance HasParser List where
  parser =
    L <$> (char '[' *> opt_space_around (optionMaybe parser) <* char ']')

instance HasParser BigList where
  parser =
    BL <$> leous_p ++< leous_l_p <* nl_indent <* char ']'
    where
    leous_p :: Parser LineExprOrUnders
    leous_p = char '[' *> opt_space *> parser

    leous_l_p :: Parser [LineExprOrUnders]
    leous_l_p = many $ try (nl_indent *> comma) *> parser

instance HasParser ArgsStr where
  parser = parser ++< many1 lower_under

instance HasParser ParenFuncAppOrId where
  parser =
    optionMaybe parser >>= \margs1 ->
    parser >>= \id_start ->
    many (try parser) >>= \arg_str_pairs ->
    optionMaybe digit >>= \mdigit ->
    optionMaybe parser >>= \margs2 ->
    return $ PFAOI (margs1, id_start, arg_str_pairs, mdigit, margs2)

instance HasParser Arguments where
  parser = As <$> in_paren parser

--  PreFunc, PostFunc, BasicExpr, Change

instance HasParser PreFunc where
  parser = PF <$> parser <* string "--"

instance HasParser PreFuncApp where
  parser = PrFA <$> parser ++< parser

instance HasParser PostFunc where
  parser =
    try (char '.' *> notFollowedBy (string "change{")) *>
    (SId1 <$> parser <|> SI2 <$> parser)

instance HasParser SpecialId where
  parser =
    string "1st" *> return First <|> string "2nd" *> return Second <|>
    string "3rd" *> return Third <|> string "4th" *> return Fourth <|>
    string "5th" *> return Fifth

instance HasParser PostFuncApp where
  parser = PoFA <$> parser ++< parser

instance HasParser PostFuncArg where
  parser =
    PE2 <$> try parser <|> BE2 <$> parser <|> underscore *> return Underscore2

instance HasParser PostFuncAppEnd where
  parser = DC1 <$> parser <|> PFsMDC <$> many1 parser ++< optionMaybe parser

instance HasParser DotChange where
  parser =
    DC <$>
    (try (string ".change{") *> opt_space_around field_changes_p <* char '}')
    where
    field_changes_p :: Parser (FieldChange, [FieldChange])
    field_changes_p = field_change_p ++< many (comma *> field_change_p)

    field_change_p :: Parser FieldChange
    field_change_p = FC <$> field_p ++< (equals *> parser)

    field_p :: Parser Field
    field_p = SId2 <$> parser <|> SI3 <$> parser

--  OpExpr

instance HasParser OpExpr where
  parser = BOE1 <$> try parser <|> LOE3 <$> parser

instance HasParser OpExprStart where
  parser = OES <$> many1 (try $ parser ++< parser)

instance HasParser LineOpExpr where
  parser = LOE <$> try parser ++< parser

instance HasParser LineOpExprEnd where
  parser = LFE3 <$> try parser <|> O1 <$> parser

instance HasParser BigOpExpr where
  parser = BOEOS1 <$> try parser <|> BOEFS1 <$> parser

instance HasParser BigOpExprOpSplit where
  parser =
    BOEOS <$>
      parser >:<
      (set_in_equal_line False *> many (try parser)) ++<
      optionMaybe (try parser) +++<
      parser

instance HasParser OpSplitLine where
  parser =
    ( OFCO1 <$> try parser <|>
      OESMOFCO <$> (parser ++< mofco_parser)
    ) <* indent
    where
    mofco_parser :: Parser (Maybe OperFCO)
    mofco_parser = try nl *> return Nothing <|> Just <$> parser

instance HasParser OperFCO where
  parser = OFCO <$> parser ++< (char ' ' *> parser <* char '\n')

instance HasParser OpSplitEnd where
  parser = FE1 <$> try parser <|> O2 <$> parser

instance HasParser BigOpExprFuncSplit where
  parser = BOEFS <$> parser ++< parser

instance HasParser BigOrCasesFuncExpr where
  parser = BFE1 <$> try parser <|> CFE1 <$> parser

instance HasParser Operand where
  parser =
    BOAE2 <$> try parser <|> PE3 <$> try parser <|>
    underscore *> return Underscore3

instance HasParser Op where
  parser =
    FCO3 <$> try (char ' ' *> parser <* char ' ') <|>
    OSO <$> opt_space_around parser

instance HasParser FuncCompOp where
  parser =
    string "o>" *> return RightComp <|> try (string "<o") *> return LeftComp

instance HasParser OptionalSpacesOp where
  parser =
    try (string "->") *> return RightApp <|>
    try (string "<-") *> return LeftApp <|>
    string "^" *> return Power <|>
    string "*" *> return Mult <|>
    string "/" *> return Div <|>
    string "+" *> return Plus <|>
    string "-" *> return Minus <|>
    string "==" *> return Equal <|>
    string "!=" *> return NotEqual <|>
    try (string ">=") *> return GrEq <|>
    try (string "<=") *> return LeEq <|>
    try (string ">>") *> return Use <|>
    string ">" *> return Greater <|>
    string "<" *> return Less <|>
    string "&" *> return And <|>
    string "|" *> return Or <|>
    string ";" *> return Then

--  FuncExpr

instance HasParser FuncExpr where
  parser = CFE2 <$> try parser <|> BFE2 <$> try parser <|> LFE4 <$> parser

instance HasParser LineFuncExpr where
  parser = LFE <$> parser ++< (func_arr *> parser)

instance HasParser BigFuncExpr where
  parser = BFE <$> parser ++< (func_arr *> parser)

instance HasParser Parameters where
  parser =
    ParamId <$> try parser <|> char '*' *> return Star1 <|>
    Params <$> in_paren (parser ++< many1 (comma *> parser))

instance HasParser LineFuncBody where
  parser =
    opt_space *>
    ( LOE4 <$> try parser <|> BOAE3 <$> try parser <|>
      LFE5 <$> (char '(' *> opt_space_around parser <* char ')')
    )

instance HasParser BigFuncBody where
  parser =
    nl_indent *> set_in_equal_line False *>
    ( OE1 <$> try parser <|> BOAE4 <$> try parser <|>
      LFE6 <$> (char '(' *> opt_space_around parser <* char ')')
    )

instance HasParser CasesFuncExpr where
  parser =
    parser >>= \cases_params ->

    check_at_least_one_cases_keyword cases_params >>

    deeper_if_not_in_equal_line all_cases_p >>= \(cases, maybe_end_case) ->
    return $ CFE (cases_params, cases, maybe_end_case)
    where
    all_cases_p :: Parser ([Case], Maybe EndCase)
    all_cases_p =
      set_in_equal_line False *> many1 parser ++< optionMaybe parser

    check_at_least_one_cases_keyword :: CasesParams -> Parser ()
    check_at_least_one_cases_keyword =
      count_cases_keywords >=> \case
        0 -> unexpected "0 cases keywords in cases func expr"
        n ->
          case n > 0 of
            True -> do_nothing
            False -> error "Should be impossible"

    count_cases_keywords :: CasesParams -> Parser Int
    count_cases_keywords = \case
      CParamId _ -> return 0
      QuestionMark -> return 1
      Star2 -> return 0
      CParams (cp, cps) -> mapM count_cases_keywords (cp : cps) >$> sum

instance HasParser CasesParams where
  parser =
    try (char '?') *> return QuestionMark <|>
    char '*' *> return Star2 <|> CParamId <$> try parser <|>
    CParams <$> in_paren (parser ++< many1 (comma *> parser))

instance HasParser Case where
  parser = Ca <$> try (nl_indent *> parser) ++< (func_arr *> deeper parser)

instance HasParser EndCase where
  parser = EC <$> try (nl_indent *> parser) ++< (func_arr *> deeper parser)

instance HasParser OuterMatching where
  parser = M1 <$> try parser <|> SId3 <$> parser

instance HasParser EndCaseParam where
  parser = Id1 <$> parser <|> string "..." *> return Ellipsis

instance HasParser Matching where
  parser =
    Lit2 <$> parser <|> PFM <$> try (parser ++< parser) <|>
    TM1 <$> parser <|> LM1 <$> parser

instance HasParser InnerMatching where
  parser = M2 <$> try parser <|> Id2 <$> parser <|> char '*' *> return Star

instance HasParser TupleMatching where
  parser = TM <$> in_paren (parser ++< many1 (comma *> parser))

instance HasParser ListMatching where
  parser =
    LM <$> (char '[' *> opt_space_around inside_list_p <* char ']')
    where
    inside_list_p
      :: Parser
         (Maybe (InnerMatching, [InnerMatching], Maybe RestListMatching))
    inside_list_p = optionMaybe $ parser ++< ims_p +++< optionMaybe parser

    ims_p :: Parser [InnerMatching]
    ims_p =
      many $ try $ comma *> parser <* notFollowedBy (equals *> string "...")

instance HasParser RestListMatching where
  parser = RLM <$> (comma *> optionMaybe (parser <* equals) <* string "...")

instance HasParser CaseBody where
  parser = BFB1 <$> try parser ++< optionMaybe (try parser) <|> LFB1 <$> parser

--  ValueDef, WhereExpr

instance HasParser ValueDef where
  parser =
    indent *> parser >>= \identifier ->

    increase_il_by 1 >>

    has_type_symbol *> parser >>= \type_ ->
    nl_indent *> string "= " *>

    set_in_equal_line True >>
    increase_il_by 1 >>

    parser >>= \value_expr ->

    set_in_equal_line False >>

    optionMaybe (try parser) >>= \maybe_where_expr ->

    decrease_il_by 2 >>

    return (VD (identifier, type_, value_expr, maybe_where_expr))

instance HasParser ValueExpr where
  parser =
    OE2 <$> try parser <|> FE2 <$> try parser <|> BT1 <$> try parser <|>
    BL1 <$> try parser <|> BOAE5 <$> parser <?> "value expression"

instance HasParser GroupedValueDefs where
  parser =
    indent *> parser >>= \id ->
    many1 (comma *> parser) >>= \ids ->
    deeper (types_p ++< equal_les_p +++< les_l_p) >>= \(ts, equal_les, les_l) ->
    return $ GVDs (id, ids, ts, equal_les, les_l)
    where
    types_p :: Parser Types
    types_p = has_type_symbol *> parser

    equal_les_p :: Parser LineExprs
    equal_les_p = nl_indent *> string "= " *> parser

    les_l_p :: Parser [LineExprs]
    les_l_p = many $ try (nl_indent *> comma) *> parser

instance HasParser Types where
  parser =
    Ts <$> (parser ++< many1 (comma *> parser)) <|>
    All <$> (string "all " *> parser)

instance HasParser LineExprs where
  parser = LEs <$> parser ++< many (comma *> parser)

instance HasParser WhereExpr where
  parser =
    WE <$>
      (try (nl_indent *> string "where") *> nl *> where_def_expr_p) ++<
      many (try $ nl *> nl *> where_def_expr_p)
    where
    where_def_expr_p :: Parser WhereDefExpr
    where_def_expr_p = VD1 <$> try parser <|> GVDs1 <$> parser

--  Type

instance HasParser Type where
  parser = Ty <$> (optionMaybe $ try parser) ++< parser

instance HasParser SimpleType where
  parser =
    FT1 <$> try parser <|> PT1 <$> try parser <|> PoT1 <$> try parser <|>
    PTV1 <$> try parser <|> TAIOA1 <$> parser

instance HasParser TypeId where
  parser = TId <$> upper >:< many (upper <|> lower)

instance HasParser ParamTVar where
  parser = PTV <$> (char 'T' *> mapf digit (\d -> read [d]))

instance HasParser AdHocTVar where
  parser = AHTV <$> (char '@' *> upper)

instance HasParser TypeAppIdOrAHTV where
  parser = TAIOA <$> optionMaybe parser ++< parser +++< optionMaybe parser

instance HasParser TAIOAMiddle where
  parser =
    AHTV2 <$> parser <|>
    TIdStart1 <$> parser ++< many (try $ parser ++< many1 (lower <|> upper))

instance HasParser TypesInParen where
  parser = TIP <$> in_paren (parser ++< many (comma *> parser))

instance HasParser ProdType where
  parser = PT <$> parser ++< (many1 $ try (string " x ") *> parser)

instance HasParser FieldType where
  parser = PoT2 <$> try parser <|> PBT1 <$> parser

instance HasParser PowerBaseType where
  parser = PTV2 <$> parser <|> TAIOA2 <$> try parser <|> IPT <$> parser

instance HasParser InParenT where
  parser =
    in_paren $
      FT3 <$> try parser <|> PT3 <$> try parser <|> PoT3 <$> try parser

instance HasParser PowerType where
  parser = PoT <$> parser ++< (string "^" *> parser >>= err_if_less_than_2)

instance HasParser FuncType where
  parser = FT <$> parser ++< (string " => " *> parser)

instance HasParser InOrOutType where
  parser =
    PT2 <$> try parser <|> FT2 <$> try (in_paren parser) <|>
    PoT4 <$> try parser <|> PTV3 <$> try parser <|> TAIOA3 <$> parser

instance HasParser Condition where
  parser = Co <$> (parser <* string " --> ")

--  TypeDef, TypeNickname

instance HasParser TypeDef where
  parser = TTD1 <$> parser <|> OTD1 <$> parser

instance HasParser TupleTypeDef where
  parser =
    TTD <$>
      (try (string "tuple type:") *> opt_space *> parser)
      ++<
      (opt_space_around (char '=') *> parser)
      +++<
      (nl *> string "field names:" *> space_or_nl *> parser)

instance HasParser ProdOrPowerType where
  parser = PT4 <$> try parser <|> PoT5 <$> parser

instance HasParser TypeName where
  parser =
    TN <$>
      optionMaybe parser ++<
      parser +++<
      many (try $ parser ++< (many1 $ lower <|> upper)) ++++<
      optionMaybe parser

instance HasParser ParamVarsInParen where
  parser = PVIP <$> in_paren (parser ++< many (comma *> parser))

instance HasParser FieldNames where
  parser = PCSIs <$> in_paren (parser ++< many1 (comma *> parser))

instance HasParser OrTypeDef where
  parser =
    OTD <$>
      (try (string "or type:") *> opt_space *> parser) ++<
      (nl *> string "values:" *> space_or_nl *> parser) +++<
      many (opt_space_around (string "|") *> parser)

instance HasParser PossibleValue where
  parser =
    PV <$>
      parser ++<
      ( optionMaybe $
        (string "--<" *> parser <* opt_space_around (char ':')) ++<
        (parser <* char '>')
      )

instance HasParser TypeNickname where
  parser =
    TNN <$>
      (try (string "type nickname:") *> opt_space *> parser) ++<
      (equals *> parser)

--  TypePropDef

instance HasParser TypePropDef where
  parser = APD1 <$> try parser <|> RPD1 <$> parser

instance HasParser AtomPropDef where
  parser =
    APD <$>
      parser ++<
      (nl *> string "needed" *> space_or_nl *> parser) +++<
      (opt_space_around (string ":") *> parser)

instance HasParser RenamingPropDef where
  parser =
    RPD <$>
      parser ++<
      (nl *> string "equivalent" *> space_or_nl *> parser) +++<
      many (comma *> parser)

instance HasParser PropNameLine where
  parser = PNL <$> (try (string "type_proposition ") *> parser)

instance HasParser PropName where
  parser =
    NPStart1 <$> np_start_p <|> TIPStart <$> tip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, TypesInParen)], Maybe NamePart)
    np_start_p =
      upper ++< many1 (try $ parser ++< parser) +++< optionMaybe parser

    tip_start_p
      :: Parser ([(TypesInParen, NamePart)], Maybe TypesInParen)
    tip_start_p = (many1 $ try $ parser ++< parser) ++< optionMaybe parser

instance HasParser NamePart where
  parser =
    NP <$> concat <$> many1 (lower_or_upper <|> under_upper)
    where
    lower_or_upper :: Parser String
    lower_or_upper = fmap (:[]) (lower <|> upper)

    under_upper :: Parser String
    under_upper = underscore >:< fmap (:[]) upper

--  TypeTheo

instance HasParser TypeTheo where
  parser =
    TT <$> pnws_p ++< mpnws_p +++< proof_p
    where
    pnws_p :: Parser [PropNameWithSubs]
    pnws_p = try (string "type_theorem ") *> parser >$> (\x -> [x])

    mpnws_p :: Parser (Maybe PropNameWithSubs)
    mpnws_p = optionMaybe (string " --> " *> parser)

    proof_p :: Parser Proof
    proof_p = nl *> string "proof" *> parser

instance HasParser PropNameWithSubs where
  parser =
    NPStart2 <$> np_start_p <|> SIPStart <$> sip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, SubsInParen)], Maybe NamePart)
    np_start_p =
      upper ++< many1 (try $ parser ++< parser) +++< optionMaybe parser

    sip_start_p :: Parser ([(SubsInParen, NamePart)], Maybe SubsInParen)
    sip_start_p = (many1 $ try $ parser ++< parser) ++< optionMaybe parser

instance HasParser SubsInParen where
  parser = SIP <$> in_paren (parser ++< many (comma *> parser))

instance HasParser TVarSub where
  parser =
    FTS1 <$> try parser <|> PTS1 <$> try parser <|> PoTS1 <$> try parser <|>
    PTV4 <$> try parser <|> TAIOAS1 <$> parser

instance HasParser TypeAppIdOrAHTVSub where
  parser = TAIOAS <$> optionMaybe parser ++< parser +++< optionMaybe parser

instance HasParser TAIOASMiddle where
  parser =
    AHTV3 <$> parser <|>
    TIdStart2 <$> parser ++< many (try $ parser ++< many1 (lower <|> upper))

instance HasParser SubsOrUndersInParen where
  parser = SOUIP <$> in_paren (parser ++< many (comma *> parser))

instance HasParser SubOrUnder where
  parser = TVS1 <$> try parser <|> underscore *> return Underscore4

instance HasParser PowerTypeSub where
  parser = PoTS <$> parser ++< (string "^" *> parser >>= err_if_less_than_2)

instance HasParser PowerBaseTypeSub where
  parser =
    IPTS <$> try (in_paren parser) <|> TAIOAS2 <$> try parser <|>
    PTV5 <$> parser <|> underscore *> return Underscore5

instance HasParser InParenTSub where
  parser = FTS2 <$> try parser <|> PTS2 <$> parser

instance HasParser ProdTypeSub where
  parser = PTS <$> parser ++< (many1 $ try (string " x ") *> parser)

instance HasParser FieldTypeSub where
  parser = PoTS2 <$> try parser <|> PBTS1 <$> parser

instance HasParser FuncTypeSub where
  parser = FTS <$> parser ++< (string " => " *> parser)

instance HasParser InOrOutTypeSub where
  parser =
    FTS3 <$> try (in_paren parser) <|> PTS3 <$> try parser <|>
    PoTS3 <$> try parser <|> TAIOAS3 <$> try parser <|> PTV6 <$> parser <|>
    underscore *> return Underscore6

instance HasParser Proof where
  parser =
    P1 <$> (char ' ' *> parser) ++< (char ' ' *> parser) <|>
    P2 <$> (nl *> string "  " *> parser) ++< parser

instance HasParser IdOrOpEq where
  parser =
    IOOE <$> parser ++< optionMaybe (try $ parser ++< parser) <* string " ="

instance HasParser TTValueExpr where
  parser =
    VEMWE <$> vemwe_p <|> LE2 <$> (char ' ' *> parser)
    where
    vemwe_p :: Parser (ValueExpr, Maybe WhereExpr)
    vemwe_p = twice_deeper (try nl_indent *> parser ++< optionMaybe parser)

--  Program

instance HasParser Program where
  parser =
    P <$>
      (many nl *> parser) ++< many (try $ nl *> nl *> parser) <* spaces <* eof

instance HasParser ProgramPart where
  parser =
    TD <$> parser <|> TNN1 <$> parser <|> TT1 <$> parser <|>
    TPD <$> parser <|> GVDs2 <$> try parser <|> VD2 <$> parser

