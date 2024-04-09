{-# LANGUAGE LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Parsing.AST where

import Text.Parsec 

import ASTTypes
import Helpers

import Parsing.TypesAndHelpers

-- HasParser class
class HasParser a where
  parser :: Parser a

-- HasParser: Literal
instance HasParser Int where
  parser = read <$> option "" (string "-") >++< digits   

instance HasParser Double where
  parser =
    read <$> int_p >++< string "." >++< digits >++< option "" exponent_p
    where
    int_p :: Parser String
    int_p = mapf (parser :: Parser Int) show

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

-- HasParser: Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId
instance HasParser Identifier where
  parser =
    id_p >>= \case
      Id (Nothing, IS "cases", [], Nothing, Nothing) ->
        unexpected $ "cannot use \"cases\" as an identifier"
      Id (Nothing, IS "where", [], Nothing, Nothing) ->
        unexpected $ "cannot use \"where\" as an identifier"
      id -> return id
    where
    id_p :: Parser Identifier
    id_p =
      optionMaybe parser >>= \maybe_uip1 ->
      parser >>= \id_start ->
      many (try parser) >>= \id_conts ->
      optionMaybe digit >>= \maybe_digit ->
      optionMaybe parser >>= \maybe_uip2 ->
      return $ Id (maybe_uip1, id_start, id_conts, maybe_digit, maybe_uip2)

instance HasParser SimpleId where
  parser =
    sid_p >>= \case
      SId (IS "cases", Nothing) ->
        unexpected $ "cannot use \"cases\" as an identifier"
      SId (IS "where", Nothing) ->
        unexpected $ "cannot use \"where\" as an identifier"
      id -> return id
    where
    sid_p :: Parser SimpleId
    sid_p = SId <$> parser +++ optionMaybe digit

instance HasParser IdStart where
  parser = IS <$> lower >:< many lower_under

instance HasParser IdCont where
  parser = IC <$> parser +++ many1 lower_under

instance HasParser UndersInParen where
  parser =
    string "(_" *> many (comma *> underscore) <* char ')' >>= \interm_unders ->
    return $ UIP $ length interm_unders + 1

instance HasParser ParenExpr where
  parser = PE <$> in_paren parser

instance HasParser InsideParenExpr where
  parser = LOE1 <$> try parser <|> LFE1 <$> parser 

instance HasParser Tuple where
  parser = T <$> in_paren (parser +++ (comma *> parser))

instance HasParser LineExprOrUnders where
  parser = LEOUs <$> parser +++ many (comma *> parser)

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
    line_expr_or_under_p +++ line_expr_or_unders_p ++< line_expr_or_unders_l_p
    where
    line_expr_or_under_p :: Parser LineExprOrUnder
    line_expr_or_under_p = char '(' *> opt_space *> parser

    line_expr_or_unders_p :: Parser LineExprOrUnders
    line_expr_or_unders_p = (optional (try nl_indent) *> comma *> parser) 

    line_expr_or_unders_l_p :: Parser [LineExprOrUnders]
    line_expr_or_unders_l_p =
      many (try (nl_indent *> comma) *> parser) <* nl_indent <* char ')'

instance HasParser List where
  parser =
    L <$> (char '[' *> opt_space_around (optionMaybe parser) <* char ']')

instance HasParser BigList where
  parser =
    BL <$>
      (char '[' *> opt_space *> parser) +++
      many (try (nl_indent *> comma) *> parser) <* nl_indent <* char ']'

instance HasParser ArgsStr where
  parser = parser >>= \args -> many1 lower_under >>= \str -> return (args, str)

instance HasParser ParenFuncAppOrId where
  parser = 
    optionMaybe parser >>= \maybe_args1 ->
    parser >>= \id_start ->
    many (try parser) >>= \arg_str_pairs ->
    optionMaybe digit >>= \maybe_digit ->
    optionMaybe parser >>= \maybe_args2 ->
    return $
      PFAOI (maybe_args1, id_start, arg_str_pairs, maybe_digit, maybe_args2)

instance HasParser Arguments where
  parser = As <$> in_paren parser

-- HasParser: PreFunc, PostFunc, BasicExpr, Change
instance HasParser PreFunc where
  parser = PF <$> parser <* char ':'

instance HasParser PreFuncApp where
  parser = PrFA <$> parser +++ parser

instance HasParser PostFunc where
  parser = char '.' *> (C1 <$> parser <|> SId1 <$> parser <|> SI2 <$> parser)

instance HasParser SpecialId where
  parser =
    string "1st" *> return First <|> string "2nd" *> return Second <|>
    string "3rd" *> return Third <|> string "4th" *> return Fourth <|>
    string "5th" *> return Fifth 

instance HasParser PostFuncApp where
  parser =
    PoFA <$> post_func_arg_p +++ many1 parser
    where
    post_func_arg_p :: Parser PostFuncArg
    post_func_arg_p =
      PE2 <$> try parser <|> BE2 <$> parser <|>
      underscore *> return Underscore2

instance HasParser Change where
  parser = 
    C <$>
      (try (string "change{") *> opt_space_around field_changes_p <* char '}')
    where
    field_changes_p :: Parser (FieldChange, [FieldChange])
    field_changes_p = field_change_p +++ many (comma *> field_change_p)

    field_change_p :: Parser FieldChange
    field_change_p =
      FC <$> field_p +++ (opt_space_around (string "=") *> parser)

    field_p :: Parser Field
    field_p = SId2 <$> parser <|> SI3 <$> parser

-- HasParser: OpExpr
instance HasParser OpExpr where
  parser = BOE1 <$> try parser <|> LOE3 <$> parser

instance HasParser OpExprStart where
  parser = OES <$> many1 (try $ parser +++ parser)

instance HasParser LineOpExpr where
  parser = LOE <$> try parser +++ parser

instance HasParser LineOpExprEnd where
  parser = LFE3 <$> try parser <|> O1 <$> parser

instance HasParser BigOpExpr where
  parser = BOEOS1 <$> try parser <|> BOEFS1 <$> parser

instance HasParser BigOpExprOpSplit where
  parser =
    parser >>= \osl ->
    we_are_not_in_equal_line >>
    many (try parser) >>= \osls ->
    optionMaybe (try parser) >>= \maybe_oes ->
    parser >>= \ose ->
    return $ BOEOS (osl : osls, maybe_oes, ose)

instance HasParser OpSplitLine where
  parser =
    OSL <$> (parser +++ maybe_oper_fco_parser <* indent)
    where
    maybe_oper_fco_parser :: Parser (Maybe OperFCO)
    maybe_oper_fco_parser = try nl *> return Nothing <|> Just <$> parser

instance HasParser OperFCO where
  parser = OFCO <$> parser +++ (char ' ' *> parser <* char '\n')

instance HasParser OpSplitEnd where
  parser = FE1 <$> try parser <|> O2 <$> parser

instance HasParser BigOpExprFuncSplit where
  parser = BOEFS <$> parser +++ (we_are_not_in_equal_line *> parser)

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
    string ">" *> return Greater <|>
    string "<" *> return Less <|>
    string "&" *> return And <|>
    string "|" *> return Or <|>
    try (string ";>") *> return Use <|>
    string ";" *> return Then

-- HasParser: FuncExpr
instance HasParser FuncExpr where
  parser = CFE2 <$> try parser <|> BFE2 <$> try parser <|> LFE4 <$> parser

instance HasParser LineFuncExpr where
  parser = LFE <$> parser +++ (func_arr *> parser)

instance HasParser BigFuncExpr where
  parser = BFE <$> parser +++ (func_arr *> parser)

instance HasParser Parameters where
  parser =
    ParamId <$> try parser <|> char '*' *> return Star1 <|>
    Params <$> in_paren (parser +++ many1 (comma *> parser)) 

instance HasParser LineFuncBody where
  parser = opt_space *> (LOE4 <$> try parser <|> BOAE3 <$> parser)

instance HasParser BigFuncBody where
  parser = nl_indent *> (OE1 <$> try parser <|> BOAE4 <$> parser)

instance HasParser CasesFuncExpr where 
  parser =
    parser >>= \cases_params ->

    are_we_in_equal_line >>= \answer ->
    inc_il_if_false answer *>

    many1 parser >>= \cases ->
    optionMaybe parser >>= \maybe_end_case ->

    dec_il_if_false answer *>

    return (CFE (cases_params, cases, maybe_end_case))

instance HasParser CasesParams where
  parser = 
    try (string "cases") *> return CasesKeyword <|>
    char '*' *> return Star2 <|> CParamId <$> try parser <|>
    CParams <$> in_paren (parser +++ many1 (comma *> parser))

instance HasParser Case where
  parser = Ca <$> try (nl_indent *> parser) +++ (func_arr *> deeper parser)

instance HasParser EndCase where
  parser = EC <$> try (nl_indent *> parser) +++ (func_arr *> deeper parser)

instance HasParser EndCaseParam where
  parser = Id1 <$> parser <|> string "..." *> return Ellipsis

instance HasParser OuterMatching where
  parser = M1 <$> try parser <|> SId3 <$> parser

instance HasParser Matching where
  parser = 
    Lit2 <$> parser <|> PFM <$> try (parser +++ parser) <|>
    TM1 <$> parser <|> LM1 <$> parser

instance HasParser InnerMatching where
  parser = Id2 <$> try parser <|> M2 <$> parser <|> char '*' *> return Star

instance HasParser TupleMatching where
  parser = TM <$> in_paren (parser +++ many1 (comma *> parser))

instance HasParser ListMatching where
  parser = 
    LM <$> (char '[' *> opt_space_around (optionMaybe inside_p) <* char ']')
    where
    inside_p :: Parser (InnerMatching, [InnerMatching])
    inside_p = parser +++ (many $ comma *> parser)

instance HasParser CaseBody where
  parser = BFB1 <$> try parser +++ optionMaybe (try parser) <|> LFB1 <$> parser

-- HasParser: ValueDef, WhereExpr
instance HasParser ValueDef where
  parser = 
    indent *> parser >>= \identifier ->

    increase_il_by 1 >>

    has_type_symbol *> parser >>= \type_ ->
    nl_indent *> string "= " *>

    we_are_in_equal_line >>
    increase_il_by 1 >>

    parser >>= \value_expr ->

    we_are_not_in_equal_line >>

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

    increase_il_by 1 >>

    has_type_symbol *> types_p >>= \types ->
    nl_indent *> string "= " *> parser >>= \comma_sep_line_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \comma_sep_line_exprs_l ->

    decrease_il_by 1 >>

    return
      (GVDs (id, ids, types, comma_sep_line_exprs, comma_sep_line_exprs_l)) 
    where
    types_p :: Parser Types
    types_p =
      Ts <$> (parser +++ (many1 $ comma *> parser)) <|>
      All <$> (string "all " *> parser)

instance HasParser LineExprs where
  parser = LEs <$> parser +++ (many (comma *> parser))

instance HasParser WhereExpr where
  parser = 
    nl_indent *> string "where" *>
    nl *> where_def_expr_p >>= \where_def_expr1 ->
    many (try $ nl *> nl *> where_def_expr_p) >>= \where_def_exprs ->
    return $ WE (where_def_expr1, where_def_exprs)
    where
    where_def_expr_p :: Parser WhereDefExpr
    where_def_expr_p = VD1 <$> try parser <|> GVDs1 <$> parser

-- HasParser: Type
instance HasParser Type where
  parser = Ty <$> (optionMaybe $ try parser) +++ parser

instance HasParser SimpleType where
  parser = 
    FT1 <$> try parser <|> PT1 <$> try parser <|> PoT1 <$> try parser <|> 
    TA1 <$> try parser <|> TIOV1 <$> parser

instance HasParser TypeIdOrVar where
  parser = TV1 <$> try parser <|> TId1 <$> parser

instance HasParser TypeId where
  parser = TId <$> upper >:< many (upper <|> lower)

instance HasParser TypeVar where
  parser = PTV1 <$> try parser <|> AHTV1 <$> parser

instance HasParser ParamTVar where
  parser = PTV <$> (char 'T' *> mapf digit (\d -> read [d]))

instance HasParser AdHocTVar where
  parser = AHTV <$> (char '@' *> upper)

instance HasParser TypeApp where
  parser =
    TIWA1 <$> try parser <|> TIPTI <$> parser <|> TITIP <$> parser +++ parser

instance HasParser TIWATypeApp where
  parser =
    optionMaybe parser >>= \maybe_types_in_paren1 ->
    parser >>= \type_id_with_args ->
    optionMaybe parser >>= \maybe_types_in_paren2 ->
    return (maybe_types_in_paren1, type_id_with_args, maybe_types_in_paren2)

instance HasParser TIPTITypeApp where
  parser =
    parser >>= \types_in_paren ->
    parser >>= \type_id_or_var ->
    optionMaybe parser >>= \maybe_types_in_paren ->
    return (types_in_paren, type_id_or_var, maybe_types_in_paren)

instance HasParser TypeIdWithArgs where
  parser = TIWA <$> parser +++ many1 (try $ parser +++ many1 (lower <|> upper))

instance HasParser TIdOrAdHocTVar where
  parser = TId2 <$> try parser <|> AHTV2 <$> parser

instance HasParser TypesInParen where
  parser = TIP <$> in_paren (parser +++ many (comma *> parser))

instance HasParser ProdType where
  parser = PT <$> parser +++ (many1 $ try (string " x ") *> parser)

instance HasParser FieldType where
  parser = PoT3 <$> try parser <|> PBT1 <$> parser 

instance HasParser PowerBaseType where
  parser =
    IPT <$> try (in_paren $ FT3 <$> try parser <|> PT3 <$> parser) <|>
    TA3 <$> try parser <|> TIOV3 <$> parser

instance HasParser PowerType where
  parser = PoT <$> parser +++ (string "^" *> int_greater_than_1)

int_greater_than_1 :: Parser Int
int_greater_than_1 =
  parser >>= \i -> case (i < 2) of
    True -> unexpected "integer in power type must be greater than 1"
    False -> return i

instance HasParser FuncType where
  parser = FT <$> parser +++ (string " => " *> parser)

instance HasParser InOrOutType where
  parser =
    PT2 <$> try parser <|> FT2 <$> try (in_paren parser) <|>
    PoT2 <$> try parser <|> TA2 <$> try parser <|> TIOV2 <$> parser

instance HasParser Condition where
  parser = Co <$> (parser <* string " --> ")

-- HasParser: TypeDef, TypeNickname
instance HasParser TypeDef where
  parser = TTD1 <$> parser <|> OTD1 <$> parser

instance HasParser TupleTypeDef where
  parser =
    string "tuple_type " *> parser >>= \type_name ->
    nl *> string "value" *> space_or_nl *> parser >>= \paren_comma_sep_ids ->
    opt_space_around (string ":") *> parser >>= \prod_or_power_type ->
    return $ TTD (type_name, paren_comma_sep_ids, prod_or_power_type)

instance HasParser ProdOrPowerType where
  parser = PT4 <$> try parser <|> PoT4 <$> parser

instance HasParser TypeName where
  parser =
    optionMaybe parser >>= \maybe_pvip1 ->
    parser >>= \tid ->
    many (try $ parser +++ (many1 $ lower <|> upper)) >>= \pvip_string_pairs ->
    optionMaybe parser >>= \maybe_pvip2 ->
    return $ TN (maybe_pvip1, tid, pvip_string_pairs, maybe_pvip2)

instance HasParser ParamVarsInParen where
  parser = PVIP <$> in_paren (parser +++ many (comma *> parser))

instance HasParser IdTuple where
  parser = PCSIs <$> in_paren (parser +++ many1 (comma *> parser))

instance HasParser OrTypeDef where
  parser =
    string "or_type " *> parser >>= \type_name ->
    nl *> string "values" *> space_or_nl *> parser >>= \simple_id ->
    optionMaybe (char ':' *> parser) >>= \maybe_simple_type ->
    many1 ident_maybe_simple_type_p >>= \ident_maybe_simple_types ->
    return $
      OTD (type_name, simple_id, maybe_simple_type, ident_maybe_simple_types)
    where
    ident_maybe_simple_type_p :: Parser (SimpleId, Maybe SimpleType)
    ident_maybe_simple_type_p =
      (opt_space_around (string "|") *> parser) +++
      (optionMaybe $ char ':' *> parser)

instance HasParser TypeNickname where
  parser =
    TNN <$>
      (string "type_nickname " *> parser) +++
      (opt_space_around (string "=") *> parser)

-- HasParser: TypePropDef
instance HasParser TypePropDef where
  parser = APD1 <$> try parser <|> RPD1 <$> parser

instance HasParser AtomPropDef where
  parser = 
    parser >>= \prop_name_line ->
    nl *> string "needed" *> space_or_nl *> parser >>= \identifier ->
    opt_space_around (string ":") *> parser >>= \simple_type ->
    return $ APD (prop_name_line, identifier, simple_type)

instance HasParser RenamingPropDef where
  parser =
    parser >>= \prop_name_line ->
    nl *> string "equivalent" *> space_or_nl *> parser >>= \prop_name ->
    many (comma *> parser) >>= \prop_names ->
    return $ RPD (prop_name_line, prop_name, prop_names)

instance HasParser PropNameLine where
  parser = PNL <$> (string "type_proposition " *> parser)

instance HasParser PropName where
  parser =
    NPStart1 <$> np_start_p <|> AHVIPStart <$> ahvip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, AdHocVarsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try $ parser +++ parser) >>= \np_ahvips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_ahvips, maybe_name_part)

    ahvip_start_p
      :: Parser ([(AdHocVarsInParen, NamePart)], Maybe AdHocVarsInParen)
    ahvip_start_p = (many1 $ try $ parser +++ parser) +++ optionMaybe parser

instance HasParser AdHocVarsInParen where
  parser = AHVIP <$> in_paren (parser +++ many (comma *> parser))

instance HasParser NamePart where
  parser =
    NP <$> concat <$> many1 (lower_upper <|> under_lower_upper)
    where
    lower_upper :: Parser String
    lower_upper = fmap pure (lower <|> upper)

    under_lower_upper :: Parser String
    under_lower_upper = underscore >:< lower_upper

-- HasParser: TypeTheo
instance HasParser TypeTheo where
  parser =
    string "type_theorem " *> parser >>= \pnws ->
    optionMaybe (string " --> " *> parser) >>= \maybe_pnws ->
    nl *> string "proof" *> parser >>= \proof ->
    return $ TT (pnws, maybe_pnws, proof)

instance HasParser PropNameWithSubs where
  parser = 
    NPStart2 <$> np_start_p <|> SIPStart <$> sip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, SubsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try $ parser +++ parser) >>= \np_sips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_sips, maybe_name_part)

    sip_start_p :: Parser ([(SubsInParen, NamePart)], Maybe SubsInParen)
    sip_start_p = (many1 $ try $ parser +++ parser) +++ optionMaybe parser

instance HasParser SubsInParen where
  parser = SIP <$> in_paren (parser +++ many (comma *> parser))

instance HasParser TVarSub where
  parser =
    FTS1 <$> try parser <|> PTS1 <$> try parser <|> PoTS1 <$> try parser <|> 
    TAS1 <$> try parser <|> TIOV4 <$> parser

instance HasParser TypeAppSub where
  parser =
    TIWS_TAS <$> try tiws_p <|> SOUIP_TI <$> souip_ti_p <|>
    TI_SOUIP <$> parser +++ parser
    where
    tiws_p ::
      Parser
        (Maybe SubsOrUndersInParen, TypeIdWithSubs, Maybe SubsOrUndersInParen) 
    tiws_p =
      optionMaybe parser >>= \maybe_souip1 ->
      parser >>= \tiws ->
      optionMaybe parser >>= \maybe_souip2 ->
      return (maybe_souip1, tiws, maybe_souip2)

    souip_ti_p ::
      Parser (SubsOrUndersInParen, TIdOrAdHocTVar, Maybe SubsOrUndersInParen)
    souip_ti_p = 
      parser >>= \souip ->
      parser >>= \type_id_or_var ->
      optionMaybe parser >>= \maybe_souip ->
      return (souip, type_id_or_var, maybe_souip)

instance HasParser TypeIdWithSubs where
  parser = TIWS <$> parser +++ many1 (try $ parser +++ many1 (lower <|> upper))

instance HasParser SubsOrUndersInParen where
  parser = SOUIP <$> in_paren (parser +++ many (comma *> parser))

instance HasParser SubOrUnder where
  parser = TVS1 <$> try parser <|> underscore *> return Underscore4

instance HasParser PowerTypeSub where
  parser = PoTS <$> parser +++ (string "^" *> int_greater_than_1)

instance HasParser PowerBaseTypeSub where
  parser =
    IPTS1 <$> try (in_paren parser) <|> TAS2 <$> try parser <|>
    TIOV5 <$> parser <|> underscore *> return Underscore5

instance HasParser InParenTSub where
  parser = FTS2 <$> try parser <|> PTS2 <$> parser

instance HasParser ProdTypeSub where
  parser = PTS <$> parser +++ (many1 $ try (string " x ") *> parser)

instance HasParser FieldTypeSub where
  parser = PoTS2 <$> try parser <|> PBTS1 <$> parser

instance HasParser FuncTypeSub where
  parser = FTS <$> parser +++ (string " => " *> parser)

instance HasParser InOrOutTypeSub where
  parser =
    FTS3 <$> try (in_paren parser) <|> PTS3 <$> try parser <|>
    PoTS3 <$> try parser <|> TAS3 <$> try parser <|> TIOV6 <$> parser <|>
    underscore *> return Underscore6 

instance HasParser Proof where
  parser =
    P1 <$> (char ' ' *> parser) +++ (char ' ' *> parser) <|>
    P2 <$> (nl *> string "  " *> parser) +++ parser

instance HasParser IdOrOpEq where
  parser =
    IOOE <$> parser +++ optionMaybe (try $ parser +++ parser) <* string " ="

instance HasParser TTValueExpr where
  parser =
    VEMWE <$> vemwe_p <|> LE2 <$> (char ' ' *> parser)
    where
    vemwe_p :: Parser (ValueExpr, Maybe WhereExpr)
    vemwe_p = deeper2 (try nl_indent *> parser +++ optionMaybe parser)

-- HasParser: Program
instance HasParser Program where
  parser =
    many nl *> parser >>= \pp ->
    many (try $ nl *> nl *> parser) >>= \pps ->
    spaces *> eof *> return (P (pp, pps))

instance HasParser ProgramPart where
  parser = 
    TD <$> try parser <|> TNN1 <$> try parser <|> TT1 <$> try parser <|>
    TPD <$> try parser <|> GVDs2 <$> try parser <|> VD2 <$> parser

-- For fast vim navigation
-- ShowInstances.hs
-- HsGenTest.hs
-- Parsing/Test.hs
-- ASTTypes.hs
