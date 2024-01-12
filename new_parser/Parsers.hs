{-# LANGUAGE LambdaCase,
TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Parsers where

import Text.Parsec hiding (parse)
import Text.Parsec.Token hiding (comma, charLiteral, stringLiteral)

import ASTTypes
import ShowInstances

type Parser = Parsec String ParserState

class HasParser a where
  parser :: Parser a

test_parser :: Parser a -> String -> Either ParseError a
test_parser = \p s -> runParser (p <* eof) (0, False) "test" s

-- Parser State

type IndentationLevel = Int
type InEqualLine = Bool
type ParserState = (IndentationLevel, InEqualLine)

increase_il :: Int -> Parser ()
increase_il = \i -> modifyState (\(il, b) -> (il + i, b))

decrease_il :: Int -> Parser ()
decrease_il = \i -> modifyState (\(il, b) -> (il - i, b))

in_equal_line :: Parser ()
in_equal_line = modifyState (\(il, _) -> (il, True))

not_in_equal_line :: Parser ()
not_in_equal_line = modifyState (\(il, _) -> (il, False))

are_we_in_equal_line :: Parser Bool
are_we_in_equal_line = snd <$> getState

inc_il_if_false :: Bool -> Parser ()
inc_il_if_false = \case
  True -> return ()
  False -> increase_il 1

dec_il_if_false :: Bool -> Parser ()
dec_il_if_false = \case
  True -> return ()
  False -> decrease_il 1

-- helper parsers

(comma, underscore, lower_under, in_paren) =
  (char ',' <* optional (char ' '), char '_', lower <|> underscore
  , \a -> char '(' *> a <* char ')'
  ) :: (Parser Char, Parser Char, Parser Char, Parser a -> Parser a) 

(nl, nl_indent, digits) =
  (many (char ' ' <|> char '\t') *> char '\n', nl *> indent, many1 digit)
  :: (Parser Char, Parser (), Parser String) 

nl_ind_or_one_space :: Parser ()
nl_ind_or_one_space = try nl_indent <|> char ' ' *> return ()

indent :: Parser ()
indent = getState >>= \(il, _) -> string (concat $ replicate il "  ") >> return ()

par_lower_unders :: Parser String
par_lower_unders = 
  try (string "()") >>= \par ->
  many1 lower_under >>= \lowers_unders ->
  return $ par ++ lowers_unders

(+++) :: Parser a -> Parser b -> Parser (a, b)
pa +++ pb = pa >>= \a -> pb >>= \b -> return (a, b)

-- helper ops

(.>) = flip (.)
(%>) = flip ($)

-- HasParser: Literal

instance HasParser Int where
  parser = 
    optionMaybe (char '-') >>= \case
      Nothing -> read <$> digits
      Just _ -> ((0 -) . read) <$> digits
  
instance HasParser Double where
  parser =
    optionMaybe (string "-") >>= \maybe_minus ->
    digits >>= \integer_part -> 
    char '.' *> digits >>= \decimal_part -> 
    optionMaybe exponent_p >>= \maybe_exponent ->
    return $ read $
      maybe_str maybe_minus ++ integer_part ++ "." ++ decimal_part ++ 
      maybe_str maybe_exponent
    where
    maybe_str :: Maybe String -> String
    maybe_str = \case
      Nothing -> ""
      Just str -> str

    exponent_p :: Parser String
    exponent_p =
      (char 'e' <|> char 'E') >>= \e ->
      optionMaybe (string "-") >>= \maybe_minus ->
      digits >>= \power_digits -> 
      return $ e : (maybe_str maybe_minus ++ power_digits)

instance HasParser Char where
  parser = charLiteral

instance HasParser String where
  parser = stringLiteral

instance HasParser Literal where
  parser = R <$> try parser <|> Int <$> parser <|> Ch <$> parser <|> S <$> parser

-- HasParser: Identifier, ParenExpr, Tuple, List, ParenFuncApp

instance HasParser Identifier where
  parser =
    lower >>= \l1 ->
    many lower_under >>= \lower_unders ->
    many par_lower_unders >>= \par_lower_unders_l ->
    option [] ((:[]) <$> digit) >>= \digit ->
    (l1 : concat [lower_unders, concat par_lower_unders_l, digit]) %> \id_str ->
      case elem id_str reserved of
        True -> unexpected $ "cannot use \"" ++ id_str ++ "\" as an identifier"
        False -> return $ Id id_str
    where
    reserved :: [String]
    reserved = ["cases", "where"]

instance HasParser ParenExpr where
  parser = PE <$> in_paren parser

instance HasParser InsideParenExpr where
  parser = LOE1 <$> try parser <|> LFE1 <$> parser 

instance HasParser Tuple where
  parser = T <$> (char '(' *> parser) +++ (comma *> parser <* char ')')

instance HasParser LineOrUnderExprs where
  parser = LOUEs <$> parser +++ many (comma *> parser)

instance HasParser LineOrUnderExpr where
  parser = LE1 <$> try parser <|> underscore *> return Underscore1

instance HasParser LineExpr where
  parser = LFE2 <$> try parser <|> LOE2 <$> try parser <|> BOAE1 <$> parser

instance HasParser BigTuple where
  parser =
    char '(' *> optional (char ' ') *> parser >>= \line_or_under_expr ->
    optional (try nl_indent) *> comma *> parser >>= \line_or_under_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \line_or_under_exprs_l ->
    nl_indent *> char ')' *>
    return (BT (line_or_under_expr, line_or_under_exprs, line_or_under_exprs_l))

instance HasParser List where
  parser = L <$> (char '[' *> optionMaybe parser <* char ']')

instance HasParser BigList where
  parser =
    char '[' *> optional (char ' ') *> parser >>= \line_or_under_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \line_or_under_exprs_l ->
    nl_indent *> char ']' *>
    (return $ BL (line_or_under_exprs, line_or_under_exprs_l))

instance HasParser ParenFuncApp where
  parser = 
    try iwa_parser <|> ai_parser <|> ia_parser
    where
    iwa_parser =
      optionMaybe parser >>= \maybe_args1 ->
      parser >>= \ident_with_args ->
      optionMaybe parser >>= \maybe_args2 ->
      return $ IWA1 (maybe_args1, ident_with_args, maybe_args2)
    ai_parser =
      parser >>= \args1 ->
      parser >>= \identifier ->
      optionMaybe parser >>= \maybe_args2 ->
      return $ AI (args1, identifier, maybe_args2)
    ia_parser = IA <$> parser +++ parser

instance HasParser Arguments where
  parser = As <$> in_paren parser

instance HasParser IdentWithArgs where
  parser =
    ident_with_args_start_p >>= \ident_with_args_start ->
    parser >>= \arguments ->
    many1 lower_under >>= \string ->
    many (try $ empty_paren_or_args_p +++ many1 lower_under) >>= \pairs ->
    optionMaybe digit >>= \maybe_digit ->
    return $ IWA $ (ident_with_args_start, arguments, string, pairs, maybe_digit)
    where
    ident_with_args_start_p :: Parser IdentWithArgsStart
    ident_with_args_start_p =
      lower >>= \l1 ->
      many lower_under >>= \lower_unders ->
      many par_lower_unders >>= \par_lower_unders_l ->
      return $ IWAS $ l1 : concat [lower_unders, concat par_lower_unders_l]

    empty_paren_or_args_p :: Parser EmptyParenOrArgs
    empty_paren_or_args_p =
      try (string "()") *> return EmptyParen <|> As1 <$> parser
      

-- HasParser: PreFunc, PostFunc, BasicExpr, Change

instance HasParser PreFunc where
  parser = PF <$> parser <* char ':'

instance HasParser PreFuncApp where
  parser = PrFA <$> parser +++ parser

instance HasParser PostFunc where
  parser = char '.' *> (C1 <$> parser <|> Id2 <$> parser <|> SI2 <$> parser)

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
      PE2 <$> try parser <|> BE2 <$> parser <|> underscore *> return Underscore2

instance HasParser BasicExpr where
  parser =
    PFA <$> try parser <|> SI1 <$> try parser <|> Lit1 <$> parser <|>
    Id1 <$> parser <|> T1 <$> parser <|> L1 <$> parser

instance HasParser Change where
  parser = 
    try (string "change{") *> field_change_p >>= \field_change ->
    many (comma *> field_change_p) <* char '}' >>= \field_changes ->
    return $ C (field_change, field_changes)
    where
    field_change_p :: Parser FieldChange
    field_change_p = FC <$> field_p +++ (string " = " *> parser)

    field_p :: Parser Field
    field_p = Id3 <$> parser <|> SI3 <$> parser

-- HasParser: OpExpr

instance HasParser OpExpr where
  parser = BOE1 <$> try parser <|> LOE3 <$> parser

instance HasParser OpExprStart where
  parser = OES <$> many1 (try $ parser +++ parser)

instance HasParser LineOpExpr where
  parser = LOE <$> try parser +++ parser

instance HasParser LineOpExprEnd where
  parser = LFE3 <$> try parser <|> OA1 <$> parser

instance HasParser BigOpExpr where
  parser = BOEOS1 <$> try parser <|> BOEFS1 <$> parser

instance HasParser BigOpExprOpSplit where
  parser =
    parser >>= \osl ->
    not_in_equal_line >>
    many (try parser) >>= \osls ->
    optionMaybe (try parser) >>= \maybe_oes ->
    parser >>= \ose ->
    return $ BOEOS (osl : osls, maybe_oes, ose)

instance HasParser OpSplitLine where
  parser =
    OSL <$> (parser +++ maybe_op_arg_comp_op <* indent)
    where
    maybe_op_arg_comp_op :: Parser (Maybe (Operand, FuncCompOp))
    maybe_op_arg_comp_op =
      try nl *> return Nothing <|>
      Just <$> (parser +++ (char ' ' *> parser <* char '\n'))

instance HasParser OpSplitEnd where
  parser = FE1 <$> try parser <|> OA2 <$> parser

instance HasParser BigOpExprFuncSplit where
  parser = BOEFS <$> parser +++ (not_in_equal_line *> parser)

instance HasParser BigOrCasesFuncExpr where
  parser = BFE1 <$> try parser <|> CFE1 <$> parser
  
instance HasParser Operand where
  parser =
    PE3 <$> try parser <|> BOAE2 <$> try parser <|> underscore *> return Underscore3

instance HasParser BasicOrAppExpr where
  parser = PrFA1 <$> try parser <|> PoFA1 <$> try parser <|> BE3 <$> parser 

instance HasParser Op where
  parser =  
    FCO3 <$> try (char ' ' *> parser <* char ' ') <|>
    OSO <$> (optional (char ' ') *> parser <* optional (char ' '))

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
    string "=" *> notFollowedBy (char '>') *> return Equal <|>
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
  parser = LFE <$> parser +++ (string " =>" *> parser)

instance HasParser BigFuncExpr where
  parser = BFE <$> parser +++ (string " =>" *> parser)

instance HasParser Parameters where
  parser =
    ParamId <$> parser <|>
    char '*' *> return Star1 <|>
    Params <$> (char '(' *> parser) +++ (many1 (comma *> parser) <* char ')')

instance HasParser LineFuncBody where
  parser = char ' ' *> (LOE4 <$> try parser <|> BOAE3 <$> parser)

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
    char '*' *> return Star2 <|>
    CParamId <$> parser <|>
    CParams <$> (char '(' *> parser) +++ (many1 (comma *> parser) <* char ')')

instance HasParser Case where
  parser = Ca <$> (try $ nl_indent *> parser) +++ (string " =>" *> parser)

instance HasParser EndCase where
  parser = EC <$> (try (nl_indent *> string "... =>") *> parser)

instance HasParser Matching where
  parser = 
    Lit2 <$> parser <|> PFM <$> try (parser +++ parser) <|> Id5 <$> parser <|>
    TM1 <$> parser <|> LM1 <$> parser

instance HasParser MatchingOrStar where
  parser = M1 <$> parser <|> char '*' *> return Star

instance HasParser TupleMatching where
  parser = TM <$> (char '(' *> parser) +++ (many1 (comma *> parser) <* char ')')

instance HasParser ListMatching where
  parser = 
    LM <$> (char '[' *> optionMaybe inside_p <* char ']')
    where
    inside_p :: Parser (MatchingOrStar, [MatchingOrStar])
    inside_p = parser +++ (many $ comma *> parser)

instance HasParser CaseBody where
  parser = 
    increase_il 1 >>
    case_body_start_p >>= \case_body_start ->
    optionMaybe (try parser) >>= \maybe_where_expr ->
    decrease_il 1 >>
    return (CB (case_body_start, maybe_where_expr))
    where
    case_body_start_p :: Parser CaseBodyStart
    case_body_start_p = BFB1 <$> try parser <|> LFB1 <$> parser

-- HasParser: ValueDef, WhereExpr

instance HasParser ValueDef where
  parser = 
    indent *> parser >>= \identifier ->
    increase_il 1 >>
    nl_ind_or_one_space *> string ": " *> parser >>= \type_ ->
    nl_indent *> string "= " *>
    increase_il 1 >> in_equal_line >>
    parser >>= \value_expr ->
    not_in_equal_line >>
    optionMaybe (try parser) >>= \maybe_where_expr ->
    decrease_il 2 >>
    return (VD (identifier, type_, value_expr, maybe_where_expr))

instance HasParser ValueExpr where
  parser = 
    OE2 <$> try parser <|>
    FE2 <$> try parser <|>
    BT1 <$> try parser <|>
    BL1 <$> try parser <|>
    BOAE5 <$> parser

instance HasParser GroupedValueDefs where
  parser = 
    indent *> parser >>= \identifier ->
    many1 (comma *> parser) >>= \identifiers -> 
    increase_il 1 >>
    nl_ind_or_one_space *> string ": " *> types_p >>= \types ->
    nl_indent *> string "= " *> parser >>= \comma_sep_line_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \comma_sep_line_exprs_l ->
    decrease_il 1 >>
    return (GVDs
      ( identifier, identifiers, types, comma_sep_line_exprs
      , comma_sep_line_exprs_l
      )) 
    where
    types_p :: Parser Types
    types_p =
      Ts <$> (parser +++ (many1 $ comma *> parser)) <|>
      All <$> (string "all " *> parser)

instance HasParser LineExprs where
  parser = CSLE <$> parser +++ (many (comma *> parser))

instance HasParser WhereExpr where
  parser = 
    nl_indent *> string "where" *> nl *> where_def_expr_p >>= \where_def_expr1 ->
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
    TA1 <$> try parser <|> TId1 <$> try parser <|> TV1 <$> parser

instance HasParser TypeId where
  parser = 
    upper >>= \u1 ->
    many1 (upper <|> lower) >>= \upper_lowers ->
    return $ TId $ u1 : upper_lowers

instance HasParser TypeVar where
  parser = PTV1 <$> try parser <|> AHTV1 <$> parser

instance HasParser ParamTVar where
  parser = PTV <$> (char 'T' *> fmap (\d -> read [d]) digit)

instance HasParser AdHocTVar where
  parser = AHTV <$> (char '@' *> upper)

instance HasParser FuncType where
  parser = FT <$> parser +++ (string " => " *> parser)

instance HasParser InOrOutType where
  parser =
    PT2 <$> try parser <|> FT2 <$> try (in_paren parser) <|> PoT2 <$> try parser <|>
    TA2 <$> try parser <|> TId2 <$> try parser <|> TV2 <$> parser

instance HasParser ProdType where
  parser = PT <$> parser +++ (many1 $ try (string " x ") *> parser)

instance HasParser FieldType where
  parser = PoT3 <$> try parser <|> PBT1 <$> parser 

instance HasParser PowerBaseType where
  parser =
    IPT <$> try (in_paren $ FT3 <$> try parser <|> PT3 <$> parser) <|>
    TA3 <$> try parser <|> TId3 <$> try parser <|> TV3 <$> parser

instance HasParser PowerType where
  parser = PoT <$> parser +++ (many1 $ try (string "^") *> parser)

instance HasParser TypeApp where
  parser =
    TIWA1 <$> try tiwa_p <|> TIPTI <$> tipti_p <|> TITIP <$> parser +++ parser
    where
    tiwa_p :: Parser (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) 
    tiwa_p =
      optionMaybe parser >>= \maybe_types_in_paren1 ->
      parser >>= \type_id_with_args ->
      optionMaybe parser >>= \maybe_types_in_paren2 ->
      return (maybe_types_in_paren1, type_id_with_args, maybe_types_in_paren2)

    tipti_p :: Parser (TypesInParen, TypeIdOrVar, Maybe TypesInParen)
    tipti_p = 
      parser >>= \types_in_paren ->
      parser >>= \type_id_or_var ->
      optionMaybe parser >>= \maybe_types_in_paren ->
      return (types_in_paren, type_id_or_var, maybe_types_in_paren)

instance HasParser TypeIdWithArgs where
  parser = TIWA <$> parser +++ many1 (try $ parser +++ many1 (lower <|> upper))

instance HasParser TypeIdOrVar where
  parser = TId4 <$> try parser <|> TV4 <$> parser

instance HasParser TypesInParen where
  parser = TIP <$> (char '(' *> parser) +++ (many (comma *> parser) <* char ')')

instance HasParser Condition where
  parser = Co <$> (parser <* string " ==> ")

-- HasParser: TypeDef, TypeNickname

instance HasParser TypeDef where
  parser = TTD1 <$> parser <|> OTD1 <$> parser

instance HasParser TupleTypeDef where
  parser =
    string "tuple_type " *> parser >>= \type_name ->
    nl *> string "value" *> nl *> string "  "  *> parser >>= \paren_comma_sep_ids ->
    string " : " *> parser >>= \tuple_type_def_end ->
    return $ TTD (type_name, paren_comma_sep_ids, tuple_type_def_end)

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
  parser = PVIP <$> (char '(' *> parser) +++ (many (comma *> parser) <* char ')')

instance HasParser IdTuple where
  parser = PCSIs <$> (char '(' *> parser) +++ (many1 (comma *> parser) <* char ')')

instance HasParser OrTypeDef where
  parser =
    string "or_type " *> parser >>= \type_name ->
    nl *> string "values" *> nl *> string "  " *> parser >>= \identifier ->
    optionMaybe (char ':' *> parser) >>= \maybe_simple_type ->
    many1 ident_maybe_simple_type_p >>= \ident_maybe_simple_types ->
    return $
      OTD (type_name, identifier, maybe_simple_type, ident_maybe_simple_types)
    where
    ident_maybe_simple_type_p :: Parser (Identifier, Maybe SimpleType)
    ident_maybe_simple_type_p =
      (string " | " *> parser) +++ (optionMaybe $ char ':' *> parser)

instance HasParser TypeNickname where
  parser = TNN <$> (string "type_nickname " *> parser) +++ (string " = " *> parser)

-- HasParser: TypePropDef

instance HasParser TypePropDef where
  parser = APD1 <$> try parser <|> RPD1 <$> parser

instance HasParser AtomPropDef where
  parser = 
    parser >>= \prop_name_line ->
    nl *> string "needed" *> nl *> string "  " *> parser >>= \identifier ->
    string " : " *> parser >>= \simple_type ->
    return $ APD (prop_name_line, identifier, simple_type)

instance HasParser RenamingPropDef where
  parser =
    parser >>= \prop_name_line ->
    nl *> string "equivalent" *> nl *> string "  " *> parser >>= \prop_name ->
    many (comma *> parser) >>= \prop_names ->
    return $ RPD (prop_name_line, prop_name, prop_names)

instance HasParser PropNameLine where
  parser = PNL <$> (string "type_proposition " *> parser)

instance HasParser PropName where
  parser =
    NPStart1 <$> np_start_p <|> AHVIPStart1 <$> ahvip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, AdHocVarsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try $ parser +++ parser) >>= \np_ahvips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_ahvips, maybe_name_part)

    ahvip_start_p :: Parser ([(AdHocVarsInParen, NamePart)], Maybe AdHocVarsInParen)
    ahvip_start_p = (many1 $ try $ parser +++ parser) +++ optionMaybe parser

instance HasParser AdHocVarsInParen where
  parser = AHVIP <$> (char '(' *> parser) +++ (many (comma *> parser) <* char ')')

instance HasParser NamePart where
  parser =
    NP <$> concat <$> many1 (lower_upper <|> under_lower_upper)
    where
    lower_upper :: Parser String
    lower_upper = fmap pure (lower <|> upper)

    under_lower_upper :: Parser String
    under_lower_upper = fmap (:) (char '_') <*> lower_upper

instance HasParser TypeTheo where
  parser =
    string "type_theorem " *> parser >>= \pps ->
    optionMaybe (string " => " *> parser) >>= \maybe_pps ->
    nl *> string "proof" *> nl *> string "  " *> parser >>= \id ->
    optionMaybe (try op_id_p) >>= \maybe_op_id ->
    increase_il 2 *>
    string " =" *> parser >>= \ttve ->
    decrease_il 2 *>
    return (TT (pps, maybe_pps, id, maybe_op_id, ttve))
    where
    op_id_p :: Parser (Op, Identifier)
    op_id_p = parser +++ (parser <* followed_by_equal)

    followed_by_equal :: Parser String
    followed_by_equal = lookAhead (string " =" <* notFollowedBy (char '>'))

instance HasParser PropNameWithSubs where
  parser = 
    NPStart2 <$> np_start_p <|> PSIPStart <$> psip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, SubsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try $ parser +++ parser) >>= \np_psips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_psips, maybe_name_part)

    psip_start_p :: Parser ([(SubsInParen, NamePart)], Maybe SubsInParen)
    psip_start_p = (many1 $ try $ parser +++ parser) +++ optionMaybe parser

instance HasParser SubsInParen where
  parser = PSIP <$> (char '(' *> parser) +++ (many (comma *> parser) <* char ')')

instance HasParser TVarSub where
  parser =
    FTS1 <$> try parser <|> PTS1 <$> try parser <|> PoTS1 <$> try parser <|> 
    TAS1 <$> try parser <|> TId5 <$> try parser <|> TV5 <$> parser

instance HasParser FuncTypeSub where
  parser = FTS <$> parser +++ (string " => " *> parser)

instance HasParser InOrOutTypeSub where
  parser = IOOT1 <$> try parser <|> char '_' *> return Underscore4

instance HasParser ProdTypeSub where
  parser = PTS <$> parser +++ (many1 $ try (string " x ") *> parser)

instance HasParser FieldTypeSub where
  parser = FiT1 <$> try parser <|> char '_' *> return Underscore5

instance HasParser PowerBaseTypeSub where
  parser = PBT2 <$> try parser <|> char '_' *> return Underscore6

instance HasParser PowerTypeSub where
  parser = PoTS <$> parser +++ (many1 $ try (string "^") *> parser)

instance HasParser TypeAppSub where
  parser =
    TIWAS1 <$> try tiwas_p <|> TIPSTI <$> tipsti_p <|> TITIPS <$> parser +++ parser
    where
    tiwas_p
      :: Parser (Maybe TypesInParenSub, TypeIdWithArgsSub, Maybe TypesInParenSub) 
    tiwas_p =
      optionMaybe parser >>= \maybe_tips1 ->
      parser >>= \tiwas ->
      optionMaybe parser >>= \maybe_tips2 ->
      return (maybe_tips1, tiwas, maybe_tips2)

    tipsti_p :: Parser (TypesInParenSub, TypeIdOrVar, Maybe TypesInParenSub)
    tipsti_p = 
      parser >>= \types_in_paren_sub ->
      parser >>= \type_id_or_var_sub ->
      optionMaybe parser >>= \maybe_types_in_paren_sub ->
      return (types_in_paren_sub, type_id_or_var_sub, maybe_types_in_paren_sub)

instance HasParser TypeIdWithArgsSub where
  parser = TIWAS <$> parser +++ many1 (try $ parser +++ many1 (lower <|> upper))

instance HasParser TypesInParenSub where
  parser = TIPS <$> (char '(' *> parser) +++ (many (comma *> parser) <* char ')')

instance HasParser SimpleTypeOrUnder where
  parser = ST1 <$> try parser <|> char '_' *> return Underscore7

instance HasParser TypeFunc where
  parser = TF_1 <$> try tf1_p <|> TF_2 <$> tf2_p <|> TF_3 <$> tf3_p
    where
    tf1_p :: Parser (Bool, TypeId, String, Bool)
    tf1_p = 
      bool_p >>= \b1 ->
      parser >>= \tid ->
      paren_lower_uppers_p >>= \str ->
      bool_p >>= \b2 ->
      return (b1, tid, str, b2)

    tf2_p :: Parser (TypeId, Bool)
    tf2_p = (string "()" *> parser) +++ bool_p

    tf3_p :: Parser TypeId
    tf3_p = parser <* string "()" 

    bool_p :: Parser Bool
    bool_p =
      optionMaybe (string "()") >>= \case
        Nothing -> return False
        Just _ -> return True

    paren_lower_uppers_p :: Parser String
    paren_lower_uppers_p = string "()" *> fmap ("()" ++) (many1 $ lower <|> upper) 

instance HasParser TTValueExpr where
  parser = BOCE <$> (try nl_indent *> parser) <|> LE2 <$> (char ' ' *> parser)

instance HasParser BigOrCasesExpr where
  parser = 
    BOE4 <$> try parser <|> BFE3 <$> try parser <|> CFE3 <$> try parser <|>
    BT2 <$> try parser <|> BL2 <$> parser

-- Program

instance HasParser Program where
  parser =
    many nl *> parser >>= \pp ->
    many (try $ nl *> nl *> parser) >>= \pps ->
    spaces *> eof *> return (P (pp, pps))

instance HasParser ProgramPart where
  parser = 
    TD <$> try parser <|>
    TNN1 <$> try parser <|>
    TT1 <$> try parser <|>
    TPD <$> try parser <|>
    GVDs2 <$> try parser <|>
    VD2 <$> parser

-- for literals. Had to copy from Text.Parsec.Token source code
-- because I didn't want spaces after the char and string literals ...
-- if anyone knows how to import hidden functions from a module plz let me know
-- (this could have been way faster)

-- char literal

charLiteral :: Parser Char
charLiteral =
  (between (char '\'') (char '\'' <?> "end of character") characterChar)
  <?> "character"

characterChar :: Parser Char
characterChar = charLetter <|> charEscape <?> "literal character"

charLetter :: Parser Char
charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

charEscape :: Parser Char
charEscape = do{ _ <- char '\\'; charEsc <?> "escape code" }

charEsc :: Parser Char
charEsc =
  choice (map parseEsc escMap)
  where
  parseEsc (c, code) = do{ _ <- char c; return code }
  escMap = zip ("ntr0\\\"\'") ("\n\t\r\0\\\"\'")

-- string literal

stringLiteral :: Parser String
stringLiteral =
  between (char '"') (char '"' <?> "end of string") (many stringChar)
  <?> "string literal"

stringChar :: Parser Char
stringChar = stringLetter <|> charEscape <?> "string character"

stringLetter :: Parser Char
stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))
