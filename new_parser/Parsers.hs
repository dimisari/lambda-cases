{-# LANGUAGE LambdaCase,
TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Parsers where

import Text.Parsec hiding (parse)
import Text.Parsec.Token hiding (comma, charLiteral, stringLiteral)

import ASTTypes
import ShowInstances

type Parser = Parsec String Int

class HasParser a where
  parser :: Parser a

test_parser :: Parser a -> String -> Either ParseError a
test_parser = \p s -> runParser (p <* eof) 0 "test" s

-- helper parsers

(comma, underscore, lower_under, in_paren, nl_indent, one_space, digits) =
  (string ", ", char '_', lower <|> underscore, \a -> char '(' *> a <* char ')'
  , char '\n' *> indent, char ' ' *> return (), many1 digit
  )
  ::
  ( Parser String, Parser Char, Parser Char, Parser a -> Parser a
  , Parser (), Parser (), Parser String
  ) 

indent :: Parser ()
indent = getState >>= \il -> string (concat $ replicate il "  ") >> return ()

par_lower_unders = 
  try (string "()") >>= \par ->
  many1 lower_under >>= \lowers_unders ->
  return $ par ++ lowers_unders

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
    return $ Id $ l1 : concat [lower_unders, concat par_lower_unders_l, digit]

instance HasParser ParenExpr where
  parser = PE <$> in_paren parser

instance HasParser SimpleExpr where
  parser = SOE1 <$> try parser <|> SFE1 <$> parser

instance HasParser Tuple where
  parser = 
    char '(' *> parser <* comma >>= \line_expr ->
    parser <* char ')' >>= \comma_sep_line_exprs ->
    return $ T (line_expr, comma_sep_line_exprs)

instance HasParser CommaSepLineExprs where
  parser =
    parser >>= \line_expr ->
    many (comma *> parser) >>= \line_exprs ->
    return $ CSLE (line_expr, line_exprs)

instance HasParser LineExpr where
  parser = SE <$> try parser <|> NPOA1 <$> parser

instance HasParser BigTuple where
  parser =
    string "( " *> parser <* optional nl_indent <* comma >>= \line_expr ->
    parser >>= \comma_sep_line_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \comma_sep_line_exprs_l ->
    nl_indent *> char ')' *>
    (return $ BT (line_expr, comma_sep_line_exprs, comma_sep_line_exprs_l))

instance HasParser List where
  parser = L <$> (char '[' *> optionMaybe parser <* char ']')

instance HasParser BigList where
  parser =
    string "[ " *> parser >>= \comma_sep_line_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \comma_sep_line_exprs_l ->
    nl_indent *> char ']' *>
    (return $ BL (comma_sep_line_exprs, comma_sep_line_exprs_l))

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
    ia_parser =
      parser >>= \identifier ->
      parser >>= \args ->
      return $ IA (identifier, args)

instance HasParser Arguments where
  parser = As <$> in_paren parser

instance HasParser IdentWithArgs where
  parser =
    ident_with_args_start_p >>= \ident_with_args_start ->
    parser >>= \arguments ->
    many1 lower_under >>= \string ->
    many (try empty_paren_or_args_and_string_pair) >>= \pairs ->
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

    empty_paren_or_args_and_string_pair :: Parser (EmptyParenOrArgs, String)
    empty_paren_or_args_and_string_pair =
      empty_paren_or_args_p >>= \empty_paren_or_args ->
      many1 lower_under >>= \string ->
      return (empty_paren_or_args, string)

-- HasParser: PreFunc, PostFunc, BasicExpr, DotChange

instance HasParser PreFunc where
  parser = PF <$> parser <* char ':'

instance HasParser PreFuncApp where
  parser =
    parser >>= \pre_func -> parser >>= \pre_func_arg ->
    return $ PrFA (pre_func, pre_func_arg)

instance HasParser PreFuncArg where
  parser =
    PrFA1 <$> try parser <|> PoFA1 <$> try parser <|> PE1 <$> try parser <|>
    BE1 <$> parser

instance HasParser BasicExpr where
  parser =
    PFA <$> try parser <|> Lit1 <$> parser <|> Id1 <$> parser <|> T1 <$> parser <|>
    L1 <$> parser

instance HasParser PostFunc where
  parser =
    char '.' >>
    ( DC1 <$> parser <|>
      Id2 <$> parser <|>
      string "1st" *> return Dot1st <|>
      string "2nd" *> return Dot2nd <|>
      string "3rd" *> return Dot3rd <|>
      string "4th" *> return Dot4th <|>
      string "5th" *> return Dot5th
    )

instance HasParser PostFuncApp where
  parser =
    post_func_arg_p >>= \post_func_arg ->
    many1 parser >>= \post_funcs ->
    return $ PoFA (post_func_arg, post_funcs)
    where
    post_func_arg_p :: Parser PostFuncArg
    post_func_arg_p = PE2 <$> try parser <|> BE2 <$> parser

instance HasParser DotChange where
  parser = 
    try (string "change{") *> field_change_p >>= \field_change ->
    many (comma *> field_change_p) <* char '}' >>= \field_changes ->
    return $ DC (field_change, field_changes)
    where
    field_change_p :: Parser FieldChange
    field_change_p = 
      field_p >>= \field ->
      string " = " *> parser >>= \line_expr ->
      return $ FC (field, line_expr)

    field_p :: Parser Field
    field_p = 
      Id3 <$> parser <|>
      string "1st" *> return First <|>
      string "2nd" *> return Second <|>
      string "3rd" *> return Third <|>
      string "4th" *> return Fourth <|>
      string "5th" *> return Fifth 

-- HasParser: OpExpr

instance HasParser OpExpr where
  parser = SOE2 <$> try parser <|> BOE1 <$> try parser <|> COE1 <$> parser

instance HasParser OpExprStart where
  parser = 
    parser >>= \op_arg1 ->
    char ' ' *> parser >>= \op1 ->
    many (try op_arg_op_p) >>= \op_arg_ops ->
    return $ OES (op_arg1, op1, op_arg_ops)
    where
    op_arg_op_p :: Parser (OpArg, Op)
    op_arg_op_p =
      char ' ' *> parser >>= \op_arg ->
      char ' ' *> parser >>= \op ->
      return (op_arg, op)

instance HasParser SimpleOpExpr where
  parser = 
    parser >>= \oes ->
    char ' ' *> parser >>= \soee ->
    return $ SOE (oes, soee)

instance HasParser SimpleOpExprEnd where
  parser = SFE2 <$> try parser <|> OA1 <$> parser

instance HasParser BigOpExpr where
  parser = BOE_1_ <$> try parser <|> BOE_2_ <$> parser

instance HasParser BigOpExpr1 where
  parser = 
    many1 (try $ parser <* nl_indent) >>= \oess ->
    big_op_expr_end_p >>= \boee ->
    return $ BOE_1 (oess, boee)
    where
    big_op_expr_end_p :: Parser BigOpExprEnd
    big_op_expr_end_p =
      optionMaybe (try $ parser <* char ' ') >>= \maybe_oes ->
      big_op_expr_end_p2 >>= \soee2 ->
      return $ BOEE (maybe_oes, soee2)

    big_op_expr_end_p2 :: Parser BigOpExprEnd2
    big_op_expr_end_p2 =
      BFE1 <$> try parser <|> SFE3 <$> try parser <|> OA2 <$> parser

instance HasParser BigOpExpr2 where
  parser = 
    parser <* char ' ' >>= \oes ->
    parser >>= \bfe ->
    return $ BOE_2 (oes, bfe)

instance HasParser CasesOpExpr where
  parser = 
    parser >>= \oes ->
    many (try $ nl_indent *> parser) >>= \oess ->
    (nl_indent <|> one_space) *> parser >>= \cases_func_expr ->
    return $ COE (oes, oess, cases_func_expr)

instance HasParser OpArg where
  parser = PE3 <$> try parser <|> NPOA2 <$> parser

instance HasParser NoParenOpArg where
  parser =  
    PoF <$> parser <|>
    PrFA2 <$> try parser <|>
    PoFA2 <$> try parser <|>
    PrF <$> try parser <|>
    BE3 <$> parser 

instance HasParser Op where
  parser =  
    try (string "->") *> return RightApp <|>
    try (string "<-") *> return LeftApp <|>
    string "o>" *> return RightComp <|>
    try (string "<o") *> return LeftComp <|>
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
  parser = SFE4 <$> try parser <|> BFE2 <$> try parser <|> CFE1 <$> parser

instance HasParser SimpleFuncExpr where
  parser = 
    parser >>= \parameters ->
    string " => " *> parser >>= \simple_func_body ->
    return $ SFE (parameters, simple_func_body)

instance HasParser BigFuncExpr where
  parser = 
    parser >>= \parameters ->
    string " =>" *> nl_indent *> big_func_body_p >>= \big_func_body ->
    return $ BFE (parameters, big_func_body)
    where
    big_func_body_p :: Parser BigFuncBody
    big_func_body_p = BOE2 <$> try parser <|> SFB1 <$> parser

instance HasParser Parameters where
  parser = 
    OneParam <$> parser <|>
    ( char '(' *> parser >>= \identifier ->
      many1 (comma *> parser) <* char ')' >>= \identifiers ->
      return $ ManyParams (identifier, identifiers)
    )

instance HasParser SimpleFuncBody where
  parser = SOE5 <$> try parser <|> NPOA3 <$> parser

instance HasParser CasesFuncExpr where --TODO last indentation rule
  parser =
    parser >>= \cases_params ->
    string " =>" *> many1 parser >>= \cases ->
    optionMaybe parser >>= \maybe_end_case ->
    return $ CFE (cases_params, cases, maybe_end_case)

instance HasParser CasesParams where
  parser = 
    OneCParam <$> cases_param_p <|>
    ( char '(' *> cases_param_p >>= \cases_param ->
      many1 (comma *> cases_param_p) <* char ')' >>= \cases_params ->
      return $ ManyCParams (cases_param, cases_params)
    )
    where
    cases_param_p :: Parser CasesParam
    cases_param_p = try (string "cases") *> return CasesKeyword <|> Id4 <$> parser

instance HasParser Case where
  parser = 
    try (nl_indent *> parser) >>= \matching ->
    string " =>" *> parser >>= \case_body ->
    return $ Ca (matching, case_body) 

instance HasParser EndCase where
  parser = 
    nl_indent *> string "... =>" *> parser >>= \case_body ->
    return $ EC case_body

instance HasParser Matching where
  parser = 
    Lit2 <$> parser <|> PFM <$> try pre_func_matching_p <|> Id5 <$> parser <|>
    TM1 <$> parser <|> LM1 <$> parser
    where
    pre_func_matching_p :: Parser (PreFunc, Matching)
    pre_func_matching_p =
      parser >>= \pre_func ->
      parser >>= \matching ->
      return (pre_func, matching)

instance HasParser TupleMatching where
  parser = 
    char '(' *> parser >>= \matching ->
    many1 (comma *> parser) <* char ')' >>= \matchings ->
    return $ TM (matching, matchings)

instance HasParser ListMatching where
  parser = 
    LM <$> (char '[' *> optionMaybe inside_p <* char ']')
    where
    inside_p :: Parser (Matching, [Matching])
    inside_p =
      parser >>= \matching ->
      many (comma *> parser) >>= \matchings ->
      return (matching, matchings)

instance HasParser CaseBody where
  parser = 
    modifyState (+ 1) >>
    (one_space <|> nl_indent) *> case_body_start_p >>= \case_body_start ->
    optionMaybe (try parser) >>= \maybe_where_expr ->
    modifyState (\x -> x - 1) >>
    return (CB (case_body_start, maybe_where_expr))
    where
    case_body_start_p :: Parser CaseBodyStart
    case_body_start_p = BOE3 <$> try parser <|> SFB2 <$> parser

-- HasParser: ValueDef, WhereExpr

instance HasParser ValueDef where
  parser = 
    indent *> parser >>= \identifier ->
    modifyState (+1) >>
    nl_indent *> string ": " *> parser >>= \type_ ->
    nl_indent *> string "= " *> modifyState (+1) *> parser >>= \value_expr ->
    optionMaybe (try parser) >>= \maybe_where_expr ->
    modifyState (\x -> x - 2) >>
    string "\n\n" >>
    return (VD (identifier, type_, value_expr, maybe_where_expr))

instance HasParser ValueExpr where
  parser = 
    OE <$> try parser <|>
    FE <$> try parser <|>
    BT1 <$> try parser <|>
    BL1 <$> try parser <|>
    NPOA4 <$> parser

instance HasParser GroupedValueDefs where
  parser = 
    indent *> parser >>= \identifier ->
    many1 (comma *> parser) >>= \identifiers -> 
    modifyState (+1) >>
    nl_indent *> string ": " *> types_p >>= \types ->
    nl_indent *> string "= " *> parser >>= \comma_sep_line_exprs ->
    many (try (nl_indent *> comma) *> parser) >>= \comma_sep_line_exprs_l ->
    modifyState (\x -> x - 1) >>
    string "\n\n" >>
    return (GVDs
      ( identifier, identifiers, types, comma_sep_line_exprs
      , comma_sep_line_exprs_l
      )) 
    where
    types_p :: Parser Types
    types_p =
      ( parser >>= \type_ ->
        many1 (comma *> parser) >>= \types ->
        return $ Ts (type_, types)
      ) <|> All <$> (string "all " *> parser)

instance HasParser WhereExpr where
  parser = 
    nl_indent *> string "where\n" *> (WE <$> many1 where_def_expr_p)
    where
    where_def_expr_p :: Parser WhereDefExpr
    where_def_expr_p = VD1 <$> try parser <|> GVD <$> parser

-- HasParser: Type

instance HasParser Type where
  parser = 
    optionMaybe (try parser) >>= \maybe_condition ->
    parser >>= \simple_type ->
    return $ Ty (maybe_condition, simple_type)

instance HasParser SimpleType where
  parser = 
    FT1 <$> try parser <|> PT1 <$> try parser <|> TA1 <$> try parser <|>
    TId1 <$> try parser <|> TV1 <$> parser

instance HasParser TypeId where
  parser = 
    upper >>= \u1 ->
    many1 (upper <|> lower) >>= \upper_lowers ->
    return $ TId $ u1 : upper_lowers

instance HasParser TypeVar where
  parser = TV <$> upper

instance HasParser FuncType where
  parser = 
    parser >>= \param_types ->
    string " => " *> parser >>= \one_type ->
    return $ FT (param_types, one_type)

instance HasParser ParamTypes where
  parser =
    OT <$> try parser <|> ManyTs <$> many_ts_p
    where
    many_ts_p :: Parser (SimpleType, [SimpleType])
    many_ts_p =
      char '(' *> parser >>= \simple_type ->
      many1 (comma *> parser) <* char ')' >>= \simple_types ->
      return (simple_type, simple_types)

instance HasParser OneType where
  parser =
    TA2 <$> try parser <|> FT2 <$> try (char '(' *> parser <* char ')') <|>
    PT2 <$> try parser <|> TId2 <$> try parser <|> TV2 <$> parser

instance HasParser ProdType where
  parser =
    parser >>= \field_type ->
    many1 (try (string " x ") *> parser) >>= \field_types ->
    return $ PT (field_type, field_types)

instance HasParser FieldType where
  parser =
    IPT <$> try in_paren_t_p <|> TA3 <$> try parser <|>
    TId3 <$> try parser <|> TV3 <$> parser
    where
    in_paren_t_p :: Parser InParenT
    in_paren_t_p = char '(' *> (FT3 <$> try parser <|> PT3 <$> parser) <* char ')'

instance HasParser TypeApp where
  parser =
    TIWA1 <$> try tiwa_p <|> TIPTI <$> tipti_p <|> TITIP <$> titip_p
    where
    tiwa_p :: Parser (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) 
    tiwa_p =
      optionMaybe parser >>= \maybe_types_in_paren1 ->
      parser >>= \type_id_with_args ->
      optionMaybe parser >>= \maybe_types_in_paren2 ->
      return (maybe_types_in_paren1, type_id_with_args, maybe_types_in_paren2)

    tipti_p :: Parser (TypesInParen, TypeId, Maybe TypesInParen)
    tipti_p = 
      parser >>= \types_in_paren ->
      parser >>= \type_id ->
      optionMaybe parser >>= \maybe_types_in_paren ->
      return (types_in_paren, type_id, maybe_types_in_paren)

    titip_p :: Parser (TypeId, TypesInParen)
    titip_p = 
      parser >>= \type_id ->
      parser >>= \types_in_paren ->
      return (type_id, types_in_paren)

instance HasParser TypeIdWithArgs where
  parser =
    parser >>= \type_id ->
    many1 (try types_in_paren_and_string_p) >>= \tip_string_pairs ->
    return $ TIWA (type_id, tip_string_pairs)
    where
    types_in_paren_and_string_p :: Parser (TypesInParen, String)
    types_in_paren_and_string_p = 
      parser >>= \types_in_paren ->
      many1 (lower <|> upper) >>= \string ->
      return (types_in_paren, string)

instance HasParser TypesInParen where
  parser =
    char '(' *> parser >>= \simple_type ->
    many (comma *> parser) <* char ')' >>= \simple_types ->
    return $ TIP (simple_type, simple_types)

instance HasParser Condition where
  parser = Co <$> (parser <* string " ==> ")

-- HasParser: TypeDef, TypeNickname

instance HasParser TypeDef where
  parser = TTD1 <$> parser <|> OTD1 <$> parser

instance HasParser TupleTypeDef where
  parser =
    string "tuple_type " *> parser >>= \type_name ->
    string "\nvalue ("  *> parser >>= \identifier ->
    many1 (comma *> parser) >>= \identifiers ->
    string ") : " *> parser >>= \prod_type ->
    string "\n\n" *> return (TTD (type_name, identifier, identifiers, prod_type))

instance HasParser TypeName where
  parser =
    optionMaybe parser >>= \maybe_params_in_paren1 ->
    parser >>= \middle_type_name ->
    optionMaybe parser >>= \maybe_params_in_paren2 ->
    return $ TN (maybe_params_in_paren1, middle_type_name, maybe_params_in_paren2)

instance HasParser MiddleTypeName where
  parser = TIWP1 <$> try parser <|> TId4 <$> parser

instance HasParser TypeIdWithParams where
  parser =
    parser >>= \type_id ->
    many1 (try params_in_paren_and_string_p) >>= \pip_string_pairs ->
    return $ TIWP (type_id, pip_string_pairs)
    where
    params_in_paren_and_string_p :: Parser (ParamsInParen, String)
    params_in_paren_and_string_p = 
      parser >>= \params_in_paren ->
      many1 (lower <|> upper) >>= \string ->
      return (params_in_paren, string)

instance HasParser ParamsInParen where
  parser =
    char '(' *> parser >>= \type_var ->
    many (comma *> parser) <* char ')' >>= \type_vars ->
    return $ PIP (type_var, type_vars)

instance HasParser OrTypeDef where
  parser =
    string "or_type " *> parser >>= \type_name ->
    string "\nvalues "  *> parser >>= \identifier ->
    optionMaybe (char ':' *> parser) >>= \maybe_simple_type ->
    many1 ident_maybe_simple_type_p >>= \ident_maybe_simple_types ->
    string "\n\n" *>
    return 
      (OTD (type_name, identifier, maybe_simple_type, ident_maybe_simple_types))
    where
    ident_maybe_simple_type_p :: Parser (Identifier, Maybe SimpleType)
    ident_maybe_simple_type_p =
      string " | " *> parser >>= \identifier ->
      optionMaybe (char ':' *> parser) >>= \maybe_simple_type ->
      return (identifier, maybe_simple_type)

instance HasParser TypeNickname where
  parser =
    string "type_nickname " *> parser >>= \type_name ->
    string " = " *> parser >>= \simple_type ->
    string "\n\n" *> return (TNN (type_name, simple_type))

-- HasParser: TypePropDef

instance HasParser TypePropDef where
  parser = APD1 <$> try parser <|> RPD1 <$> parser

instance HasParser AtomPropDef where
  parser = 
    parser >>= \prop_name_line ->
    string "\nvalue\n  " *> parser >>= \identifier ->
    string " : " *> parser >>= \simple_type ->
    string "\n\n" *> return (APD (prop_name_line, identifier, simple_type))

instance HasParser RenamingPropDef where
  parser =
    parser >>= \prop_name_line ->
    string "\nequivalent\n  " *> parser >>= \prop_name ->
    many (comma *> parser) >>= \prop_names ->
    string "\n\n" *> return (RPD (prop_name_line, prop_name, prop_names))

instance HasParser PropNameLine where
  parser = PNL <$> (string "type_proposition " *> parser)

instance HasParser PropName where
  parser =
    NPStart1 <$> np_start_p <|> PIPStart1 <$> pip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, ParamsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try np_pip_p) >>= \np_pips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_pips, maybe_name_part)

    np_pip_p :: Parser (NamePart, ParamsInParen)
    np_pip_p =
      parser >>= \name_part ->
      parser >>= \params_in_paren ->
      return (name_part, params_in_paren)

    pip_start_p :: Parser ([(ParamsInParen, NamePart)], Maybe ParamsInParen)
    pip_start_p = 
      many1 (try pip_np_p) >>= \pip_nps ->
      optionMaybe parser >>= \maybe_params_in_paren ->
      return (pip_nps, maybe_params_in_paren)

    pip_np_p :: Parser (ParamsInParen, NamePart)
    pip_np_p =
      parser >>= \params_in_paren ->
      parser >>= \name_part ->
      return (params_in_paren, name_part)

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
    string "\nproof\n  " *> parser >>= \id ->
    string " = " *> parser >>= \ve ->
    string "\n\n" *> return (TT (pps, maybe_pps, id, ve))

instance HasParser PropNameSub where
  parser = 
    NPStart2 <$> np_start_p <|> TIPStart <$> tip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, TypesInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try np_tip_p) >>= \np_tips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_tips, maybe_name_part)

    np_tip_p :: Parser (NamePart, TypesInParen)
    np_tip_p =
      parser >>= \name_part ->
      parser >>= \types_in_paren ->
      return (name_part, types_in_paren)

    tip_start_p :: Parser ([(TypesInParen, NamePart)], Maybe TypesInParen)
    tip_start_p = 
      many1 (try tip_np_p) >>= \tip_nps ->
      optionMaybe parser >>= \maybe_types_in_paren ->
      return (tip_nps, maybe_types_in_paren)

    tip_np_p :: Parser (TypesInParen, NamePart)
    tip_np_p =
      parser >>= \types_in_paren ->
      parser >>= \name_part ->
      return (types_in_paren, name_part)
 
-- for literals. Had to copy from Text.Parsec.Token source code
-- because I didn't want spaces after the char and string literals ...

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
