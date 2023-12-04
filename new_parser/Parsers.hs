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
type ParserState = (IndentationLevel, Bool)

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

(comma, underscore, lower_under, in_paren, nl_indent, one_space, digits) =
  (string ", ", char '_', lower <|> underscore, \a -> char '(' *> a <* char ')'
  , char '\n' *> indent, char ' ' *> return (), many1 digit
  )
  ::
  ( Parser String, Parser Char, Parser Char, Parser a -> Parser a
  , Parser (), Parser (), Parser String
  ) 

indent :: Parser ()
indent = getState >>= \(il, _) -> string (concat $ replicate il "  ") >> return ()

par_lower_unders = 
  try (string "()") >>= \par ->
  many1 lower_under >>= \lowers_unders ->
  return $ par ++ lowers_unders

-- helper op

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

instance HasParser ParenExprInside where
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
  parser = SFE2 <$> try parser <|> SOE2 <$> try parser <|> NPOA1 <$> parser

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

-- HasParser: PreFunc, PostFunc, BasicExpr, Change

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
    PFA <$> try parser <|> SI1 <$> try parser <|> Lit1 <$> parser <|>
    Id1 <$> parser <|> T1 <$> parser <|> L1 <$> parser

instance HasParser PostFunc where
  parser = char '.' *> (C1 <$> parser <|> Id2 <$> parser <|> SI2 <$> parser)

instance HasParser SpecialId where
  parser =
    string "1st" *> return First <|> string "2nd" *> return Second <|>
    string "3rd" *> return Third <|> string "4th" *> return Fourth <|>
    string "5th" *> return Fifth 

instance HasParser PostFuncApp where
  parser =
    post_func_arg_p >>= \post_func_arg ->
    many1 parser >>= \post_funcs ->
    return $ PoFA (post_func_arg, post_funcs)
    where
    post_func_arg_p :: Parser PostFuncArg
    post_func_arg_p = PE2 <$> try parser <|> BE2 <$> parser

instance HasParser Change where
  parser = 
    try (string "change{") *> field_change_p >>= \field_change ->
    many (comma *> field_change_p) <* char '}' >>= \field_changes ->
    return $ C (field_change, field_changes)
    where
    field_change_p :: Parser FieldChange
    field_change_p = 
      field_p >>= \field ->
      string " = " *> parser >>= \line_expr ->
      return $ FC (field, line_expr)

    field_p :: Parser Field
    field_p = Id3 <$> parser <|> SI3 <$> parser

-- HasParser: OpExpr

instance HasParser OpExpr where
  parser = BOE1 <$> try parser <|> SOE3 <$> try parser

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
  parser = SFE3 <$> try parser <|> OA1 <$> parser

instance HasParser BigOpExpr where
  parser =
    parser >>= \oes ->
    parser >>= \cont ->
    return $ BOE (oes, cont)

instance HasParser Continuation where
  parser = OC1 <$> parser <|> FnC <$> parser

instance HasParser OpContinuation where
  parser =
    not_in_equal_line >>
    nl_indent *> many (try $ parser <* nl_indent) >>= \oess-> 
    optionMaybe (try parser <* char ' ') >>= \maybe_oes ->
    parser >>= \oce ->
    return $ OC (oess, maybe_oes, oce)

instance HasParser OpContEnd where
  parser = FE1 <$> try parser <|> OA2 <$> parser

instance HasParser FuncContinuation where
  parser = char ' ' *> (BFE1 <$> try parser <|> CFE1 <$> parser)
  
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
  parser = SFE4 <$> try parser <|> BFE2 <$> try parser <|> CFE2 <$> parser

instance HasParser SimpleFuncExpr where
  parser = 
    parser >>= \parameters ->
    string " =>" *> parser >>= \simple_func_body ->
    return $ SFE (parameters, simple_func_body)

instance HasParser SimpleFuncBody where
  parser = char ' ' *> (SOE4 <$> try parser <|> NPOA3 <$> parser)

instance HasParser BigFuncExpr where
  parser = 
    parser >>= \parameters ->
    string " =>" *> parser >>= \big_func_body ->
    return $ BFE (parameters, big_func_body)

instance HasParser BigFuncBody where
  parser = nl_indent *> (OE1 <$> try parser <|> NPOA4 <$> parser)

instance HasParser Parameters where
  parser = 
    OneParam <$> parser <|>
    ( char '(' *> parser >>= \identifier ->
      many1 (comma *> parser) <* char ')' >>= \identifiers ->
      return $ ManyParams (identifier, identifiers)
    )

instance HasParser CasesFuncExpr where 
  parser =
    parser >>= \cases_params ->
    string " =>" *>
    are_we_in_equal_line >>= \answer ->
    inc_il_if_false answer *>
    many1 parser >>= \cases ->
    optionMaybe parser >>= \maybe_end_case ->
    dec_il_if_false answer *>
    return (CFE (cases_params, cases, maybe_end_case))

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
    try (nl_indent *> string "... =>") *> parser >>= \case_body ->
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
    increase_il 1 >>
    case_body_start_p >>= \case_body_start ->
    optionMaybe (try parser) >>= \maybe_where_expr ->
    decrease_il 1 >>
    return (CB (case_body_start, maybe_where_expr))
    where
    case_body_start_p :: Parser CaseBodyStart
    case_body_start_p = SFB1 <$> parser <|> BFB1 <$> parser

-- HasParser: ValueDef, WhereExpr

instance HasParser ValueDef where
  parser = 
    indent *> parser >>= \identifier ->
    increase_il 1 >>
    nl_indent *> string ": " *> parser >>= \type_ ->
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
    NPOA5 <$> parser

instance HasParser GroupedValueDefs where
  parser = 
    indent *> parser >>= \identifier ->
    many1 (comma *> parser) >>= \identifiers -> 
    increase_il 1 >>
    nl_indent *> string ": " *> types_p >>= \types ->
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
      ( parser >>= \type_ ->
        many1 (comma *> parser) >>= \types ->
        return $ Ts (type_, types)
      ) <|> All <$> (string "all " *> parser)

instance HasParser WhereExpr where
  parser = 
    nl_indent *> string "where\n" *> (WE <$> many1 where_def_expr_p)
    where
    where_def_expr_p :: Parser WhereDefExpr
    where_def_expr_p = VD1 <$> try parser <|> GVDs1 <$> parser

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

    tipti_p :: Parser (TypesInParen, TypeIdOrVar, Maybe TypesInParen)
    tipti_p = 
      parser >>= \types_in_paren ->
      parser >>= \type_id_or_var ->
      optionMaybe parser >>= \maybe_types_in_paren ->
      return (types_in_paren, type_id_or_var, maybe_types_in_paren)

    titip_p :: Parser (TypeIdOrVar, TypesInParen)
    titip_p = 
      parser >>= \type_id_or_var ->
      parser >>= \types_in_paren ->
      return (type_id_or_var, types_in_paren)

instance HasParser TypeIdWithArgs where
  parser =
    parser >>= \tid ->
    many1 (try types_in_paren_and_string_p) >>= \tip_string_pairs ->
    return $ TIWA (tid, tip_string_pairs)
    where
    types_in_paren_and_string_p :: Parser (TypesInParen, String)
    types_in_paren_and_string_p = 
      parser >>= \types_in_paren ->
      many1 (lower <|> upper) >>= \string ->
      return (types_in_paren, string)

instance HasParser TypeIdOrVar where
  parser = TId4 <$> try parser <|> TV4 <$> parser

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
    string "\nvalue\n  ("  *> parser >>= \identifier ->
    many1 (comma *> parser) >>= \identifiers ->
    string ") : " *> parser >>= \prod_type ->
    return $ TTD (type_name, identifier, identifiers, prod_type)

instance HasParser TypeName where
  parser =
    optionMaybe parser >>= \maybe_pip1 ->
    parser >>= \tid ->
    many (try params_in_paren_and_string_p) >>= \pip_string_pairs ->
    optionMaybe parser >>= \maybe_pip2 ->
    return $ TN (maybe_pip1, tid, pip_string_pairs, maybe_pip2)
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
    string "\nvalues\n  "  *> parser >>= \identifier ->
    optionMaybe (char ':' *> parser) >>= \maybe_simple_type ->
    many1 ident_maybe_simple_type_p >>= \ident_maybe_simple_types ->
    return $
      OTD (type_name, identifier, maybe_simple_type, ident_maybe_simple_types)
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
    return $ TNN (type_name, simple_type)

-- HasParser: TypePropDef

instance HasParser TypePropDef where
  parser = APD1 <$> try parser <|> RPD1 <$> parser

instance HasParser AtomPropDef where
  parser = 
    parser >>= \prop_name_line ->
    string "\nvalue\n  " *> parser >>= \identifier ->
    string " : " *> parser >>= \simple_type ->
    return $ APD (prop_name_line, identifier, simple_type)

instance HasParser RenamingPropDef where
  parser =
    parser >>= \prop_name_line ->
    string "\nequivalent\n  " *> parser >>= \prop_name ->
    many (comma *> parser) >>= \prop_names ->
    return $ RPD (prop_name_line, prop_name, prop_names)

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
    maybe_op_id_p >>= \maybe_op_id ->
    parser >>= \ttve ->
    return $ TT (pps, maybe_pps, id, maybe_op_id, ttve)
    where
    op_id_p :: Parser (Op, Identifier)
    op_id_p =
      char ' ' *> parser >>= \op ->
      char ' ' *> parser <* notFollowedBy (char '\n') >>= \id ->
      return (op, id)

    maybe_op_id_p :: Parser (Maybe (Op, Identifier))
    maybe_op_id_p =
      Just <$> (try $ op_id_p <* string " =" <* followed_by_space_or_nl) <|>
      (string " =" *> return Nothing)
      where
      followed_by_space_or_nl = lookAhead (char ' ' <|> char '\n')

instance HasParser PropNameSub where
  parser = 
    NPStart2 <$> np_start_p <|> PSIPStart <$> psip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, ParamSubsInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 (try np_psip_p) >>= \np_psips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_psips, maybe_name_part)

    np_psip_p :: Parser (NamePart, ParamSubsInParen)
    np_psip_p =
      parser >>= \name_part ->
      parser >>= \param_subs_in_paren ->
      return (name_part, param_subs_in_paren)

    psip_start_p :: Parser ([(ParamSubsInParen, NamePart)], Maybe ParamSubsInParen)
    psip_start_p = 
      many1 (try psip_np_p) >>= \psip_nps ->
      optionMaybe parser >>= \maybe_param_subs_in_paren ->
      return (psip_nps, maybe_param_subs_in_paren)

    psip_np_p :: Parser (ParamSubsInParen, NamePart)
    psip_np_p =
      parser >>= \param_subs_in_paren ->
      parser >>= \name_part ->
      return (param_subs_in_paren, name_part)

instance HasParser ParamSubsInParen where
  parser =
    char '(' *> parser >>= \param_sub ->
    many (comma *> parser) <* char ')' >>= \param_subs ->
    return $ PSIP (param_sub, param_subs)

instance HasParser ParamSub where
  parser = TF1 <$> try parser <|> ST1 <$> parser

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
    tf2_p = 
      string "()" *> parser >>= \tid ->
      bool_p >>= \b ->
      return (tid, b)

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
  parser =
    BOCE <$> (try (increase_il 2 >> nl_indent) *> parser <* decrease_il 2) <|>
    LE <$> (char ' ' *> parser)

instance HasParser BigOrCasesExpr where
  parser = 
    BOE4 <$> try parser <|> BFE3 <$> try parser <|> CFE3 <$> try parser <|>
    BT2 <$> try parser <|> BL2 <$> parser

-- Program

instance HasParser Program where
  parser =
    parser >>= \pp ->
    many (try $ string "\n\n" *> parser) >>= \pps ->
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
