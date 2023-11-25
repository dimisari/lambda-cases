{-# LANGUAGE LambdaCase,
TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

module Parsers where

import Text.Parsec hiding (parse)
import Text.Parsec.Token hiding (comma)

import Text.Parsec.Language (haskellDef)

import ASTTypes

type Parser = Parsec String Int

haskell_lexer = makeTokenParser haskellDef -- for copying some haskell parsers 

class HasParser a where
  parser :: Parser a

-- helper parsers

(comma, underscore, lower_under, in_paren, nl_indent, one_space) =
  (string ", ", char '_', lower <|> underscore, \a -> char '(' *> a <* char ')'
  , char '\n' *> indent, char ' ' *> return ()
  )
  ::
  ( Parser String, Parser Char, Parser Char, Parser a -> Parser a
  , Parser (), Parser ()
  ) 

indent :: Parser ()
indent = getState >>= \il -> string (concat $ replicate il "  ") >> return ()

par_lower_unders = 
  string "()" >>= \par ->
  many1 lower_under >>= \lowers_unders ->
  return $ par ++ lowers_unders

-- HasParser: Literal

instance HasParser Integer where
  parser =
    optionMaybe (char '-') >>= \case
      Nothing -> natural haskell_lexer
      Just _ -> fmap (0 -) $ natural haskell_lexer
  
instance HasParser Double where
  parser =
    optionMaybe (char '-') >>= \case
      Nothing -> float haskell_lexer
      Just _ -> fmap (0 -) $ float haskell_lexer
  
instance HasParser Char where
  parser = charLiteral haskell_lexer
  
instance HasParser String where
  parser = stringLiteral haskell_lexer
  
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

instance HasParser OpOrFuncExpr where
  parser = SOE1 <$> parser <|> OEFE1 <$> parser <|> SFE1 <$> parser

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
  parser = NPOA1 <$> parser <|> OOFE <$> parser

instance HasParser BigTuple where
  parser =
    char '(' *> parser <* optional nl_indent <* comma >>= \line_expr ->
    parser >>= \comma_sep_line_exprs ->
    many (nl_indent *> comma *> parser) >>= \comma_sep_line_exprs_l ->
    nl_indent *> char ')' *>
    (return $ BT (line_expr, comma_sep_line_exprs, comma_sep_line_exprs_l))

instance HasParser List where
  parser = L <$> (char '[' *> optionMaybe parser <* char ']')

instance HasParser BigList where
  parser =
    char '[' *> parser >>= \comma_sep_line_exprs ->
    many (nl_indent *> comma *> parser) >>= \comma_sep_line_exprs_l ->
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
    many empty_paren_or_args_and_string_pair >>= \pairs ->
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
      string "()" *> return EmptyParen <|> As1 <$> parser

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
  parser = BE1 <$> parser <|> PE1 <$> parser <|> PrFA1 <$> parser

instance HasParser BasicExpr where
  parser =
    Lit1 <$> parser <|> Id1 <$> parser <|> T1 <$> parser <|> L1 <$> parser <|>
    PFA <$> parser <|> PoFA1 <$> parser

instance HasParser PostFunc where
  parser =
    char '.' >>
    ( Id2 <$> parser <|>
      string "1st" *> return Dot1st <|>
      string "2nd" *> return Dot2nd <|>
      string "3rd" *> return Dot3rd <|>
      string "4th" *> return Dot4th <|>
      string "5th" *> return Dot5th <|>
      DC1 <$> parser
    )

instance HasParser PostFuncApp where
  parser =
    post_func_arg_p >>= \post_func_arg ->
    parser >>= \post_func ->
    return $ PoFA (post_func_arg, post_func)
    where
    post_func_arg_p :: Parser PostFuncArg
    post_func_arg_p = PE2 <$> parser <|> BE2 <$> parser

instance HasParser DotChange where
  parser = 
    string "change{" *> field_change_p >>= \field_change ->
    many (comma *> field_change_p) >>= \field_changes ->
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
  parser = 
    SOE2 <$> parser <|> OEFE2 <$> parser <|> BOE1 <$> parser <|>
    COE1 <$> parser

instance HasParser SimpleOpExpr where
  parser = 
    parser >>= \op_arg ->
    many1 op_op_arg >>= \op_op_args ->
    return $ SOE (op_arg, op_op_args)
    where
    op_op_arg :: Parser (Op, OpArg)
    op_op_arg =
      char ' ' *> parser >>= \op ->
      char ' ' *> parser >>= \op_arg ->
      return (op, op_arg)

instance HasParser OpExprFuncEnd where
  parser = 
    parser >>= \simple_op_expr ->
    char ' ' *> parser >>= \op ->
    char ' ' *> parser >>= \simple_func_expr ->
    return $ OEFE (simple_op_expr, op, simple_func_expr)

instance HasParser BigOpExpr where
  parser = 
    parser >>= \op_expr_line ->
    many (nl_indent *> parser) >>= \op_expr_lines ->
    nl_indent *> big_op_expr_end_p >>= \big_op_expr_end ->
    return $ BOE (op_expr_line, op_expr_lines, big_op_expr_end)
    where
    big_op_expr_end_p :: Parser BigOpExprEnd
    big_op_expr_end_p =
      OA1 <$> parser <|> SOE3 <$> parser <|>
      ( optionMaybe (parser <* char ' ') >>= \maybe_op_expr_line ->
        big_op_func_end_p >>= \big_op_func_end ->
        return $ BOFE (maybe_op_expr_line, big_op_func_end)
      )

    big_op_func_end_p :: Parser BigOpFuncEnd
    big_op_func_end_p = 
      SFE2 <$> parser <|> BFE1 <$> parser

instance HasParser CasesOpExpr where
  parser = 
    parser >>= \op_expr_line ->
    many (nl_indent *> parser) >>= \op_expr_lines ->
    (nl_indent <|> one_space) *> parser >>= \cases_func_expr ->
    return $ COE (op_expr_line, op_expr_lines, cases_func_expr)

instance HasParser OpExprLine where
  parser = 
    op_expr_line_start_p >>= \op_expr_line_start ->
    char ' ' *> parser >>= \op ->
    return $ OEL (op_expr_line_start, op)
    where
    op_expr_line_start_p :: Parser OpExprLineStart
    op_expr_line_start_p = 
      OA2 <$> parser <|> SOE4 <$> parser

instance HasParser OpArg where
  parser = NPOA2 <$> parser <|> PE3 <$> parser

instance HasParser NoParenOpArg where
  parser =  
    BE3 <$> parser <|> PrF <$> parser <|> PoF <$> parser <|> PrFA2 <$> parser

instance HasParser Op where
  parser =  
    string "->" *> return RightApp <|>
    string "<-" *> return LeftApp <|>
    string "o>" *> return RightComp <|>
    string "<o" *> return LeftComp <|>
    string "^" *> return Power <|>
    string "*" *> return Mult <|>
    string "/" *> return Div <|>
    string "+" *> return Plus <|>
    string "-" *> return Minus <|>
    string "=" *> return Equal <|>
    string "/=" *> return NotEqual <|>
    string ">" *> return Greater <|>
    string "<" *> return Less <|>
    string ">=" *> return GrEq <|>
    string "<=" *> return LeEq <|>
    string "&" *> return And <|>
    string "|" *> return Or <|>
    string ";>" *> return Use <|>
    string ";" *> return Then

-- HasParser: FuncExpr

instance HasParser FuncExpr where
  parser = SFE3 <$> parser <|> BFE2 <$> parser <|> CFE1 <$> parser

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
    big_func_body_p = SFB1 <$> parser <|> BOE2 <$> parser

instance HasParser Parameters where
  parser = 
    OneParam <$> parser <|>
    ( char '(' *> parser >>= \identifier ->
      many1 (comma *> parser) <* char ')' >>= \identifiers ->
      return $ ManyParams (identifier, identifiers)
    )

instance HasParser SimpleFuncBody where
  parser = NPOA3 <$> parser <|> SOE5 <$> parser <|> OEFE3 <$> parser

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
    cases_param_p = Id4 <$> parser <|> string "cases" *> return CasesKeyword

instance HasParser Case where
  parser = 
    nl_indent *> parser >>= \matching ->
    string " =>" *> modifyState (+ 1) *> parser >>= \case_body ->
    return $ Ca (matching, case_body) 

instance HasParser EndCase where
  parser = 
    nl_indent *> string "... =>" *> modifyState (+ 1) *> parser >>= \case_body ->
    return $ EC case_body

instance HasParser Matching where
  parser = 
    Lit2 <$> parser <|> Id5 <$> parser <|> PFM <$> pre_func_matching_p <|>
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
      many1 (comma *> parser) >>= \matchings ->
      return (matching, matchings)

instance HasParser CaseBody where
  parser = 
    (one_space <|> nl_indent) *> case_body_start_p >>= \case_body_start ->
    optionMaybe parser >>= \maybe_where_expr ->
    modifyState (\x -> x - 1) >>
    return (CB (case_body_start, maybe_where_expr))
    where
    case_body_start_p :: Parser CaseBodyStart
    case_body_start_p = SFB2 <$> parser <|> BOE3 <$> parser

-- HasParser: ValueDef, WhereExpr

instance HasParser ValueDef where
  parser = 
    indent *> parser >>= \identifier ->
    modifyState (+1) >>
    nl_indent *> string ": " *> parser >>= \type_ ->
    nl_indent *> string "= " *> parser >>= \value_expr ->
    modifyState (+1) >>
    optionMaybe parser >>= \maybe_where_expr ->
    modifyState (\x -> x - 2) >>
    return (VD (identifier, type_, value_expr, maybe_where_expr))

instance HasParser ValueExpr where
  parser = 
    NPOA4 <$> parser <|> OE <$> parser <|> FE <$> parser <|> BT1 <$> parser <|>
    BL1 <$> parser

instance HasParser GroupedValueDefs where
  parser = 
    indent *> parser >>= \identifier ->
    modifyState (+1) >>
    many1 (comma *> parser) >>= \identifiers -> 
    nl_indent *> string ": " *> types_p >>= \types ->
    nl_indent *> string "= " *> parser >>= \comma_sep_line_exprs ->
    many (nl_indent *> comma *> parser) >>= \comma_sep_line_exprs_l ->
    modifyState (\x -> x - 1) >>
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
    where_def_expr_p = VD1 <$> parser <|> GVD <$> parser

-- HasParser: Type

instance HasParser Type where
  parser = 
    optionMaybe parser >>= \maybe_condition ->
    parser >>= \simple_type ->
    return $ Ty (maybe_condition, simple_type)

instance HasParser SimpleType where
  parser = 
    TId1 <$> parser <|> TV1 <$> parser <|> FT1 <$> parser <|>
    PT1 <$> parser <|> TA1 <$> parser

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
    OT <$> parser <|> ManyTs <$> many_ts_p
    where
    many_ts_p :: Parser (SimpleType, [SimpleType])
    many_ts_p =
      char '(' *> parser >>= \simple_type ->
      many1 (comma *> parser) <* char ')' >>= \simple_types ->
      return (simple_type, simple_types)

instance HasParser OneType where
  parser =
    TId2 <$> parser <|> TV2 <$> parser <|> PT2 <$> parser <|>
    TA2 <$> parser <|> FT2 <$> (char '(' *> parser <* char ')')

instance HasParser ProdType where
  parser =
    parser >>= \field_type ->
    many1 (string " x " *> parser) >>= \field_types ->
    return $ PT (field_type, field_types)

instance HasParser FieldType where
  parser =
    TId3 <$> parser <|> TV3 <$> parser <|> TA3 <$> parser <|>
    IPT <$> in_paren_t_p
    where
    in_paren_t_p :: Parser InParenT
    in_paren_t_p = char '(' *> (FT3 <$> parser <|> PT3 <$> parser) <* char ')'

instance HasParser TypeApp where
  parser =
    TIWA1 <$> tiwa_p <|> TIPTI <$> tipti_p <|> TITIP <$> titip_p
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
    many1 types_in_paren_and_string_p >>= \tip_string_pairs ->
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
    string ") : "  *> parser >>= \prod_type ->
    return $ TTD (type_name, identifier, identifiers, prod_type)

instance HasParser TypeName where
  parser =
    optionMaybe parser >>= \maybe_params_in_paren1 ->
    parser >>= \middle_type_name ->
    optionMaybe parser >>= \maybe_params_in_paren2 ->
    return $ TN (maybe_params_in_paren1, middle_type_name, maybe_params_in_paren2)

instance HasParser MiddleTypeName where
  parser = TId4 <$> parser <|> TIWP1 <$> parser

instance HasParser TypeIdWithParams where
  parser =
    parser >>= \type_id ->
    many1 params_in_paren_and_string_p >>= \pip_string_pairs ->
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
    many1 (comma *> parser) <* char ')' >>= \type_vars ->
    return $ PIP (type_var, type_vars)

instance HasParser OrTypeDef where
  parser =
    string "or_type " *> parser >>= \type_name ->
    string "\nvalue "  *> parser >>= \identifier ->
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
    string "type_nickname" *> parser >>= \type_name ->
    string " = " *> parser >>= \simple_type ->
    return $ TNN (type_name, simple_type)

-- HasParser: TypePropDef

instance HasParser TypePropDef where
  parser = APD1 <$> parser <|> RPD1 <$> parser

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
      many1 np_pip_p >>= \np_pips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_pips, maybe_name_part)

    np_pip_p :: Parser (NamePart, ParamsInParen)
    np_pip_p =
      parser >>= \name_part ->
      parser >>= \params_in_paren ->
      return (name_part, params_in_paren)

    pip_start_p :: Parser ([(ParamsInParen, NamePart)], Maybe ParamsInParen)
    pip_start_p = 
      many1 pip_np_p >>= \pip_nps ->
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
    string "type_theorem " *> type_theo_start_p >>= \type_theo_start ->
    string "\nproof\n  " *> parser >>= \identifier ->
    string " = " *> parser >>= \value_expr ->
    return $ TT (type_theo_start, identifier, value_expr)
    where
    type_theo_start_p :: Parser TypeTheoStart
    type_theo_start_p = AP1 <$> parser <|> IP1 <$> parser

instance HasParser AtomicProp where
  parser = AP <$> parser

instance HasParser ImplicationProp where
  parser = 
    parser >>= \prop_name_sub1 ->
    string " => " *> parser >>= \prop_name_sub2 ->
    return $ IP (prop_name_sub1, prop_name_sub2)

instance HasParser PropNameSub where
  parser = 
    NPStart2 <$> np_start_p <|> TIPStart <$> tip_start_p
    where
    np_start_p :: Parser (Char, [(NamePart, TypesInParen)], Maybe NamePart)
    np_start_p = 
      upper >>= \u ->
      many1 np_tip_p >>= \np_tips ->
      optionMaybe parser >>= \maybe_name_part ->
      return (u, np_tips, maybe_name_part)

    np_tip_p :: Parser (NamePart, TypesInParen)
    np_tip_p =
      parser >>= \name_part ->
      parser >>= \types_in_paren ->
      return (name_part, types_in_paren)

    tip_start_p :: Parser ([(TypesInParen, NamePart)], Maybe TypesInParen)
    tip_start_p = 
      many1 tip_np_p >>= \tip_nps ->
      optionMaybe parser >>= \maybe_types_in_paren ->
      return (tip_nps, maybe_types_in_paren)

    tip_np_p :: Parser (TypesInParen, NamePart)
    tip_np_p =
      parser >>= \types_in_paren ->
      parser >>= \name_part ->
      return (types_in_paren, name_part)
