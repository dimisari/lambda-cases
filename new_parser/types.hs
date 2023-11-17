{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}

import Text.Parsec hiding (parse)
import Text.Parsec.Token hiding (comma)

import Text.Parsec.Language (haskellDef)
import Data.Functor((<&>))


-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  Int Int | R Double | Ch Char | S String 
  deriving Show

newtype Identifier = Id String
  deriving Show

newtype ParenExpr = PE OpOrFuncExpr
  deriving Show

data OpOrFuncExpr = 
  SOE1 SimpleOpExpr | OEFE1 OpExprFuncEnd | SFE1 SimpleFuncExpr
  deriving Show

newtype Tuple = T (LineExpr, CommaSepLineExprs)
  deriving Show

newtype CommaSepLineExprs = CSLE (LineExpr, [LineExpr])
  deriving Show

data LineExpr = 
  NPOA1 NoParenOpArg | OOFE OpOrFuncExpr
  deriving Show

newtype BigTuple = BT (LineExpr, CommaSepLineExprs, [CommaSepLineExprs])
  deriving Show

newtype List = L (Maybe CommaSepLineExprs)
  deriving Show

newtype BigList = BL (CommaSepLineExprs, [CommaSepLineExprs])
  deriving Show

data ParenFuncApp = 
  IWA1 (Maybe Arguments, IdentWithArgs, Maybe Arguments) |
  AI (Arguments, Identifier, Maybe Arguments) |
  IA (Identifier, Arguments)
  deriving Show

newtype Arguments = As CommaSepLineExprs
  deriving Show

newtype IdentWithArgs =
  IWA
  (IdentWithArgsStart, Arguments, String, [(EmptyParenOrArgs, String)], Maybe Char)
  deriving Show

newtype IdentWithArgsStart = IWAS String
  deriving Show

data EmptyParenOrArgs =
  EmptyParen | As1 Arguments
  deriving Show

-- Values: PreFunc, PostFunc, BasicExpr, DotChange

newtype PreFunc = PF Identifier 
  deriving Show

newtype PreFuncApp = PrFA (PreFunc, PreFuncArg)
  deriving Show

data PreFuncArg = 
  BE1 BasicExpr | PE1 ParenExpr | PrFA1 PreFuncApp
  deriving Show

data BasicExpr = 
  Lit1 Literal | Id1 Identifier | T1 Tuple | L1 List | PFA ParenFuncApp |
  PoFA1 PostFuncApp
  deriving Show

data PostFunc = 
  Id2 Identifier | Dot1st | Dot2nd | Dot3rd | Dot4th | Dot5th | DC1 DotChange
  deriving Show

newtype PostFuncApp = PoFA (PostFuncArg, PostFunc)
  deriving Show

data PostFuncArg = 
  PE2 ParenExpr | BE2 BasicExpr
  deriving Show

newtype DotChange = DC (FieldChange, [FieldChange])
  deriving Show

newtype FieldChange = FC (Field, LineExpr)
  deriving Show

data Field =
  Id3 Identifier | First | Second | Third | Fourth | Fifth 
  deriving Show

-- Values: OpExpr

data OpExpr = 
  SOE2 SimpleOpExpr | OEFE2 OpExprFuncEnd | BOE1 BigOpExpr | COE1 CasesOpExpr
  deriving Show

newtype SimpleOpExpr = SOE (OpArg, [(Op, OpArg)])
  deriving Show

newtype OpExprFuncEnd = OEFE (SimpleOpExpr, Op, SimpleFuncExpr)
  deriving Show

newtype BigOpExpr = BOE (OpExprLine, [OpExprLine], BigOpExprEnd)
  deriving Show

data BigOpExprEnd = 
  OA1 OpArg | SOE3 SimpleOpExpr | BOFE (Maybe OpExprLine, BigOpFuncEnd)
  deriving Show

data BigOpFuncEnd = 
  SFE2 SimpleFuncExpr | BFE1 BigFuncExpr
  deriving Show

newtype CasesOpExpr = COE (OpExprLine, [OpExprLine], CasesFuncExpr)
  deriving Show

newtype OpExprLine = OEL (OpExprLineStart, Op)
  deriving Show

data OpExprLineStart = 
  OA2 OpArg | SOE4 SimpleOpExpr
  deriving Show

data OpArg =
  NPOA2 NoParenOpArg | PE3 ParenExpr
  deriving Show

data NoParenOpArg =
  BE3 BasicExpr | PrF PreFunc | PoF PostFunc | PrFA2 PreFuncApp
  deriving Show

data Op = 
  RightApp | LeftApp | RightComp | LeftComp | Power | Mult | Div | Plus | 
  Minus | Equal | NotEqual | Greater | Less | GrEq | LeEq | And | Or | Use | Then
  deriving Show

-- Values: FuncExpr

data FuncExpr = 
  SFE3 SimpleFuncExpr | BFE2 BigFuncExpr | CFE1 CasesFuncExpr
  deriving Show

newtype SimpleFuncExpr = SFE (Parameters, SimpleFuncBody)
  deriving Show

newtype BigFuncExpr = BFE (Parameters, BigFuncBody)
  deriving Show

data BigFuncBody = 
  SFB1 SimpleFuncBody | BOE2 BigOpExpr
  deriving Show

data Parameters = 
  OneParam Identifier | ManyParams (Identifier, [Identifier])
  deriving Show

data SimpleFuncBody = 
  NPOA3 NoParenOpArg | SOE5 SimpleOpExpr | OEFE3 OpExprFuncEnd
  deriving Show

newtype CasesFuncExpr = CFE (CasesParams, [Case], EndCase)
  deriving Show

data CasesParams =
  OneCParam CasesParam | ManyCParams (CasesParam, [CasesParam])
  deriving Show

data CasesParam =
  Id4 Identifier | CasesKeyword
  deriving Show

newtype Case = Ca (Matching, CaseBody)
  deriving Show

newtype EndCase = EC (EndMatching, CaseBody)
  deriving Show

data EndMatching =
  Dots | M Matching
  deriving Show

data Matching = 
  Lit2 Literal | Id5 Identifier | PFM (PreFunc, Matching) | TM1 TupleMatching |
  LM1 ListMatching
  deriving Show

newtype TupleMatching = TM (Matching, [Matching])
  deriving Show

newtype ListMatching = LM (Maybe (Matching, [Matching]))
  deriving Show

newtype CaseBody = CB (CaseBodyStart, Maybe WhereExpr)
  deriving Show

data CaseBodyStart =
  SFB2 SimpleFuncBody | BOE3 BigOpExpr
  deriving Show

-- Values: ValueDef, GroupedValueDefs, WhereExpr

newtype ValueDef = VD (Identifier, Type, ValueExpr, Maybe WhereExpr)
  deriving Show

data ValueExpr = 
  NPOA4 NoParenOpArg | OE OpExpr | FE FuncExpr | BT1 BigTuple | BL1 BigList
  deriving Show

newtype GroupedValueDefs =
  GVDs (Identifier, [Identifier], Types, CommaSepLineExprs, [CommaSepLineExprs]) 
  deriving Show

data Types = 
  Ts (Type, [Type]) | All Type
  deriving Show

data WhereExpr = 
  VD1 ValueDef | GVD GroupedValueDefs
  deriving Show

-- Type

type Type = (Maybe Condition, SimpleType)

data SimpleType = 
  TId1 TypeId | TV1 TypeVar | FT FuncType | PT1 ProdType | TA1 TypeApp
  deriving Show

type TypeId = String

type TypeVar = Char
 
type FuncType = (ParamTypes, OneType)

data ParamTypes = 
  OT OneType | ManyTs (SimpleType, [SimpleType])
  deriving Show

data OneType = 
  TId2 TypeId | TV2 TypeVar | PT2 ProdType | TA2 TypeApp | FT1 FuncType
  deriving Show

newtype ProdType = PT (FieldType, [FieldType])
  deriving Show

data FieldType = 
  TId3 TypeId | TV3 TypeVar | TA3 TypeApp | IPT InParenT
  deriving Show

data InParenT = 
  FT2 FuncType | PT3 ProdType
  deriving Show

data TypeApp =  
  TIWA1 (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) |
  TIPTI (TypesInParen, TypeId, Maybe TypesInParen) |
  TITIP (TypeId, TypesInParen)
  deriving Show

newtype TypeIdWithArgs = TIWA (TypeId, [(TypesInParen, String)])
  deriving Show

newtype TypesInParen = TIP (SimpleType, [SimpleType])
  deriving Show

newtype Condition = Co PropName
  deriving Show

-- TypeDef, TypeNickname

data TypeDef =
  TTD1 TupleTypeDef | OTD1 OrTypeDef
  deriving Show

newtype TupleTypeDef = TTD (TypeName, Identifier, [Identifier], ProdType)
  deriving Show

newtype TypeName = TN (Maybe ParamsInParen, MiddleTypeName, Maybe ParamsInParen)
  deriving Show

data MiddleTypeName = 
  TId4 TypeId | TIWP1 TypeIdWithParams
  deriving Show
   
newtype TypeIdWithParams = TIWP (TypeId, [(ParamsInParen, String)])
  deriving Show

newtype ParamsInParen = PIP (TypeVar, [TypeVar])
  deriving Show

newtype OrTypeDef =
  OTD (TypeName, Identifier, Maybe SimpleType, [(Identifier, Maybe SimpleType)])
  deriving Show

newtype TypeNickname = TNN (TypeName, SimpleType)
  deriving Show

-- TypePropDef

data TypePropDef = 
  APD1 AtomPropDef | RPD1 RenamingPropDef
  deriving Show

newtype AtomPropDef = APD (PropNameLine, Identifier, SimpleType)
  deriving Show

newtype RenamingPropDef = RPD (PropNameLine, PropName, [PropName])
  deriving Show

newtype PropNameLine = PNL PropName
  deriving Show

data PropName =
  NPStart1 ([(NamePart, ParamsInParen)], Maybe NamePart) |
  PIPStart1 ([(ParamsInParen, NamePart)], Maybe ParamsInParen)
  deriving Show

type NamePart = String

-- TypeTheo 

newtype TypeTheo = TT (TypeTheoStart, Identifier, ValueExpr)
  deriving Show

data TypeTheoStart =
  AP1 AtomicProp | IP1 ImplicationProp
  deriving Show

newtype AtomicProp = AP PropNameSub
  deriving Show

newtype ImplicationProp = IP (PropNameSub, PropNameSub)
  deriving Show

data PropNameSub = 
  NPStart2 ([(NamePart, TypesInParen)], Maybe NamePart) |
  PIPStart2 ([(TypesInParen, NamePart)], Maybe TypesInParen)
  deriving Show

-- Program

newtype Program = P [ProgramPart]

data ProgramPart = 
  VD2 ValueDef | TD TypeDef | TNN1 TypeNickname | TPD TypePropDef | TT1 TypeTheo

---------------------------------------Parser---------------------------------------

type Parser = Parsec String Int

haskell_lexer = makeTokenParser haskellDef -- for copying some haskell parsers 

class HasParser a where
  parser :: Parser a

-- helper parsers

(comma, underscore, lower_under, in_paren, nl_indent) =
  (string ", ", char '_', lower <|> underscore, \a -> char '(' *> a <* char ')'
  , char '\n' *> indent
  )
  ::
  (Parser String, Parser Char, Parser Char, Parser a -> Parser a, Parser ()) 

indent :: Parser ()
indent = getState >>= \il -> string (concat $ replicate il "  ") >> return ()

par_lower_unders = 
  string "()" >>= \par ->
  many1 lower_under >>= \lowers_unders ->
  return $ par ++ lowers_unders

-- HasParser: Literal

instance HasParser Int where
  parser = read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  
instance HasParser Double where
  parser = float haskell_lexer
  
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
      SFE2 <$> parser <|> BFE1 <$> undefined

instance HasParser CasesOpExpr where
  parser = 
    parser >>= \op_expr_line ->
    many (nl_indent *> parser) >>= \op_expr_lines ->
    (nl_indent <|> char ' ' *> return ()) *> undefined >>= \cases_func_expr ->
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
  parser = SFE3 <$> parser <|> BFE2 <$> undefined <|> CFE1 <$> undefined

instance HasParser SimpleFuncExpr where
  parser = 
    undefined >>= \parameters ->
    string " => " *> undefined >>= \simple_func_body ->
    return $ SFE (parameters, simple_func_body)

-- newtype SimpleFuncExpr = SFE (Parameters, SimpleFuncBody)
--   deriving Show
-- 
-- newtype BigFuncExpr = BFE (Parameters, BigFuncBody)
--   deriving Show

-- Parse class and instance

class HasParser a => Parse a where
  parse :: String -> Either ParseError a
  parse = runParser (parser <* eof) 0 input_file

instance Parse Literal
instance Parse Identifier
instance Parse ParenExpr
instance Parse Tuple
instance Parse BigTuple
instance Parse List
instance Parse BigList
instance Parse ParenFuncApp
instance Parse PreFuncApp
instance Parse PostFuncApp
instance Parse OpExpr
instance Parse SimpleOpExpr
instance Parse BigOpExpr
instance Parse CasesOpExpr

input_file = "identifiers.txt"
type ParseType = Identifier

main :: IO ()
main = 
  readFile input_file >>= \file_string ->
  mapM_ print (map parse (lines file_string) :: [Either ParseError ParseType])
