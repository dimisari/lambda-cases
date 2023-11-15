{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Text.Parsec hiding (parse)
import Text.Parsec.Token 
import Text.Parsec.Language (haskellDef)

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  Int Int | R Double | C Char | S String 
  deriving Show

type Identifier = String

type ParenExpr = OpOrFuncExpr

data OpOrFuncExpr = 
  SOE1 SimpleOpExpr | OEFE1 OpExprFuncEnd | SFE1 SimpleFuncExpr

type Tuple = (LineExpr, CommaSepLineExprs)

type CommaSepLineExprs = (LineExpr, [LineExpr])

data LineExpr = 
  NPOA1 NoParenOpArg | OOFE OpOrFuncExpr

type BigTuple = (LineExpr, CommaSepLineExprs, [CommaSepLineExprs])

type List = Maybe CommaSepLineExprs

type BigList = (CommaSepLineExprs, [CommaSepLineExprs])

data ParenFuncApp = 
  IWA (Maybe Arguments, IdentWithArgs, Maybe Arguments) |
  AI (Arguments, Identifier, Maybe Arguments) |
  IA (Identifier, Arguments)

type Arguments = CommaSepLineExprs

type IdentWithArgs =
  (IdentWithArgsStart, Arguments, String, [(EmptyParenOrArgs, String)], Maybe Int)

type IdentWithArgsStart = String

data EmptyParenOrArgs =
  EmptyParen | As Arguments

-- Values: PreFunc, PostFunc, BasicExpr, DotChange

type PreFunc = Identifier 

type PreFuncApp = (PreFunc, PreFuncArg)

data PreFuncArg = 
  BE1 BasicExpr | PE1 ParenExpr | PFA1 PreFuncApp

data BasicExpr = 
  Lit1 Literal | Id1 Identifier | T Tuple | L List | PaFA ParenFuncApp |
  PoFA PostFuncApp

data PostFunc = 
  Id2 Identifier | Dot1st | Dot2nd | Dot3rd | Dot4th | Dot5th | DC DotChange

type PostFuncApp = (PostFuncArg, PostFunc)

data PostFuncArg = 
  PE2 ParenExpr | BE2 BasicExpr

type DotChange = (FieldChange, [FieldChange])

type FieldChange = (Field, LineExpr)

data Field =
  Id3 Identifier | First | Second | Thrid | Fourth | Fifth 

-- Values: OpEpxr

data OpEpxr = 
  SOE2 SimpleOpExpr | OEFE2 OpExprFuncEnd | BOE1 BigOpExpr | COE CasesOpExpr

type SimpleOpExpr = (OpArg, [(Op, OpArg)])

type OpExprFuncEnd = (SimpleOpExpr, Op, SimpleFuncExpr)

type BigOpExpr = (OpExprLine, [OpExprLine], BigOpExprEnd)

data BigOpExprEnd = 
  OA1 OpArg | SOE3 SimpleOpExpr | BOFE (Maybe OpExprLine, BigOpFuncEnd)

data BigOpFuncEnd = 
  SFE2 SimpleFuncExpr | BFE1 BigFuncExpr

type CasesOpExpr = (OpExprLine, [OpExprLine], CasesFuncExpr)

type OpExprLine = (OpExprLineStart, Op)

data OpExprLineStart = 
  OA2 OpArg | SOE4 SimpleOpExpr

data OpArg =
  NPOA2 NoParenOpArg | PE3 ParenExpr

data NoParenOpArg =
  BE3 BasicExpr | PrF PreFunc | PoF PostFunc | PFA2 PreFuncApp

data Op = 
  RightApp | LeftApp | RightComp | LeftComp | Power | Mult | Div | Plus | 
  Minus | Equal | NotEqual | Greater | Less | GrEq | LeEq | And | Or | Use | Then

-- Values: FuncExpr

data FuncExpr = 
  SFE3 SimpleFuncExpr | BFE2 BigFuncExpr | CFE CasesFuncExpr

type SimpleFuncExpr = (Parameters, SimpleFuncBody)

type BigFuncExpr = (Parameters, BigFuncBody)

data BigFuncBody = 
  SFB1 SimpleFuncBody | BOE2 BigOpExpr

data Parameters = 
  OneParam Identifier | ManyParams (Identifier, [Identifier])

data SimpleFuncBody = 
  NPOA3 NoParenOpArg | SOE5 SimpleOpExpr | OEFE3 OpExprFuncEnd

type CasesFuncExpr = (CasesParams, [Case], EndCase)

data CasesParams =
  OneCParam CasesParam | ManyCParams (CasesParam, [CasesParam])

data CasesParam =
  Id4 Identifier | CasesKeyword

type Case = (Matching, CaseBody)

type EndCase = (EndMatching, CaseBody)

data EndMatching =
  Dots | M Matching

data Matching = 
  Lit2 Literal | Id5 Identifier | PFM (PreFunc, Matching) | TM TupleMatching |
  LM ListMatching

type TupleMatching = (Matching, [Matching])

type ListMatching = Maybe (Matching, [Matching])

type CaseBody = (CaseBodyStart, Maybe WhereExpr)

data CaseBodyStart =
  SFB2 SimpleFuncBody | BOE3 BigOpExpr

-- Values: ValueDef, GroupedValueDefs, WhereExpr

type ValueDef = (Identifier, Type, ValueExpr, Maybe WhereExpr)

data ValueExpr = 
  NPOA4 NoParenOpArg | OE OpEpxr | FE FuncExpr | BT BigTuple | BL BigList

type GroupedValueDefs =
  (Identifier, [Identifier], Types, CommaSepLineExprs, [CommaSepLineExprs]) 

data Types = 
  Ts (Type, [Type]) | All Type

data WhereExpr = 
  VD ValueDef | GVD GroupedValueDefs

-- Type

type Type = (Maybe Condition, SimpleType)

data SimpleType = 
  TId1 TypeId | TV1 TypeVar | FT FuncType | PT1 ProdType | TA1 TypeApp

type TypeId = String

type TypeVar = Char
 
type FuncType = (ParamTypes, OneType)

data ParamTypes = 
  OT OneType | ManyTs (SimpleType, [SimpleType])

data OneType = 
  TId2 TypeId | TV2 TypeVar | PT2 ProdType | TA2 TypeApp | FT1 FuncType

type ProdType = (FieldType, [FieldType])

data FieldType = 
  TId3 TypeId | TV3 TypeVar | TA3 TypeApp | IPT InParenT

data InParenT = 
  FT2 FuncType | PT3 ProdType

data TypeApp =  
  TIWA (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) |
  TIPTI (TypesInParen, TypeId, Maybe TypesInParen) |
  TITIP (TypeId, TypesInParen)

type TypeIdWithArgs = (TypeId, [(TypesInParen, String)])

type TypesInParen = (SimpleType, [SimpleType])

type Condition = PropName

-- TypeDef, TypeNickname

data TypeDef =
  TTD TupleTypeDef | OTD OrTypeDef

type TupleTypeDef = (TypeName, Identifier, [Identifier], ProdType)

type TypeName = (Maybe ParamsInParen, MiddleTypeName, Maybe ParamsInParen)

data MiddleTypeName = 
  TId4 TypeId | TIWP TypeIdWithParams
   
type TypeIdWithParams = (TypeId, [(ParamsInParen, String)])

type ParamsInParen = (TypeVar, [TypeVar])

type OrTypeDef =
  (TypeName, Identifier, Maybe SimpleType, [(Identifier, Maybe SimpleType)])

type TypeNickname = (TypeName, SimpleType)

-- TypePropDef

data TypePropDef = 
  APD AtomPropDef | RPD RenamingPropDef

type AtomPropDef = (PropNameLine, Identifier, SimpleType)

type RenamingPropDef = (PropNameLine, PropName, [PropName])

type PropNameLine = PropName

data PropName =
  NPStart1 ([(NamePart, ParamsInParen)], Maybe NamePart) |
  PIPStart1 ([(ParamsInParen, NamePart)], Maybe ParamsInParen)

type NamePart = String

-- TypeTheo 

type TypeTheo = (TypeTheoStart, Identifier, ValueExpr)

data TypeTheoStart =
  AP AtomicProp | IP ImplicationProp

type AtomicProp = PropNameSub

type ImplicationProp = (PropNameSub, PropNameSub)

data PropNameSub = 
  NPStart2 ([(NamePart, TypesInParen)], Maybe NamePart) |
  PIPStart2 ([(TypesInParen, NamePart)], Maybe TypesInParen)

-- Parser

type Parser = Parsec String Int

haskell_lexer = makeTokenParser haskellDef -- copy some haskell parsers (for tokens)

-- HasParser class and instances

class HasParser a where
  parser :: Parser a

instance HasParser Int where
  parser = read <$> ((:) <$> char '-' <*> many1 digit <|> many1 digit)
  
instance HasParser Double where
  parser = float haskell_lexer
  
instance HasParser Char where
  parser = charLiteral haskell_lexer
  
instance HasParser String where
  parser = stringLiteral haskell_lexer
  
instance HasParser Literal where
  parser = R <$> try parser <|> Int <$> parser <|> S <$> try parser <|> C <$> parser

-- Parse class and instance

class HasParser a => Parse a where
  parse :: String -> Either ParseError a
  parse = runParser (parser <* eof) 0 "input"

instance Parse Int
instance Parse Double
instance Parse Char
instance Parse String
instance Parse Literal

main :: IO ()
main = 
  readFile "input" >>= \file_string ->
  mapM_ print (map parse (lines file_string) :: [Either ParseError Literal])
