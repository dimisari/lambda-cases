module ASTTypes where

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  Int Integer | R Double | Ch Char | S String 

newtype Identifier = Id String

newtype ParenExpr = PE OpOrFuncExpr

data OpOrFuncExpr = 
  SOE1 SimpleOpExpr | OEFE1 OpExprFuncEnd | SFE1 SimpleFuncExpr

newtype Tuple = T (LineExpr, CommaSepLineExprs)

newtype CommaSepLineExprs = CSLE (LineExpr, [LineExpr])

data LineExpr = 
  NPOA1 NoParenOpArg | OOFE OpOrFuncExpr

newtype BigTuple = BT (LineExpr, CommaSepLineExprs, [CommaSepLineExprs])

newtype List = L (Maybe CommaSepLineExprs)

newtype BigList = BL (CommaSepLineExprs, [CommaSepLineExprs])

data ParenFuncApp = 
  IWA1 (Maybe Arguments, IdentWithArgs, Maybe Arguments) |
  AI (Arguments, Identifier, Maybe Arguments) |
  IA (Identifier, Arguments)

newtype Arguments = As CommaSepLineExprs

newtype IdentWithArgs =
  IWA
  (IdentWithArgsStart, Arguments, String, [(EmptyParenOrArgs, String)], Maybe Char)

newtype IdentWithArgsStart = IWAS String

data EmptyParenOrArgs =
  EmptyParen | As1 Arguments

-- Values: PreFunc, PostFunc, BasicExpr, DotChange

newtype PreFunc = PF Identifier 

newtype PreFuncApp = PrFA (PreFunc, PreFuncArg)

data PreFuncArg = 
  BE1 BasicExpr | PE1 ParenExpr | PrFA1 PreFuncApp

data BasicExpr = 
  Lit1 Literal | Id1 Identifier | T1 Tuple | L1 List | PFA ParenFuncApp |
  PoFA1 PostFuncApp

data PostFunc = 
  Id2 Identifier | Dot1st | Dot2nd | Dot3rd | Dot4th | Dot5th | DC1 DotChange

newtype PostFuncApp = PoFA (PostFuncArg, PostFunc)

data PostFuncArg = 
  PE2 ParenExpr | BE2 BasicExpr

newtype DotChange = DC (FieldChange, [FieldChange])

newtype FieldChange = FC (Field, LineExpr)

data Field =
  Id3 Identifier | First | Second | Third | Fourth | Fifth 

-- Values: OpExpr

data OpExpr = 
  SOE2 SimpleOpExpr | OEFE2 OpExprFuncEnd | BOE1 BigOpExpr | COE1 CasesOpExpr

newtype SimpleOpExpr = SOE (OpArg, [(Op, OpArg)])

newtype OpExprFuncEnd = OEFE (SimpleOpExpr, Op, SimpleFuncExpr)

newtype BigOpExpr = BOE (OpExprLine, [OpExprLine], BigOpExprEnd)

data BigOpExprEnd = 
  OA1 OpArg | SOE3 SimpleOpExpr | BOFE (Maybe OpExprLine, BigOpFuncEnd)

data BigOpFuncEnd = 
  SFE2 SimpleFuncExpr | BFE1 BigFuncExpr

newtype CasesOpExpr = COE (OpExprLine, [OpExprLine], CasesFuncExpr)

newtype OpExprLine = OEL (OpExprLineStart, Op)

data OpExprLineStart = 
  OA2 OpArg | SOE4 SimpleOpExpr

data OpArg =
  NPOA2 NoParenOpArg | PE3 ParenExpr

data NoParenOpArg =
  BE3 BasicExpr | PrF PreFunc | PoF PostFunc | PrFA2 PreFuncApp

data Op = 
  RightApp | LeftApp | RightComp | LeftComp | Power | Mult | Div | Plus | 
  Minus | Equal | NotEqual | Greater | Less | GrEq | LeEq | And | Or | Use | Then

-- Values: FuncExpr

data FuncExpr = 
  SFE3 SimpleFuncExpr | BFE2 BigFuncExpr | CFE1 CasesFuncExpr

newtype SimpleFuncExpr = SFE (Parameters, SimpleFuncBody)

newtype BigFuncExpr = BFE (Parameters, BigFuncBody)

data BigFuncBody = 
  SFB1 SimpleFuncBody | BOE2 BigOpExpr

data Parameters = 
  OneParam Identifier | ManyParams (Identifier, [Identifier])

data SimpleFuncBody = 
  NPOA3 NoParenOpArg | SOE5 SimpleOpExpr | OEFE3 OpExprFuncEnd

newtype CasesFuncExpr = CFE (CasesParams, [Case], Maybe EndCase)

data CasesParams =
  OneCParam CasesParam | ManyCParams (CasesParam, [CasesParam])

data CasesParam =
  Id4 Identifier | CasesKeyword

newtype Case = Ca (Matching, CaseBody)

newtype EndCase = EC CaseBody

data Matching = 
  Lit2 Literal | Id5 Identifier | PFM (PreFunc, Matching) | TM1 TupleMatching |
  LM1 ListMatching

newtype TupleMatching = TM (Matching, [Matching])

newtype ListMatching = LM (Maybe (Matching, [Matching]))

newtype CaseBody = CB (CaseBodyStart, Maybe WhereExpr)

data CaseBodyStart =
  SFB2 SimpleFuncBody | BOE3 BigOpExpr

-- Values: ValueDef, GroupedValueDefs, WhereExpr

newtype ValueDef = VD (Identifier, Type, ValueExpr, Maybe WhereExpr)

data ValueExpr = 
  NPOA4 NoParenOpArg | OE OpExpr | FE FuncExpr | BT1 BigTuple | BL1 BigList

newtype GroupedValueDefs =
  GVDs (Identifier, [Identifier], Types, CommaSepLineExprs, [CommaSepLineExprs]) 

data Types = 
  Ts (Type, [Type]) | All Type

newtype WhereExpr = WE [WhereDefExpr]

data WhereDefExpr = 
  VD1 ValueDef | GVD GroupedValueDefs

-- Type

newtype Type = Ty (Maybe Condition, SimpleType)

data SimpleType = 
  TId1 TypeId | TV1 TypeVar | FT1 FuncType | PT1 ProdType | TA1 TypeApp

newtype TypeId = TId String

newtype TypeVar = TV Char
 
newtype FuncType = FT (ParamTypes, OneType)

data ParamTypes = 
  OT OneType | ManyTs (SimpleType, [SimpleType])

data OneType = 
  TId2 TypeId | TV2 TypeVar | PT2 ProdType | TA2 TypeApp | FT2 FuncType

newtype ProdType = PT (FieldType, [FieldType])

data FieldType = 
  TId3 TypeId | TV3 TypeVar | TA3 TypeApp | IPT InParenT

data InParenT = 
  FT3 FuncType | PT3 ProdType

data TypeApp =  
  TIWA1 (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) |
  TIPTI (TypesInParen, TypeId, Maybe TypesInParen) |
  TITIP (TypeId, TypesInParen)

newtype TypeIdWithArgs = TIWA (TypeId, [(TypesInParen, String)])

newtype TypesInParen = TIP (SimpleType, [SimpleType])

newtype Condition = Co PropName

-- TypeDef, TypeNickname

data TypeDef =
  TTD1 TupleTypeDef | OTD1 OrTypeDef

newtype TupleTypeDef = TTD (TypeName, Identifier, [Identifier], ProdType)

newtype TypeName = TN (Maybe ParamsInParen, MiddleTypeName, Maybe ParamsInParen)

data MiddleTypeName = 
  TId4 TypeId | TIWP1 TypeIdWithParams
   
newtype TypeIdWithParams = TIWP (TypeId, [(ParamsInParen, String)])

newtype ParamsInParen = PIP (TypeVar, [TypeVar])

newtype OrTypeDef =
  OTD (TypeName, Identifier, Maybe SimpleType, [(Identifier, Maybe SimpleType)])

newtype TypeNickname = TNN (TypeName, SimpleType)

-- TypePropDef

data TypePropDef = 
  APD1 AtomPropDef | RPD1 RenamingPropDef

newtype AtomPropDef = APD (PropNameLine, Identifier, SimpleType)

newtype RenamingPropDef = RPD (PropNameLine, PropName, [PropName])

newtype PropNameLine = PNL PropName

data PropName =
  NPStart1 (Char, [(NamePart, ParamsInParen)], Maybe NamePart) |
  PIPStart1 ([(ParamsInParen, NamePart)], Maybe ParamsInParen)

newtype NamePart = NP String

-- TypeTheo 

newtype TypeTheo = TT (TypeTheoStart, Identifier, ValueExpr)

data TypeTheoStart =
  AP1 AtomicProp | IP1 ImplicationProp

newtype AtomicProp = AP PropNameSub

newtype ImplicationProp = IP (PropNameSub, PropNameSub)

data PropNameSub = 
  NPStart2 (Char, [(NamePart, TypesInParen)], Maybe NamePart) |
  TIPStart ([(TypesInParen, NamePart)], Maybe TypesInParen)

-- Program

newtype Program = P [ProgramPart]

data ProgramPart = 
  VD2 ValueDef | TD TypeDef | TNN1 TypeNickname | TPD TypePropDef | TT1 TypeTheo
