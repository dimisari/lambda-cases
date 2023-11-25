module ASTTypes where

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  Int Integer | R Double | Ch Char | S String 
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

newtype WhereExpr = WE [WhereDefExpr]
  deriving Show

data WhereDefExpr = 
  VD1 ValueDef | GVD GroupedValueDefs
  deriving Show

-- Type

newtype Type = Ty (Maybe Condition, SimpleType)
  deriving Show

data SimpleType = 
  TId1 TypeId | TV1 TypeVar | FT1 FuncType | PT1 ProdType | TA1 TypeApp
  deriving Show

newtype TypeId = TId String
  deriving Show

newtype TypeVar = TV Char
  deriving Show
 
newtype FuncType = FT (ParamTypes, OneType)
  deriving Show

data ParamTypes = 
  OT OneType | ManyTs (SimpleType, [SimpleType])
  deriving Show

data OneType = 
  TId2 TypeId | TV2 TypeVar | PT2 ProdType | TA2 TypeApp | FT2 FuncType
  deriving Show

newtype ProdType = PT (FieldType, [FieldType])
  deriving Show

data FieldType = 
  TId3 TypeId | TV3 TypeVar | TA3 TypeApp | IPT InParenT
  deriving Show

data InParenT = 
  FT3 FuncType | PT3 ProdType
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
  NPStart1 (Char, [(NamePart, ParamsInParen)], Maybe NamePart) |
  PIPStart1 ([(ParamsInParen, NamePart)], Maybe ParamsInParen)
  deriving Show

newtype NamePart = NP String
  deriving Show

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
  NPStart2 (Char, [(NamePart, TypesInParen)], Maybe NamePart) |
  TIPStart ([(TypesInParen, NamePart)], Maybe TypesInParen)
  deriving Show

-- Program

newtype Program = P [ProgramPart]
  deriving Show

data ProgramPart = 
  VD2 ValueDef | TD TypeDef | TNN1 TypeNickname | TPD TypePropDef | TT1 TypeTheo
  deriving Show
