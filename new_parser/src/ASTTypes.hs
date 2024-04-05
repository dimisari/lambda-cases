module ASTTypes where

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId
data Literal = 
  Int Int | R Double | Ch Char | S String 

type HasParen = Bool
newtype Identifier = Id (HasParen, [String], Maybe Char, HasParen)

newtype SimpleId = SId String

newtype ParenExpr = PE InsideParenExpr

data InsideParenExpr = 
  LOE1 LineOpExpr | LFE1 LineFuncExpr 

newtype Tuple = T (LineExprOrUnder, LineExprOrUnders)

newtype LineExprOrUnders = LEOUs (LineExprOrUnder, [LineExprOrUnder]) 

data LineExprOrUnder = 
  LE1 LineExpr | Underscore1

data LineExpr = 
  BOAE1 BasicOrAppExpr | LOE2 LineOpExpr | LFE2 LineFuncExpr

data BasicOrAppExpr =
  BE3 BasicExpr | PrFA1 PreFuncApp | PoFA1 PostFuncApp

data BasicExpr = 
  Lit1 Literal | PFAOI1 ParenFuncAppOrId | T1 Tuple | L1 List | SI1 SpecialId

newtype BigTuple = BT (LineExprOrUnder, LineExprOrUnders, [LineExprOrUnders])

newtype List = L (Maybe LineExprOrUnders)

newtype BigList = BL (LineExprOrUnders, [LineExprOrUnders])

type ArgsStr = (Arguments, String)
newtype ParenFuncAppOrId = 
  PFAOI (Maybe Arguments, String, [ArgsStr], Maybe Char, Maybe Arguments)

newtype Arguments = As LineExprOrUnders

-- Values: PreFunc, PostFunc, BasicExpr, Change
newtype PreFunc = PF SimpleId

newtype PreFuncApp = PrFA (PreFunc, Operand)

data PostFunc = 
  SId1 SimpleId | SI2 SpecialId | C1 Change

data SpecialId = 
  First | Second | Third | Fourth | Fifth

newtype PostFuncApp = PoFA (PostFuncArg, [PostFunc])

data PostFuncArg = 
  PE2 ParenExpr | BE2 BasicExpr | Underscore2

newtype Change = C (FieldChange, [FieldChange])

newtype FieldChange = FC (Field, LineExprOrUnder)

data Field =
  SId2 SimpleId | SI3 SpecialId

-- Values: OpExpr
data OpExpr = 
  LOE3 LineOpExpr | BOE1 BigOpExpr

newtype OpExprStart = OES [(Operand, Op)]

newtype LineOpExpr = LOE (OpExprStart, LineOpExprEnd)

data LineOpExprEnd = 
  O1 Operand | LFE3 LineFuncExpr

data BigOpExpr = 
  BOEOS1 BigOpExprOpSplit | BOEFS1 BigOpExprFuncSplit

newtype BigOpExprOpSplit = BOEOS ([OpSplitLine], Maybe OpExprStart, OpSplitEnd)

newtype OpSplitLine = OSL (OpExprStart, Maybe OperFCO) 

newtype OperFCO = OFCO (Operand, FuncCompOp)

data OpSplitEnd =
  O2 Operand | FE1 FuncExpr 

newtype BigOpExprFuncSplit = BOEFS (OpExprStart, BigOrCasesFuncExpr)

data BigOrCasesFuncExpr =
  BFE1 BigFuncExpr | CFE1 CasesFuncExpr

data Operand =
  BOAE2 BasicOrAppExpr | PE3 ParenExpr | Underscore3

data Op = 
  FCO3 FuncCompOp | OSO OptionalSpacesOp

data FuncCompOp =
  RightComp | LeftComp

data OptionalSpacesOp = 
  RightApp | LeftApp | Power | Mult | Div | Plus | Minus | Equal | NotEqual |
  Greater | Less | GrEq | LeEq | And | Or | Use | Then

-- Values: FuncExpr
data FuncExpr = 
  LFE4 LineFuncExpr | BFE2 BigFuncExpr | CFE2 CasesFuncExpr

newtype LineFuncExpr = LFE (Parameters, LineFuncBody)

newtype BigFuncExpr = BFE (Parameters, BigFuncBody)

data Parameters = 
  ParamId Identifier | Star1 | Params (Parameters, [Parameters])

data LineFuncBody = 
  BOAE3 BasicOrAppExpr | LOE4 LineOpExpr

data BigFuncBody = 
  BOAE4 BasicOrAppExpr | OE1 OpExpr

newtype CasesFuncExpr = CFE (CasesParams, [Case], Maybe EndCase)

data CasesParams =
  CParamId Identifier | CasesKeyword | Star2 |
  CParams (CasesParams, [CasesParams])

newtype Case = Ca (Matching, CaseBody)

newtype EndCase = EC (EndCaseParam, CaseBody)

data EndCaseParam =
  Id1 Identifier | Ellipsis

data Matching = 
  Lit2 Literal | SId3 SimpleId | PFM (PreFunc, InnerMatching) |
  TM1 TupleMatching | LM1 ListMatching

data InnerMatching = 
  M1 Matching | IWP2 IdWithParen | Star

newtype TupleMatching = TM (InnerMatching, [InnerMatching])

newtype ListMatching = LM (Maybe (InnerMatching, [InnerMatching]))

type IdWithParenStart = ([String], Maybe Char, HasParen)
type IdWithNoParenStart = (String, IWNPSContinuation)
data IdWithParen =
  IWPS IdWithParenStart | IWNPS IdWithNoParenStart

newtype ParenInTheMiddle = PITM ([String], Maybe Char, HasParen)
type NoParenInTheMiddle = Maybe Char
data IWNPSContinuation = 
  PITM1 ParenInTheMiddle | NPITM NoParenInTheMiddle

data CaseBody = 
  LFB1 LineFuncBody | BFB1 (BigFuncBody, Maybe WhereExpr)

-- Values: ValueDef, GroupedValueDefs, WhereExpr
newtype ValueDef = VD (Identifier, Type, ValueExpr, Maybe WhereExpr)

data ValueExpr = 
  BOAE5 BasicOrAppExpr | OE2 OpExpr | FE2 FuncExpr | BT1 BigTuple | BL1 BigList

newtype GroupedValueDefs =
  GVDs (Identifier, [Identifier], Types, LineExprs, [LineExprs]) 

data Types = 
  Ts (Type, [Type]) | All Type

newtype LineExprs = LEs (LineExpr, [LineExpr])

newtype WhereExpr = WE (WhereDefExpr, [WhereDefExpr])

data WhereDefExpr = 
  VD1 ValueDef | GVDs1 GroupedValueDefs

-- Type
newtype Type = Ty (Maybe Condition, SimpleType)

data SimpleType = 
  TIOV1 TypeIdOrVar | TA1 TypeApp | PoT1 PowerType | PT1 ProdType |
  FT1 FuncType 

data TypeIdOrVar =
  TId1 TypeId | TV1 TypeVar

newtype TypeId = TId String

data TypeVar =
  PTV1 ParamTVar | AHTV1 AdHocTVar
 
newtype ParamTVar = PTV Int

newtype AdHocTVar = AHTV Char

type TIWATypeApp = (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen)
type TIPTITypeApp = (TypesInParen, TIdOrAdHocTVar, Maybe TypesInParen)
data TypeApp =  
  TIWA1 TIWATypeApp | TIPTI TIPTITypeApp | TITIP (TIdOrAdHocTVar, TypesInParen)

type TIPSTR = (TypesInParen, String)
newtype TypeIdWithArgs = TIWA (TypeId, [TIPSTR])

data TIdOrAdHocTVar =
  TId2 TypeId | AHTV2 AdHocTVar

newtype TypesInParen = TIP (SimpleType, [SimpleType])

newtype ProdType = PT (FieldType, [FieldType])

data FieldType =
  PBT1 PowerBaseType | PoT3 PowerType 

data PowerBaseType = 
  TIOV3 TypeIdOrVar | TA3 TypeApp | IPT InParenT

data InParenT = 
  PT3 ProdType | FT3 FuncType

newtype PowerType = PoT (PowerBaseType, Int)

newtype FuncType = FT (InOrOutType, InOrOutType)

data InOrOutType = 
  TIOV2 TypeIdOrVar | PT2 ProdType | PoT2 PowerType | TA2 TypeApp |
  FT2 FuncType

newtype Condition = Co PropName

-- TypeDef, TypeNickname
data TypeDef =
  TTD1 TupleTypeDef | OTD1 OrTypeDef

newtype TupleTypeDef = TTD (TypeName, IdTuple, ProdOrPowerType)

data ProdOrPowerType =
  PT4 ProdType | PoT4 PowerType

type PVIPStr = (ParamVarsInParen, String)
newtype TypeName =
  TN (Maybe ParamVarsInParen, TypeId, [PVIPStr], Maybe ParamVarsInParen)

newtype ParamVarsInParen = PVIP (ParamTVar, [ParamTVar])

newtype IdTuple = PCSIs (SimpleId, [SimpleId])

newtype OrTypeDef =
  OTD
    (TypeName, SimpleId, Maybe SimpleType, [(SimpleId, Maybe SimpleType)])

newtype TypeNickname = TNN (TypeName, SimpleType)

-- TypePropDef
data TypePropDef = 
  APD1 AtomPropDef | RPD1 RenamingPropDef

newtype AtomPropDef = APD (PropNameLine, Identifier, SimpleType)

newtype RenamingPropDef = RPD (PropNameLine, PropName, [PropName])

newtype PropNameLine = PNL PropName

type NPStart1 = (Char, [(NamePart, AdHocVarsInParen)], Maybe NamePart)
type AHVIPStart = ([(AdHocVarsInParen, NamePart)], Maybe AdHocVarsInParen)
data PropName = 
  NPStart1 NPStart1 | AHVIPStart AHVIPStart

newtype AdHocVarsInParen = AHVIP (AdHocTVar, [AdHocTVar])

newtype NamePart = NP String

-- TypeTheo 
newtype TypeTheo =
  TT (PropNameWithSubs, Maybe PropNameWithSubs, Proof)

type NPStart2 = (Char, [(NamePart, SubsInParen)], Maybe NamePart) 
type SIPStart = ([(SubsInParen, NamePart)], Maybe SubsInParen)
data PropNameWithSubs = 
  NPStart2 NPStart2 | SIPStart SIPStart

newtype SubsInParen = SIP (TVarSub, [TVarSub])

data TVarSub =
  TIOV4 TypeIdOrVar | TAS1 TypeAppSub | PoTS1 PowerTypeSub | PTS1 ProdTypeSub |
  FTS1 FuncTypeSub 

type TIWS_TAS =
  (Maybe SubsOrUndersInParen, TypeIdWithSubs, Maybe SubsOrUndersInParen)
type SOUIP_TI_TAS =
  (SubsOrUndersInParen, TIdOrAdHocTVar, Maybe SubsOrUndersInParen)
data TypeAppSub =  
  TIWS_TAS TIWS_TAS | SOUIP_TI SOUIP_TI_TAS |
  TI_SOUIP (TIdOrAdHocTVar, SubsOrUndersInParen)

type SOUIPSTR = (SubsOrUndersInParen, String)
newtype TypeIdWithSubs = TIWS (TypeId, [SOUIPSTR])

newtype SubsOrUndersInParen = SOUIP (SubOrUnder, [SubOrUnder])

data SubOrUnder = 
  TVS1 TVarSub | Underscore4
 
newtype PowerTypeSub = PoTS (PowerBaseTypeSub, Int)

data PowerBaseTypeSub =
  Underscore5 | TIOV5 TypeIdOrVar | TAS2 TypeAppSub | IPTS1 InParenTSub

data InParenTSub = 
  PTS2 ProdTypeSub | FTS2 FuncTypeSub

newtype ProdTypeSub = PTS (FieldTypeSub, [FieldTypeSub])

data FieldTypeSub =
  PBTS1 PowerBaseTypeSub | PoTS2 PowerTypeSub

newtype FuncTypeSub = FTS (InOrOutTypeSub, InOrOutTypeSub)

data InOrOutTypeSub = 
  Underscore6 | TIOV6 TypeIdOrVar | TAS3 TypeAppSub | 
  PoTS3 PowerTypeSub | PTS3 ProdTypeSub | FTS3 FuncTypeSub

data Proof =
  P1 (IdOrOpEq, LineExpr) | P2 (IdOrOpEq, TTValueExpr) 

newtype IdOrOpEq = IOOE (Identifier, Maybe (Op, Identifier))

data TTValueExpr =
  LE2 LineExpr | VEMWE (ValueExpr, Maybe WhereExpr)

-- Program
newtype Program = P (ProgramPart, [ProgramPart])

data ProgramPart = 
  VD2 ValueDef | GVDs2 GroupedValueDefs | TD TypeDef | TNN1 TypeNickname |
  TPD TypePropDef | TT1 TypeTheo

-- For fast vim navigation
-- Generation/TypesAndHelpers.hs
-- Generation/AST.hs
-- Generation/Test.hs
-- Parsing/Test.hs
