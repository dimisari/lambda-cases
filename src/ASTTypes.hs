{-
This file contains all the types that make up the AST
-}

module ASTTypes where

import Prelude qualified as P

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

data Literal =
  Int P.Integer | R P.Double | Ch P.Char | S P.String

newtype Identifier =
  Id
  ( P.Maybe UndersInParen, IdStart, [IdCont], P.Maybe P.Char
  , P.Maybe UndersInParen
  )
  deriving (P.Eq, P.Ord)

newtype SimpleId = SId (IdStart, P.Maybe P.Char)
  deriving (P.Eq, P.Ord)

newtype IdStart = IS P.String
  deriving (P.Eq, P.Ord)

newtype IdCont = IC (UndersInParen, P.String)
  deriving (P.Eq, P.Ord)

newtype UndersInParen = UIP P.Int
  deriving (P.Eq, P.Ord)

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

newtype BigTuple =
  BT (LineExprOrUnder, BigTupleSplit, LineExprOrUnders, [LineExprOrUnders])

data BigTupleSplit =
  Split | NoSplit


newtype List = L (P.Maybe LineExprOrUnders)

newtype BigList = BL (LineExprOrUnders, [LineExprOrUnders])

type ArgsStr = (Arguments, P.String)
newtype ParenFuncAppOrId =
  PFAOI
    (P.Maybe Arguments, IdStart, [ArgsStr], P.Maybe P.Char, P.Maybe Arguments)

newtype Arguments = As LineExprOrUnders


-- Values: PreFunc, PostFunc, BasicExpr, Change

newtype PreFunc = PF SimpleId

newtype PreFuncApp = PrFA (PreFunc, Operand)

newtype PostFunc = PoF IdOrSpecialId

data IdOrSpecialId =
  Id1 Identifier | SI2 SpecialId

data SpecialId =
  First | Second | Third | Fourth | Fifth

newtype PostFuncApp = PoFA (PostFuncArg, PostFuncAppEnd)

data PostFuncArg =
  PE2 ParenExpr | BE2 BasicExpr | Underscore2

data PostFuncAppEnd =
  DC1 DotChange | PFsMDC ([PostFunc], P.Maybe DotChange)

newtype DotChange = DC (FieldChange, [FieldChange])

newtype FieldChange = FC (IdOrSpecialId, LineExprOrUnder)


-- Values: OpExpr

data OpExpr =
  LOE3 LineOpExpr | BOE1 BigOpExpr


newtype OpExprStart = OES [(Operand, Op)]

newtype LineOpExpr = LOE (OpExprStart, LineOpExprEnd)

data LineOpExprEnd =
  O1 Operand | LFE3 LineFuncExpr

data BigOpExpr =
  BOEOS1 BigOpExprOpSplit | BOEFS1 BigOpExprFuncSplit

newtype BigOpExprOpSplit = BOEOS ([OpSplitLine], P.Maybe OpExprStart, OpSplitEnd)

data OpSplitLine =
  OESMOFCO (OpExprStart, P.Maybe OperFCO) | OFCO1 OperFCO

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
  BOAE3 BasicOrAppExpr | LOE4 LineOpExpr | PLFE1 ParenLineFuncExpr

newtype ParenLineFuncExpr = PLFE LineFuncExpr

data BigFuncBody =
  BOAE4 BasicOrAppExpr | OE1 OpExpr | PLFE2 ParenLineFuncExpr

newtype CasesFuncExpr = CFE (CasesParams, [Case], P.Maybe EndCase)

data CasesParams =
  CParamId Identifier | QuestionMark | Star2 |
  CParams (CasesParams, [CasesParams])

newtype Case = Ca (OuterMatching, CaseBody)

newtype EndCase = EC (EndCaseParam, CaseBody)

data OuterMatching =
  SId3 SimpleId | M1 Matching

data EndCaseParam =
  Id2 Identifier | Ellipsis

data Matching =
  Lit2 Literal | PFM (PreFunc, InnerMatching) | TM1 TupleMatching |
  LM1 ListMatching

data InnerMatching =
  Star | Id3 Identifier | M2 Matching

newtype TupleMatching = TM (InnerMatching, [InnerMatching])

newtype ListMatching =
  LM (P.Maybe (InnerMatching, [InnerMatching], P.Maybe RestListMatching))

newtype RestListMatching = RLM (P.Maybe SimpleId)

data CaseBody =
  LFB1 LineFuncBody | BFB1 (BigFuncBody, P.Maybe WhereExpr)


-- Values: ValueDef, GroupedValueDefs, WhereExpr

newtype ValueDef = VD (Identifier, Type, P.Maybe ValueEquals)

newtype ValueEquals = VE (ValueExpr, P.Maybe WhereExpr)

data ValueExpr =
  BOAE5 BasicOrAppExpr | OE2 OpExpr | FE2 FuncExpr | BT1 BigTuple | BL1 BigList

newtype GroupedValueDefs = GVDs (Identifiers, Types, LineExprs, [LineExprs])

data Types =
  Ts (Type, [Type]) | All Type

newtype LineExprs = LEs (LineExpr, [LineExpr])

newtype WhereExpr = WE (WhereDefExpr, [WhereDefExpr])

data WhereDefExpr =
  VD1 ValueDef | GVDs1 GroupedValueDefs


-- Type

newtype Type = Ty (P.Maybe Condition, SimpleType)

data SimpleType =
  PTV1 ParamTVar | TAIOA1 TypeAppIdOrAHTV | POPT1 ProdOrPowerType | FT1 FuncType

data ProdOrPowerType =
  PT4 ProdType | PoT5 PowerType

newtype TypeId = TId P.String
  deriving P.Eq

newtype ParamTVar = PTV P.Int
  deriving (P.Eq, P.Ord)

newtype AdHocTVar = AHTV P.Char
  deriving (P.Eq, P.Ord)

newtype TypeAppIdOrAHTV =
  TAIOA (P.Maybe TypesInParen, TAIOAMiddle, P.Maybe TypesInParen)

data TAIOAMiddle =
  TIdStart1 (TypeId, [(TypesInParen, P.String)]) | AHTV2 AdHocTVar

newtype TypesInParen = TIP (SimpleType, [SimpleType])

newtype ProdType = PT (FieldType, [FieldType])

data FieldType =
  PBT1 PowerBaseType | PoT2 PowerType

data PowerBaseType =
  PTV2 ParamTVar | TAIOA2 TypeAppIdOrAHTV | IPT InParenT

data InParenT =
  PT3 ProdType | FT3 FuncType | PoT3 PowerType

newtype PowerType = PoT (PowerBaseType, P.Integer)

newtype FuncType = FT (InOrOutType, InOrOutType)

data InOrOutType =
  PTV3 ParamTVar | TAIOA3 TypeAppIdOrAHTV | POPT2 ProdOrPowerType | FT2 FuncType

newtype Condition = Co PropName


-- TypeDef, TypeNickname

data TypeDef =
  TTD1 TupleTypeDef | OTD1 OrTypeDef

newtype TupleTypeDef = TTD (TypeName, ProdOrPowerType, FieldNames)

type PVIPStr = (ParamVarsInParen, P.String)
newtype TypeName =
  TN (P.Maybe ParamVarsInParen, TypeId, [PVIPStr], P.Maybe ParamVarsInParen)

newtype ParamVarsInParen = PVIP (ParamTVar, [ParamTVar])

newtype FieldNames = PCSIs Identifiers

newtype Identifiers = Ids (Identifier, [Identifier])

newtype OrTypeDef = OTD (TypeName, OrTypeValues)

data OrTypeValues = VL OrTypeValuesLine | Ls OrTypeValuesLines

newtype OrTypeValuesLine = OTVL (OrTypeValue, [OrTypeValue])

newtype OrTypeValuesLines = OTVLs (OrTypeValuesLine, [OrTypeValuesLine])

newtype OrTypeValue = OTV (SimpleId, P.Maybe InternalValue)

newtype InternalValue = IV (Identifier, SimpleType)

newtype TypeNickname = TNN (TypeName, SimpleType)


-- TypePropDef

data TypePropDef =
  APD1 AtomPropDef | RPD1 RenamingPropDef

newtype AtomPropDef = APD (PropNameLine, Identifier, SimpleType)

newtype RenamingPropDef = RPD (PropNameLine, PropName, [PropName])

newtype PropNameLine = PNL PropName

type NPStart1 = (P.Char, [(NamePart, TypesInParen)], P.Maybe NamePart)
type TIPStart = ([(TypesInParen, NamePart)], P.Maybe TypesInParen)
data PropName =
  NPStart1 NPStart1 | TIPStart TIPStart

newtype NamePart = NP P.String
  deriving P.Eq


-- TypeTheo

newtype TypeTheo =
  TT ([PropNameWithSubs], P.Maybe PropNameWithSubs, Proof)

type NPStart2 = (P.Char, [(NamePart, SubsInParen)], P.Maybe NamePart)
type SIPStart = ([(SubsInParen, NamePart)], P.Maybe SubsInParen)
data PropNameWithSubs =
  NPStart2 NPStart2 | SIPStart SIPStart

newtype SubsInParen = SIP (TVarSub, [TVarSub])

data TVarSub =
  PTV4 ParamTVar | TAIOAS1 TypeAppIdOrAHTVSub | POPTS1 ProdOrPowerTypeSub |
  FTS1 FuncTypeSub

data ProdOrPowerTypeSub =
  PTS1 ProdTypeSub | PoTS1 PowerTypeSub

newtype TypeAppIdOrAHTVSub =
  TAIOAS (P.Maybe SubsOrUndersInParen, TAIOASMiddle, P.Maybe SubsOrUndersInParen)

data TAIOASMiddle =
  TIdStart2 (TypeId, [(SubsOrUndersInParen, P.String)]) | AHTV3 AdHocTVar

newtype SubsOrUndersInParen = SOUIP (SubOrUnder, [SubOrUnder])

data SubOrUnder =
  TVS1 TVarSub | Underscore4

newtype PowerTypeSub = PoTS (PowerBaseTypeSub, P.Integer)

data PowerBaseTypeSub =
  Underscore5 | PTV5 ParamTVar | TAIOAS2 TypeAppIdOrAHTVSub | IPTS InParenTSub

data InParenTSub =
  PTS2 ProdTypeSub | FTS2 FuncTypeSub

newtype ProdTypeSub = PTS (FieldTypeSub, [FieldTypeSub])

data FieldTypeSub =
  PBTS1 PowerBaseTypeSub | PoTS2 PowerTypeSub

newtype FuncTypeSub = FTS (InOrOutTypeSub, InOrOutTypeSub)

data InOrOutTypeSub =
  Underscore6 | PTV6 ParamTVar | TAIOAS3 TypeAppIdOrAHTVSub |
  POPTS2 ProdOrPowerTypeSub | FTS3 FuncTypeSub

data Proof =
  P1 (IdOrOpEq, LineExpr) | P2 (IdOrOpEq, TTValueExpr)

newtype IdOrOpEq = IOOE (Identifier, P.Maybe (Op, Identifier))

data TTValueExpr =
  LE2 LineExpr | VEMWE (ValueExpr, P.Maybe WhereExpr)


-- Program

newtype Program = P (ProgramPart, [ProgramPart])

data ProgramPart =
  VD2 ValueDef | GVDs2 GroupedValueDefs | TD TypeDef | TNN1 TypeNickname |
  TPD TypePropDef | TT1 TypeTheo


-- For fast vim file navigation:
{-
Helpers.hs
ShowInstances.hs
Parsing/TypesAndHelpers.hs
Parsing/AST.hs
Generation/Helpers.hs
Generation/Collect.hs
Generation/Preprocess.hs
Generation/CheckCompatibility.hs
Generation/AST.hs
-}
