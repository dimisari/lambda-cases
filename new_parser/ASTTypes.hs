module ASTTypes where

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  Int Int | R Double | Ch Char | S String 

newtype Identifier = Id String

newtype ParenExpr = PE InsideParenExpr

data InsideParenExpr = 
  LOE1 LineOpExpr | LFE1 LineFuncExpr 

newtype Tuple = T (LineOrUnderExpr, LineOrUnderExprs)

newtype LineOrUnderExprs = LOUEs (LineOrUnderExpr, [LineOrUnderExpr]) 

data LineOrUnderExpr = 
  LE1 LineExpr | Underscore1

newtype BigTuple = BT (LineOrUnderExpr, LineOrUnderExprs, [LineOrUnderExprs])

newtype List = L (Maybe LineOrUnderExprs)

newtype BigList = BL (LineOrUnderExprs, [LineOrUnderExprs])

data ParenFuncApp = 
  IWA1 (Maybe Arguments, IdentWithArgs, Maybe Arguments) |
  AI (Arguments, Identifier, Maybe Arguments) |
  IA (Identifier, Arguments)

newtype Arguments = As LineOrUnderExprs

newtype IdentWithArgs =
  IWA
  (IdentWithArgsStart, Arguments, String, [(EmptyParenOrArgs, String)], Maybe Char)

newtype IdentWithArgsStart = IWAS String

data EmptyParenOrArgs =
  EmptyParen | As1 Arguments

-- Values: PreFunc, PostFunc, BasicExpr, Change

newtype PreFunc = PF Identifier 

newtype PreFuncApp = PrFA (PreFunc, Operand)

data PostFunc = 
  Id2 Identifier | SI2 SpecialId | C1 Change

data SpecialId = 
  First | Second | Third | Fourth | Fifth

newtype PostFuncApp = PoFA (PostFuncArg, [PostFunc])

data PostFuncArg = 
  PE2 ParenExpr | BE2 BasicExpr | Underscore2

data BasicExpr = 
  Lit1 Literal | Id1 Identifier | T1 Tuple | L1 List | PFA ParenFuncApp | 
  SI1 SpecialId

newtype Change = C (FieldChange, [FieldChange])

newtype FieldChange = FC (Field, LineOrUnderExpr)

data Field =
  Id3 Identifier | SI3 SpecialId

-- Values: OpExpr

data OpExpr = 
  LOE3 LineOpExpr | BOE1 BigOpExpr

newtype OpExprStart = OES [(Operand, Op)]

newtype LineOpExpr = LOE (OpExprStart, LineOpExprEnd)

data LineOpExprEnd = 
  OA1 Operand | LFE3 LineFuncExpr

data BigOpExpr = 
  BOEOS1 BigOpExprOpSplit | BOEFS1 BigOpExprFuncSplit

newtype BigOpExprOpSplit = BOEOS ([OpSplitLine], Maybe OpExprStart, OpSplitEnd)

newtype OpSplitLine = OSL (OpExprStart, Maybe (Operand, FuncCompOp)) 

data OpSplitEnd =
  OA2 Operand | FE1 FuncExpr 

newtype BigOpExprFuncSplit = BOEFS (OpExprStart, BigOrCasesFuncExpr)

data BigOrCasesFuncExpr =
  BFE1 BigFuncExpr | CFE1 CasesFuncExpr

data Operand =
  BOAE2 BasicOrAppExpr | PE3 ParenExpr | Underscore3

data BasicOrAppExpr =
  BE3 BasicExpr | PrFA1 PreFuncApp | PoFA1 PostFuncApp

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

newtype EndCase = EC CaseBody

data Matching = 
  Lit2 Literal | Id5 Identifier | PFM (PreFunc, Matching) | TM1 TupleMatching |
  LM1 ListMatching

newtype TupleMatching = TM (Matching, [Matching])

newtype ListMatching = LM (Maybe (Matching, [Matching]))

newtype CaseBody = CB (CaseBodyStart, Maybe WhereExpr)

data CaseBodyStart =
  LFB1 LineFuncBody | BFB1 BigFuncBody

-- Values: ValueDef, GroupedValueDefs, WhereExpr

newtype ValueDef = VD (Identifier, Type, ValueExpr, Maybe WhereExpr)

data ValueExpr = 
  BOAE5 BasicOrAppExpr | OE2 OpExpr | FE2 FuncExpr | BT1 BigTuple | BL1 BigList

newtype GroupedValueDefs =
  GVDs (Identifier, [Identifier], Types, LineExprs, [LineExprs]) 

data Types = 
  Ts (Type, [Type]) | All Type

newtype LineExprs = CSLE (LineExpr, [LineExpr])

data LineExpr = 
  BOAE1 BasicOrAppExpr | LOE2 LineOpExpr | LFE2 LineFuncExpr

newtype WhereExpr = WE (WhereDefExpr, [WhereDefExpr])

data WhereDefExpr = 
  VD1 ValueDef | GVDs1 GroupedValueDefs

-- Type

newtype Type = Ty (Maybe Condition, SimpleType)

data SimpleType = 
  TId1 TypeId | TV1 TypeVar | FT1 FuncType | PT1 ProdType | PoT1 PowerType | 
  TA1 TypeApp

newtype TypeId = TId String

data TypeVar =
  PTV1 ParamTVar | AHTV1 AdHocTVar
 
newtype ParamTVar = PTV Int

newtype AdHocTVar = AHTV Char

newtype FuncType = FT (InOrOutType, InOrOutType)

data InOrOutType = 
  TId2 TypeId | TV2 TypeVar | PT2 ProdType | PoT2 PowerType | TA2 TypeApp |
  FT2 FuncType

newtype ProdType = PT (FieldOrPowerType, [FieldOrPowerType])

data FieldOrPowerType =
  FiT1 FieldType | PoT3 PowerType 

data FieldType = 
  TId3 TypeId | TV3 TypeVar | TA3 TypeApp | IPT InParenT

data InParenT = 
  FT3 FuncType | PT3 ProdType

newtype PowerType = PoT (FieldType, [Int])

data TypeApp =  
  TIWA1 (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) |
  TIPTI (TypesInParen, TypeIdOrVar, Maybe TypesInParen) |
  TITIP (TypeIdOrVar, TypesInParen)

newtype TypeIdWithArgs = TIWA (TypeId, [(TypesInParen, String)])

data TypeIdOrVar =
  TId4 TypeId | TV4 TypeVar

newtype TypesInParen = TIP (SimpleType, [SimpleType])

newtype Condition = Co PropName

-- TypeDef, TypeNickname

data TypeDef =
  TTD1 TupleTypeDef | OTD1 OrTypeDef

newtype TupleTypeDef = TTD (TypeName, IdTuple, ProdOrPowerType)

data ProdOrPowerType =
  PT4 ProdType | PoT4 PowerType

newtype TypeName =
  TN
    ( Maybe ParamVarsInParen, TypeId, [(ParamVarsInParen, String)]
    , Maybe ParamVarsInParen)

newtype ParamVarsInParen = PVIP (ParamTVar, [ParamTVar])

newtype IdTuple = PCSIs (Identifier, [Identifier])

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
  NPStart1 (Char, [(NamePart, AdHocVarsInParen)], Maybe NamePart) |
  AHVIPStart1 ([(AdHocVarsInParen, NamePart)], Maybe AdHocVarsInParen)

newtype AdHocVarsInParen = AHVIP (AdHocTVar, [AdHocTVar])

newtype NamePart = NP String

-- TypeTheo 

newtype TypeTheo =
  TT
  (PropNameSub, Maybe PropNameSub, Identifier, Maybe (Op, Identifier), TTValueExpr)

data PropNameSub = 
  NPStart2 (Char, [(NamePart, ParamSubsInParen)], Maybe NamePart) |
  PSIPStart ([(ParamSubsInParen, NamePart)], Maybe ParamSubsInParen)

newtype ParamSubsInParen = PSIP (ParamSub, [ParamSub])

data ParamSub =
  ST1 SimpleType | TF1 TypeFunc

data TypeFunc = 
  TF_1 (Bool, TypeId, String, Bool) | TF_2 (TypeId, Bool) | TF_3 TypeId

data TTValueExpr =
  LE2 LineExpr | BOCE BigOrCasesExpr

data BigOrCasesExpr =
  BOE4 BigOpExpr | BFE3 BigFuncExpr | CFE3 CasesFuncExpr | BT2 BigTuple |
  BL2 BigList

-- Program

newtype Program = P (ProgramPart, [ProgramPart])

data ProgramPart = 
  VD2 ValueDef | GVDs2 GroupedValueDefs | TD TypeDef | TNN1 TypeNickname |
  TPD TypePropDef | TT1 TypeTheo
