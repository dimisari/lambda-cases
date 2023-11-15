-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

data Literal = 
  I Int | R Double | C Char | S String 

type Identifier = String

type ParenExpr = OpOrFuncExpr

data OpOrFuncExpr = 
  SOE SimpleOpExpr | OEFE OpExprFuncEnd | SFE SimpleFuncExpr

type Tuple = (LineExpr, CommaSepLineExprs)

type CommaSepLineExprs = (LineExpr, [LineExpr])

data LineExpr = 
  NPOA NoParenOpArg | OOFE OpOrFuncExpr

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
  BE BasicExpr | PE ParenExpr | PFA PreFuncApp

data BasicExpr = 
  Lit Literal | Id Identifier | T Tuple | L List | PaFA ParenFuncApp |
  PoFA PostFuncApp

data PostFunc = 
  I_ Identifier | Dot1st | Dot2nd | Dot3rd | Dot4th | Dot5th | DC DotChange

type PostFuncApp = (PostFuncArg, PostFunc)

data PostFuncArg = 
  PE_ ParenExpr | BE_ BasicExpr

type DotChange = (FieldChange, [FieldChange])

type FieldChange = (Field, LineExpr)

data Field =
  I__ Identifier | First | Second | Thrid | Fourth | Fifth 

-- Values: OpEpxr

data OpEpxr = 
  SOE_ SimpleOpExpr | OEFE_ OpExprFuncEnd | BOE BigOpExpr | COE CasesOpExpr

type SimpleOpExpr = (OpArg, [(Op, OpArg)])

type OpExprFuncEnd = (SimpleOpExpr, Op, SimpleFuncExpr)

type BigOpExpr = (OpExprLine, [OpExprLine], BigOpExprEnd)

data BigOpExprEnd = 
  OA OpArg | SOE__ SimpleOpExpr | BOFE (Maybe OpExprLine, BigOpFuncEnd)

data BigOpFuncEnd = 
  SFE_ SimpleFuncExpr | BFE BigFuncExpr

type CasesOpExpr = (OpExprLine, [OpExprLine], CasesFuncExpr)

type OpExprLine = (OpExprLineStart, Op)

data OpExprLineStart = 
  OA_ OpArg | SOE___ SimpleOpExpr

data OpArg =
  NPOA_ NoParenOpArg | PE__ ParenExpr

data NoParenOpArg =
  BE__ BasicExpr | PrF PreFunc | PoF PostFunc | PFA_ PreFuncApp

data Op = 
  RightApp | LeftApp | RightComp | LeftComp | Power | Mult | Div | Plus | 
  Minus | Equal | NotEqual | Greater | Less | GrEq | LeEq | And | Or | Use | Then

-- Values: FuncExpr

data FuncExpr = 
  SFE__ SimpleFuncExpr | BFE_ BigFuncExpr | CFE CasesFuncExpr

type SimpleFuncExpr = (Parameters, SimpleFuncBody)

type BigFuncExpr = (Parameters, BigFuncBody)

data BigFuncBody = 
  SFB SimpleFuncBody | BOE_ BigOpExpr

data Parameters = 
  OneParam Identifier | ManyParams (Identifier, [Identifier])

data SimpleFuncBody = 
  NPOA__ NoParenOpArg | SOE____ SimpleOpExpr | OEFE__ OpExprFuncEnd

type CasesFuncExpr = (CasesParams, [Case], EndCase)

data CasesParams =
  OneCParam CasesParam | ManyCParams (CasesParam, [CasesParam])

data CasesParam =
  I___ Identifier | CasesKeyword

type Case = (Matching, CaseBody)

type EndCase = (EndMatching, CaseBody)

data EndMatching =
  Dots | M Matching

data Matching = 
  Lit_ Literal | I____ Identifier | PFM (PreFunc, Matching) | TM TupleMatching |
  LM ListMatching

type TupleMatching = (Matching, [Matching])

type ListMatching = Maybe (Matching, [Matching])

type CaseBody = (CaseBodyStart, Maybe WhereExpr)

data CaseBodyStart =
  SFB_ SimpleFuncBody | BOE__ BigOpExpr

-- Values: ValueDef, GroupedValueDefs, WhereExpr

type ValueDef = (Identifier, Type, ValueExpr, Maybe WhereExpr)

data ValueExpr = 
  NPOA___ NoParenOpArg | OE OpEpxr | FE FuncExpr | BT BigTuple | BL BigList

type GroupedValueDefs =
  (Identifier, [Identifier], Types, CommaSepLineExprs, [CommaSepLineExprs]) 

data Types = 
  Ts (Type, [Type]) | All Type

data WhereExpr = 
  VD ValueDef | GVD GroupedValueDefs

-- Type

type Type = (Maybe Condition, SimpleType)

data SimpleType = 
  TI TypeId | TV TypeVar | FT FuncType | PT ProdType | TA TypeApp

type TypeId = String

type TypeVar = Char
 
type FuncType = (ParamTypes, OneType)

data ParamTypes = 
  OT OneType | ManyTs (SimpleType, [SimpleType])

data OneType = 
  TI_ TypeId | TV_ TypeVar | PT_ ProdType | TA_ TypeApp | FT_ FuncType

type ProdType = (FieldType, [FieldType])

data FieldType = 
  TI__ TypeId | TV__ TypeVar | TA__ TypeApp | IPT InParenT

data InParenT = 
  FT__ FuncType | PT__ ProdType

data TypeApp =  
  TIWA (Maybe TypesInParen, TypeIdWithArgs, Maybe TypesInParen) |
  TIPTI (TypesInParen, TypeId, Maybe TypesInParen) |
  TITIP (TypeId, TypesInParen)

type TypeIdWithArgs = (TypeId, [(TypesInParen, String)])

type TypesInParen = (SimpleType, [SimpleType])

type Condition = PropName

-- TypeDef

