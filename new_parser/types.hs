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
  SFE_ SimpleFuncExpr | BFE BigFuncEpxr
