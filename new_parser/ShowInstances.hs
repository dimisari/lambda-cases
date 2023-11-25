{-# LANGUAGE LambdaCase, StandaloneDeriving #-}

module ShowInstances where

import ASTTypes

-- helpers

show_maybe :: Show a => Maybe a -> String
show_maybe = \case
  Nothing -> ""
  Just a -> show a

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

instance Show Literal where 
  show = \case
    Int i -> show i
    R r -> show r
    Ch c -> show c
    S s -> s

instance Show Identifier where
  show = \(Id s) -> s

instance Show ParenExpr where
  show = \(PE oofe) -> "(" ++ show oofe ++ ")"

instance Show OpOrFuncExpr where
  show = \case
    SOE1 soe -> show soe
    OEFE1 oefe -> show oefe
    SFE1 sfe -> show sfe

instance Show Tuple where
  show = \(T (le, csles)) -> "(" ++ show le ++ ", " ++ show csles ++ ")"

instance Show CommaSepLineExprs where
  show = \(CSLE (le, les)) -> show le ++ concatMap ((", " ++) . show) les

instance Show LineExpr where
  show = \case
    NPOA1 npoa -> show npoa
    OOFE oofe -> show oofe

instance Show BigTuple where
  show = \(BT (le, csles, csles_l)) ->
    "(" ++ show le ++ ", " ++ show csles ++
    concatMap (("\n, " ++) . show) csles_l ++ 
    "\n)"

instance Show List where
  show = \(L maybe_csles) -> case maybe_csles of
    Nothing -> "[]"
    Just csles -> "[" ++ show csles ++ "]"

instance Show BigList where
  show = \(BL (csles, csles_l)) ->
    "[" ++ show csles ++
    concatMap (("\n, " ++) . show) csles_l ++ 
    "\n]"

instance Show ParenFuncApp where
  show = \case
    IWA1 (maybe_args1, id_with_args, maybe_args2) ->
      show_maybe maybe_args1 ++ show id_with_args ++ show_maybe maybe_args2
    AI (args, id, maybe_args) ->
      show args ++ show id ++ show_maybe maybe_args
    IA (id, args) ->
      show id ++ show args

instance Show Arguments where
  show = \(As csles) -> "(" ++ show csles ++ ")"

instance Show IdentWithArgs where
  show =
    \(IWA (id_with_args_start, args, str, empty_par_or_args_str_pairs, maybe_ch)) ->
    show id_with_args_start ++ show args ++ str ++
    concatMap show_pair empty_par_or_args_str_pairs ++ show_maybe_char maybe_ch
    where
    show_pair :: (EmptyParenOrArgs, String) -> String
    show_pair = \(epoa, str) -> show epoa ++ str

    show_maybe_char :: Maybe Char -> String
    show_maybe_char = \case
      Nothing -> ""
      Just c -> [c]

instance Show IdentWithArgsStart where
  show = \(IWAS str) -> str

instance Show EmptyParenOrArgs where
  show = \case
    EmptyParen -> "()"
    As1 args -> show args

-- Values: PreFunc, PostFunc, BasicExpr, DotChange

instance Show PreFunc where
  show = \(PF id) -> show id ++ ":"

instance Show PreFuncApp where
  show = \(PrFA (pf, pfa)) -> show pf ++ show pfa

instance Show PreFuncArg where
  show = \case
    BE1 be -> show be
    PE1 pe -> show pe
    PrFA1 pfa -> show pfa

instance Show BasicExpr where
  show = \case
    Lit1 lit -> show lit
    Id1 id -> show id
    T1 tuple -> show tuple
    L1 list -> show list
    PFA pfa -> show pfa
    PoFA1 pofa -> show pofa

instance Show PostFunc where
  show = \case
    Id2 id -> "." ++ show id
    Dot1st -> ".1st"
    Dot2nd -> ".2nd"
    Dot3rd -> ".3rd"
    Dot4th -> ".4th"
    Dot5th -> ".5th"
    DC1 dc -> show dc

instance Show PostFuncApp where
  show = \(PoFA (pfa, pf)) -> show pfa ++ show pf

instance Show PostFuncArg where
  show = \case
    PE2 pe -> show pe
    BE2 be -> show be

instance Show DotChange where
  show = \(DC (fc, fcs)) ->
    ".change{" ++ show fc ++ concatMap ((", " ++) . show) fcs ++ "}"

instance Show FieldChange where
  show = \(FC (f, le)) -> show f ++ " = " ++ show le

instance Show Field where
  show = \case
    Id3 id -> show id
    First -> "1st"
    Second -> "2nd"
    Third -> "3rd"
    Fourth -> "4th"
    Fifth -> "5th"

-- Values: OpExpr

instance Show OpExpr where
  show = \case
    SOE2 soe -> show soe
    OEFE2 oefe -> show oefe
    BOE1 boe -> show boe
    COE1 coe -> show coe

instance Show SimpleOpExpr where
  show = \(SOE (oa, op_oa_pairs)) ->
    show oa ++ concatMap (\(op, oa) -> " " ++ show op ++ " " ++ show oa) op_oa_pairs

instance Show OpExprFuncEnd where
  show = \(OEFE (soe, op, sfe)) -> show soe ++ " " ++ show op ++ " " ++ show sfe

instance Show BigOpExpr where
  show = \(BOE (oel, oels, boee)) ->
    show oel ++ concatMap (("\n" ++) . show) oels ++ "\n" ++ show boee

instance Show BigOpExprEnd where
  show = \case
    OA1 oa -> show oa
    SOE3 soe -> show soe
    BOFE (maybe_oel, bofe) ->
      show_maybe_oel ++ show bofe
      where
      show_maybe_oel =
        case maybe_oel of
          Nothing -> ""
          Just oel -> show oel ++ " "

instance Show BigOpFuncEnd where
  show = \case
    SFE2 sfe -> show sfe
    BFE1 bfe -> show bfe

instance Show CasesOpExpr where
  show = \(COE (oel, oels, cfe)) ->
    show oel ++ concatMap (("\n" ++) . show) oels ++ show cfe

instance Show OpExprLine where
  show = \(OEL (oels, op)) -> show oels ++ " " ++ show op

instance Show OpExprLineStart where
  show = \case
    OA2 oa -> show oa
    SOE4 soe -> show soe

instance Show OpArg where
  show = \case
    NPOA2 npoa -> show npoa
    PE3 pe -> show pe

instance Show NoParenOpArg where
  show = \case
    BE3 be -> show be
    PrF prf -> show prf
    PoF pof -> show pof
    PrFA2 pfa -> show pfa

instance Show Op where
  show = \case
    RightApp -> "->"
    LeftApp -> "<-"
    RightComp -> "o>"
    LeftComp -> "<o"
    Power -> "^" 
    Mult -> "*" 
    Div -> "/" 
    Plus -> "+" 
    Minus -> "-" 
    Equal -> "=" 
    NotEqual -> "/="
    Greater -> ">" 
    Less -> "<" 
    GrEq -> ">="
    LeEq -> "<="
    And -> "&" 
    Or -> "|" 
    Use -> ";>"
    Then -> ";" 

-- Values: FuncExpr

instance Show FuncExpr where
  show = \case
    SFE3 sfe -> show sfe
    BFE2 bfe -> show bfe
    CFE1 cfe -> show cfe

instance Show SimpleFuncExpr where
  show = \(SFE (params, sfb)) -> show params ++ " => " ++ show sfb

instance Show BigFuncExpr where
  show = \(BFE (params, bfb)) -> show params ++ " =>\n" ++ show bfb

instance Show BigFuncBody where
  show = \case
    SFB1 sfb -> show sfb
    BOE2 boe -> show boe

instance Show Parameters where
  show = \case
    OneParam id -> show id
    ManyParams (id, ids) ->
      "(" ++ show id ++ concatMap ((", " ++) . show) ids ++ ")"

instance Show SimpleFuncBody where
  show = \case
    NPOA3 npoa -> show npoa
    SOE5 soe -> show soe
    OEFE3 oefe -> show oefe

instance Show CasesFuncExpr where
  show = \(CFE (cps, cs, ec)) ->
    show cps ++ " =>" ++ concatMap show cs ++ show ec

instance Show CasesParams where
  show = \case
    OneCParam cp -> show cp
    ManyCParams (cp, cps) ->
      "(" ++ show cp ++ concatMap ((", " ++) . show) cps ++ ")"

instance Show CasesParam where
  show = \case
    Id4 id -> show id
    CasesKeyword -> "cases"

instance Show Case where
  show = \(Ca (m, cb)) -> "\n" ++ show m ++ " =>" ++ show cb

instance Show EndCase where
  show = \(EC cb) -> "\n... =>" ++ show cb

instance Show Matching where
  show = \case
    Lit2 lit -> show lit
    Id5 id -> show id
    PFM (pf, m) -> show pf ++ show m
    TM1 tm -> show tm
    LM1 lm -> show lm

instance Show TupleMatching where
  show = \(TM (m, ms)) -> "(" ++ show m ++ concatMap ((", " ++) . show) ms ++ ")"

instance Show ListMatching where
  show = \(LM maybe_m_ms) -> case maybe_m_ms of
    Nothing -> "[]"
    Just (m, ms) -> "[" ++ show m ++ concatMap ((", " ++) . show) ms ++ "]"

instance Show CaseBody where
  show = \(CB (cbs, maybe_we)) -> show cbs ++ show_maybe maybe_we

instance Show CaseBodyStart where
  show = \case
    SFB2 sfb -> show sfb
    BOE3 boe -> show boe

-- Values: ValueDef, GroupedValueDefs, WhereExpr

instance Show ValueDef where
  show = \(VD (id, t, ve, maybe_we)) ->
    show id ++ "\n  : " ++ show t ++ "\n  = " ++ show ve ++ show_maybe maybe_we

instance Show ValueExpr where
  show = \case
    NPOA4 npoa -> show npoa
    OE oe -> show oe
    FE fe -> show fe
    BT1 bt -> show bt
    BL1 bl -> show bl

instance Show GroupedValueDefs where
  show = \(GVDs (id, ids, ts, csles, csles_l)) ->
    show id ++ concatMap ((", " ++) . show) ids ++
    "\n  : " ++ show ts ++
    "\n  = " ++ show csles ++ concatMap (("\n  , " ++) . show) csles_l

instance Show Types where
  show = \case
    Ts (t, ts) -> show t ++ concatMap ((", " ++) . show) ts
    All t -> "all " ++ show t

instance Show WhereExpr where
  show = \(WE wdes) -> "\nwhere\n" ++ concatMap show wdes

instance Show WhereDefExpr where
  show = \case
    VD1 vd -> show vd
    GVD gvd -> show gvd

-- Type

deriving instance Show Type
deriving instance Show SimpleType
deriving instance Show TypeId
deriving instance Show TypeVar
deriving instance Show FuncType
deriving instance Show ParamTypes
deriving instance Show OneType
deriving instance Show ProdType
deriving instance Show FieldType
deriving instance Show InParenT
deriving instance Show TypeApp
deriving instance Show TypeIdWithArgs
deriving instance Show TypesInParen
deriving instance Show Condition

-- TypeDef, TypeNickname

deriving instance Show TypeDef
deriving instance Show TupleTypeDef
deriving instance Show TypeName
deriving instance Show MiddleTypeName
deriving instance Show TypeIdWithParams
deriving instance Show ParamsInParen
deriving instance Show OrTypeDef
deriving instance Show TypeNickname

-- TypePropDef

deriving instance Show TypePropDef
deriving instance Show AtomPropDef
deriving instance Show RenamingPropDef
deriving instance Show PropNameLine
deriving instance Show PropName
deriving instance Show NamePart

-- TypeTheo 

deriving instance Show TypeTheo
deriving instance Show TypeTheoStart
deriving instance Show AtomicProp
deriving instance Show ImplicationProp
deriving instance Show PropNameSub

-- Program

deriving instance Show Program
deriving instance Show ProgramPart
