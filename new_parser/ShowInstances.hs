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
    S s -> show s

instance Show Identifier where
  show = \(Id s) -> s

instance Show ParenExpr where
  show = \(PE se) -> "(" ++ show se ++ ")"

instance Show SimpleExpr where
  show = \case
    SOE1 soe -> show soe
    SFE1 sfe -> show sfe

instance Show Tuple where
  show = \(T (le, csles)) -> "(" ++ show le ++ ", " ++ show csles ++ ")"

instance Show CommaSepLineExprs where
  show = \(CSLE (le, les)) -> show le ++ concatMap ((", " ++) . show) les

instance Show LineExpr where
  show = \case
    NPOA1 npoa -> show npoa
    SE se -> show se

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
    "[ " ++ show csles ++
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
    PrFA1 prfa -> show prfa
    PoFA1 pofa -> show pofa

instance Show BasicExpr where
  show = \case
    Lit1 lit -> show lit
    Id1 id -> show id
    T1 tuple -> show tuple
    L1 list -> show list
    PFA pfa -> show pfa

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
  show = \(PoFA (pfa, pfs)) -> show pfa ++ concatMap show pfs

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
    BOE1 boe -> show boe
    COE1 coe -> show coe

instance Show OpExprStart where
  show = \(OES (op_arg1, op1, op_arg_op_pairs)) ->
     show op_arg1 ++ " " ++ show op1  ++
     concatMap
       (\(op_arg, op) -> " " ++ show op_arg ++ " " ++ show op) op_arg_op_pairs

instance Show SimpleOpExpr where
  show = \(SOE (oes, soee)) -> show oes ++ " " ++ show soee

instance Show SimpleOpExprEnd where
  show = \case
    OA1 oa -> show oa
    SFE2 sfe -> show sfe

instance Show BigOpExpr where
  show = \case
    BOE_1_ boe1 -> show boe1
    BOE_2_ boe2 -> show boe2

instance Show BigOpExpr1 where
  show = \(BOE_1 (oess, boee)) ->
    concatMap ((++ "\n") . show) oess ++ show boee

instance Show BigOpExprEnd where
  show = \(BOEE (maybe_oes, boee2)) ->
    case maybe_oes of 
      Nothing -> show boee2
      Just oes -> show oes ++ " " ++ show boee2

instance Show BigOpExprEnd2 where
  show = \case
    OA2 oa -> show oa
    SFE3 sfe -> show sfe
    BFE1 bfe -> show bfe

instance Show BigOpExpr2 where
  show = \(BOE_2 (oes, bfe)) ->
    show oes ++ " " ++ show bfe

instance Show CasesOpExpr where
  show = \(COE (oes, oess, cfe)) ->
    show oes ++ concatMap (("\n" ++) . show) oess ++ show cfe

instance Show OpArg where
  show = \case
    NPOA2 npoa -> show npoa
    PE3 pe -> show pe

instance Show NoParenOpArg where
  show = \case
    BE3 be -> show be
    PrF prf -> show prf
    PoF pof -> show pof
    PrFA2 prfa -> show prfa
    PoFA2 pofa -> show pofa

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
    SFE4 sfe -> show sfe
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

instance Show CasesFuncExpr where
  show = \(CFE (cps, cs, maybe_ec)) ->
    show cps ++ " =>" ++ concatMap show cs ++ show_maybe maybe_ec

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
  show = \(Ca (m, cb)) -> "\n" ++ show m ++ " => " ++ show cb

instance Show EndCase where
  show = \(EC cb) -> "\n... => " ++ show cb

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

instance Show Type where
  show = \(Ty (maybe_c, st)) -> show_maybe maybe_c ++ show st

instance Show SimpleType where
  show = \case
    TId1 tid -> show tid
    TV1 tv -> show tv
    FT1 ft -> show ft
    PT1 pt -> show pt
    TA1 ta -> show ta

instance Show TypeId where
  show = \(TId str) -> str

instance Show TypeVar where
  show = \(TV c) -> [c]

instance Show FuncType where
  show = \(FT (pts, ot)) -> show pts ++ " => " ++ show ot

instance Show ParamTypes where
  show = \case
    OT ot -> show ot
    ManyTs (st, sts) -> "(" ++ show st ++ concatMap ((", " ++) . show) sts ++ ")"

instance Show OneType where
  show = \case
    TId2 tid -> show tid
    TV2 tv -> show tv
    PT2 pt -> show pt
    TA2 ta -> show ta
    FT2 ft -> "(" ++ show ft ++ ")"

instance Show ProdType where
  show = \(PT (ft, fts)) -> show ft ++ concatMap ((" x " ++) . show) fts

instance Show FieldType where
  show = \case
    TId3 tid -> show tid
    TV3 tv -> show tv
    TA3 ta -> show ta
    IPT ipt -> show ipt

instance Show InParenT where
  show = \case
    FT3 ft -> "(" ++ show ft ++ ")"  
    PT3 pt -> "(" ++ show pt ++ ")"

instance Show TypeApp where
  show = \case
    TIWA1 (maybe_tip1, tiwa, maybe_tip2) ->
      show_maybe maybe_tip1 ++ show tiwa ++ show_maybe maybe_tip2
    TIPTI (tip, tid, maybe_tip) ->
      show tip ++ show tid ++ show_maybe maybe_tip
    TITIP (tid, tip) ->
      show tid ++ show tip

instance Show TypeIdWithArgs where
  show = \(TIWA (tid, tip_str_pairs)) ->
    show tid ++ concatMap (\(tip, str) -> show tip ++ str) tip_str_pairs

instance Show TypesInParen where
  show = \(TIP (st, sts)) ->
    "(" ++ show st ++ concatMap ((", " ++) . show) sts ++ ")"

instance Show Condition where
  show = \(Co pn) -> show pn ++ " ==> "

-- TypeDef, TypeNickname

instance Show TypeDef where
  show = \case
    TTD1 ttd -> show ttd
    OTD1 otd -> show otd

instance Show TupleTypeDef where
  show = \(TTD (tn, id, ids, pt)) ->
    "tuple_type " ++ show tn ++
    "\nvalue (" ++ show id ++ concatMap ((", " ++) . show) ids ++
    ") : " ++ show pt

instance Show TypeName where
  show = \(TN (maybe_pip1, mtn, maybe_pip2)) ->
    show_maybe maybe_pip1 ++ show mtn ++ show_maybe maybe_pip2

instance Show MiddleTypeName where
  show = \case
    TId4 tid -> show tid
    TIWP1 tiwp -> show tiwp

instance Show TypeIdWithParams where
  show = \(TIWP (tid, pip_str_pairs)) ->
    show tid ++ concatMap (\(pip, str) -> show pip ++ str) pip_str_pairs

instance Show ParamsInParen where
  show = \(PIP (tv, tvs)) ->
    "(" ++ show tv ++ concatMap ((", " ++) . show) tvs ++ ")"

instance Show OrTypeDef where
  show =
    \(OTD (tn, id, mst, id_mst_pairs)) ->
    "or_type " ++ show tn ++
    "\nvalues " ++ show id ++ show_mst mst ++
    concatMap show_id_mst_pair id_mst_pairs
    where
    show_mst :: Maybe SimpleType -> String
    show_mst = \case
      Nothing -> ""
      Just st -> ":" ++ show st

    show_id_mst_pair :: (Identifier, Maybe SimpleType) -> String
    show_id_mst_pair = \(id, mst) -> " | " ++ show id ++ show_mst mst

instance Show TypeNickname where
  show = \(TNN (tn, st)) -> "type_nickname " ++ show tn ++ " = " ++ show st

-- TypePropDef

instance Show TypePropDef where
  show = \case
    APD1 apd -> show apd
    RPD1 rpd -> show rpd

instance Show AtomPropDef where
  show = \(APD (pnl, id, st)) ->
    show pnl ++ "\nvalue\n  " ++ show id ++ " : " ++ show st

instance Show RenamingPropDef where
  show = \(RPD (pnl, pn, pns)) ->
    show pnl ++ "\nequivalent\n  " ++ show pn ++ concatMap ((", " ++) . show) pns

instance Show PropNameLine where
  show = \(PNL pn) ->
    "type_proposition " ++ show pn

instance Show PropName where
  show = \case
    NPStart1 (c, np_pip_pairs, maybe_np) ->
      [c] ++ concatMap (\(np, pip) -> show np ++ show pip) np_pip_pairs ++
      show_maybe maybe_np
    PIPStart1 (pip_np_pairs, maybe_pip) ->
      concatMap (\(pip, np) -> show pip ++ show np) pip_np_pairs ++
      show_maybe maybe_pip

instance Show NamePart where
  show = \(NP str) -> str

-- TypeTheo 

instance Show TypeTheo where
  show = \(TT (pps, maybe_pps, id, ve)) ->
    "type_theorem " ++ show pps ++ show_mpps maybe_pps ++
    "\nproof\n  " ++ show id ++ " = " ++ show ve
    where
    show_mpps :: Maybe PropNameSub -> String
    show_mpps = \case
      Nothing -> ""
      Just pps -> " => " ++ show pps

instance Show PropNameSub where
  show = \case
    NPStart2 (c, np_tip_pairs, maybe_np) ->
      [c] ++ concatMap (\(np, tip) -> show np ++ show tip) np_tip_pairs ++
      show_maybe maybe_np
    TIPStart (tip_np_pairs, maybe_tip) ->
      concatMap (\(tip, np) -> show tip ++ show np) tip_np_pairs ++
      show_maybe maybe_tip

-- Program

instance Show Program where
  show = \(P pps) -> concatMap show pps

instance Show ProgramPart where
  show = \case
    VD2 vd -> show vd
    TD td -> show td
    TNN1 tnn -> show tnn
    TPD tpd -> show tpd
    TT1 tt -> show tt
