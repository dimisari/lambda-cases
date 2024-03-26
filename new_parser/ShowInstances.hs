{-# LANGUAGE LambdaCase #-}

module ShowInstances where

import ASTTypes
import Data.List

-- helpers
show_maybe :: Show a => Maybe a -> String
show_maybe = \case
  Nothing -> ""
  Just a -> show a

show_list :: Show a => [a] -> String
show_list = concatMap show

show_list_sep :: Show a => String -> [a] -> String
show_list_sep = \sep -> concatMap ((sep ++) . show)

show_list_comma :: Show a => [a] -> String
show_list_comma = show_list_sep ", "

show_pair_list :: (Show a, Show b) => [(a, b)] -> String
show_pair_list = concatMap (\(a, b) -> show a ++ show b)

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp
instance Show Literal where 
  show = \case
    Int i -> show i
    R r -> show r
    Ch c -> show c
    S s -> show s

instance Show Identifier where
  show = \(Id (strs, maybe_digit)) ->
    intercalate "()" strs ++ show_maybe maybe_digit

instance Show SimpleId where
  show = \(SId str) -> str

instance Show ParenExpr where
  show = \(PE pei) -> "(" ++ show pei ++ ")"

instance Show InsideParenExpr where
  show = \case
    LOE1 soe -> show soe
    LFE1 sfe -> show sfe

instance Show Tuple where
  show = \(T (loue, loues)) -> "(" ++ show loue ++ ", " ++ show loues ++ ")"

instance Show LineExprOrUnders where
  show = \(LEOUs (loue, loues)) -> show loue ++ show_list_comma loues

instance Show LineExprOrUnder where
  show = \case
    LE1 le -> show le
    Underscore1 -> "_"

instance Show LineExpr where
  show = \case
    BOAE1 npoa -> show npoa
    LOE2 soe -> show soe
    LFE2 sfe -> show sfe

instance Show BasicOrAppExpr where
  show = \case
    BE3 be -> show be
    PrFA1 prfa -> show prfa
    PoFA1 pofa -> show pofa

instance Show BasicExpr where
  show = \case
    Lit1 lit -> show lit
    Id1 id -> show id
    T1 tuple -> show tuple
    L1 list -> show list
    PFA pfa -> show pfa
    SI1 sid -> show sid

instance Show BigTuple where
  show = \(BT (loue, loues, loues_l)) ->
    "( " ++ show loue ++ ", " ++ show loues ++ show_list_sep "\n, " loues_l ++
    "\n)"

instance Show List where
  show = \(L maybe_loues) -> "[" ++ show_maybe maybe_loues ++ "]"

instance Show BigList where
  show = \(BL (loues, loues_l)) ->
    "[ " ++ show loues ++ show_list_sep "\n, " loues_l ++ "\n]"

instance Show ParenFuncApp where
  show = \case
    IWA1 (maybe_args1, id_with_args, maybe_args2) ->
      show_maybe maybe_args1 ++ show id_with_args ++ show_maybe maybe_args2
    AI (args, id, maybe_args) ->
      show args ++ show id ++ show_maybe maybe_args
    IA (sid, args) -> show sid ++ show args

instance Show Arguments where
  show = \(As loues) -> "(" ++ show loues ++ ")"

instance Show IdentWithArgs where
  show =
    \(IWA (iwas, args, str, empty_par_or_args_str_pairs, maybe_ch)) ->
    show iwas ++ show args ++ str ++
    concatMap show_pair empty_par_or_args_str_pairs ++ show_maybe_char maybe_ch
    where
    show_pair :: (EmptyParenOrArgs, String) -> String
    show_pair = \(epoa, str) -> show epoa ++ str

    show_maybe_char :: Maybe Char -> String
    show_maybe_char = \case
      Nothing -> ""
      Just c -> [c]

instance Show IdentWithArgsStart where
  show = \(IWAS strs) -> intercalate "()" strs

instance Show EmptyParenOrArgs where
  show = \case
    EmptyParen -> "()"
    As1 args -> show args

-- Values: PreFunc, PostFunc, BasicExpr, Change
instance Show PreFunc where
  show = \(PF sid) -> show sid ++ ":"

instance Show PreFuncApp where
  show = \(PrFA (pf, oper)) -> show pf ++ show oper

instance Show PostFunc where
  show = \case
    SId1 sid -> "." ++ show sid
    SI2 sid -> "." ++ show sid
    C1 c -> "." ++ show c

instance Show SpecialId where
  show = \case
    First -> "1st"
    Second -> "2nd"
    Third -> "3rd"
    Fourth -> "4th"
    Fifth -> "5th"

instance Show PostFuncApp where
  show = \(PoFA (pfa, pfs)) -> show pfa ++ show_list pfs

instance Show PostFuncArg where
  show = \case
    PE2 pe -> show pe
    BE2 be -> show be
    Underscore2 -> "_"

instance Show Change where
  show = \(C (fc, fcs)) -> "change{" ++ show fc ++ show_list_comma fcs ++ "}"

instance Show FieldChange where
  show = \(FC (f, le)) -> show f ++ " = " ++ show le

instance Show Field where
  show = \case
    SId2 sid -> show sid
    SI3 sid -> show sid

-- Values: OpExpr
instance Show OpExpr where
  show = \case
    LOE3 soe -> show soe
    BOE1 boe -> show boe

instance Show OpExprStart where
  show = \(OES oper_op_pairs) -> show_pair_list oper_op_pairs

instance Show LineOpExpr where
  show = \(LOE (oes, loee)) -> show oes ++ show loee

instance Show LineOpExprEnd where
  show = \case
    O1 o -> show o
    LFE3 sfe -> show sfe

instance Show BigOpExpr where
  show = \case
    BOEOS1 boeos -> show boeos
    BOEFS1 boefs -> show boefs

instance Show BigOpExprOpSplit where
  show = \(BOEOS (osls, maybe_oes, ose)) ->
    show_list osls ++ show_maybe maybe_oes ++ show ose

instance Show OpSplitLine where
  show = \(OSL (oes, maybe_op_arg_comp_op)) ->
    show oes ++ show_moaco maybe_op_arg_comp_op ++ "  "
    where
    show_moaco :: Maybe (Operand, FuncCompOp) -> String
    show_moaco = \case
      Nothing -> "\n"
      Just (op_arg, comp_op) -> show op_arg ++ " " ++  show comp_op ++ "\n"

instance Show OpSplitEnd where
  show = \case
    O2 o -> show o
    FE1 fe -> show fe

instance Show BigOpExprFuncSplit where
  show = \(BOEFS (oes, boefs)) -> show oes ++ show boefs

instance Show BigOrCasesFuncExpr where
  show = \case
    BFE1 bfe -> show bfe
    CFE1 cfe -> show cfe
  
instance Show Operand where
  show = \case
    BOAE2 npoa -> show npoa
    PE3 pe -> show pe
    Underscore3 -> "_"

instance Show Op where
  show = \case
    FCO3 co -> " " ++ show co ++ " "
    OSO oso -> " " ++ show oso ++ " "

instance Show FuncCompOp where
  show = \case
    RightComp -> "o>"
    LeftComp -> "<o"

instance Show OptionalSpacesOp where
  show = \case
    RightApp -> "->"
    LeftApp -> "<-"
    Power -> "^" 
    Mult -> "*" 
    Div -> "/" 
    Plus -> "+" 
    Minus -> "-" 
    Equal -> "==" 
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
    LFE4 sfe -> show sfe
    BFE2 bfe -> show bfe
    CFE2 cfe -> show cfe

instance Show LineFuncExpr where
  show = \(LFE (params, lfb)) -> show params ++ " =>" ++ show lfb

instance Show BigFuncExpr where
  show = \(BFE (params, bfb)) -> show params ++ " =>" ++ show bfb

instance Show Parameters where
  show = \case
    ParamId id -> show id
    Star1 -> "*"
    Params (params, params_l) ->
      "(" ++ show params ++ show_list_comma params_l ++ ")"

instance Show LineFuncBody where
  show = \case
    BOAE3 npoa -> " " ++ show npoa
    LOE4 soe -> " " ++ show soe

instance Show BigFuncBody where
  show = \case
    BOAE4 npoa -> "\n" ++ show npoa
    OE1 oe -> "\n" ++ show oe

instance Show CasesFuncExpr where
  show = \(CFE (cps, cs, maybe_ec)) ->
    show cps ++ show_list cs ++ show_maybe maybe_ec

instance Show CasesParams where
  show = \case
    CParamId id -> show id
    CasesKeyword -> "cases"
    Star2 -> "*"
    CParams (cps, cps_l) -> "(" ++ show cps ++ show_list_comma cps_l ++ ")"

instance Show Case where
  show = \(Ca (m, cb)) -> "\n" ++ show m ++ " =>" ++ show cb

instance Show EndCase where
  show = \(EC (ecp, cb)) -> "\n" ++ show ecp ++ " =>" ++ show cb

instance Show EndCaseParam where
  show = \case
    IWP1 id_with_paren -> show id_with_paren
    Ellipsis -> "..."

instance Show Matching where
  show = \case
    Lit2 lit -> show lit
    SId3 sid -> show sid
    PFM (pf, mos) -> show pf ++ show mos
    TM1 tm -> show tm
    LM1 lm -> show lm

instance Show InnerMatching where
  show = \case
    M1 m -> show m
    IWP2 iwp -> show iwp
    Star -> "*"

instance Show TupleMatching where
  show = \(TM (mos, mos_l)) -> "(" ++ show mos ++ show_list_comma mos_l ++ ")"

instance Show ListMatching where
  show = \(LM maybe_m_ms) -> case maybe_m_ms of
    Nothing -> "[]"
    Just (mos, mos_l) -> "[" ++ show mos ++ show_list_comma mos_l ++ "]"

instance Show IdWithParen where
  show = \(IWP iwp) -> show (Id iwp)

instance Show CaseBody where
  show = \case
    LFB1 lfb -> show lfb
    BFB1 (bfb, maybe_we) -> show bfb ++ show_maybe maybe_we

-- Values: ValueDef, GroupedValueDefs, WhereExpr
instance Show ValueDef where
  show = \(VD (id, t, ve, maybe_we)) ->
    show id ++ "\n  : " ++ show t ++ "\n  = " ++ show ve ++ show_maybe maybe_we

instance Show ValueExpr where
  show = \case
    BOAE5 npoa -> show npoa
    OE2 oe -> show oe
    FE2 fe -> show fe
    BT1 bt -> show bt
    BL1 bl -> show bl

instance Show GroupedValueDefs where
  show = \(GVDs (id, ids, ts, csles, csles_l)) ->
    show id ++ show_list_comma ids ++
    "\n  : " ++ show ts ++
    "\n  = " ++ show csles ++ show_list_sep "\n  , " csles_l

instance Show Types where
  show = \case
    Ts (t, ts) -> show t ++ show_list_comma ts
    All t -> "all " ++ show t

instance Show LineExprs where
  show = \(CSLE (le, les)) -> show le ++ show_list_comma les

instance Show WhereExpr where
  show =
    \(WE (wde, wdes)) -> "\nwhere\n" ++ show wde ++ show_list_sep "\n\n" wdes

instance Show WhereDefExpr where
  show = \case
    VD1 vd -> show vd
    GVDs1 gvd -> show gvd

-- Type
instance Show Type where
  show = \(Ty (maybe_c, st)) -> show_maybe maybe_c ++ show st

instance Show SimpleType where
  show = \case
    TIOV1 tiov -> show tiov
    TA1 ta -> show ta
    PoT1 pt -> show pt
    PT1 pt -> show pt
    FT1 ft -> show ft

instance Show TypeIdOrVar where
  show = \case
    TId1 tid -> show tid
    TV1 tv -> show tv

instance Show TypeId where
  show = \(TId str) -> str

instance Show TypeVar where
  show = \case
    PTV1 ptv -> show ptv
    AHTV1 ahtv -> show ahtv

instance Show ParamTVar where
  show = \(PTV i) -> "T" ++ show i

instance Show AdHocTVar where
  show = \(AHTV c) -> "@" ++ [c]

instance Show TypeApp where
  show = \case
    TIWA1 (maybe_tip1, tiwa, maybe_tip2) ->
      show_maybe maybe_tip1 ++ show tiwa ++ show_maybe maybe_tip2
    TIPTI (tip, tid_or_tv, maybe_tip) ->
      show tip ++ show tid_or_tv ++ show_maybe maybe_tip
    TITIP (tid_or_tv, tip) ->
      show tid_or_tv ++ show tip

instance Show TypeIdWithArgs where
  show = \(TIWA (tid, tip_str_pairs)) ->
    show tid ++ concatMap (\(tip, str) -> show tip ++ str) tip_str_pairs

instance Show TIdOrAdHocTVar where
  show = \case
    TId2 tid -> show tid
    AHTV2 ahtv -> show ahtv

instance Show TypesInParen where
  show = \(TIP (st, sts)) -> "(" ++ show st ++ show_list_comma sts ++ ")"

instance Show ProdType where
  show = \(PT (fopt, fopts)) -> show fopt ++ show_list_sep " x " fopts

instance Show FieldType where
  show = \case
    PBT1 ft -> show ft
    PoT3 pt -> show pt

instance Show PowerBaseType where
  show = \case
    TIOV3 tiov -> show tiov
    TA3 ta -> show ta
    IPT ipt -> show ipt

instance Show InParenT where
  show = \case
    PT3 pt -> "(" ++ show pt ++ ")"
    FT3 ft -> "(" ++ show ft ++ ")"  

instance Show PowerType where
  show = \(PoT (ft, i)) -> show ft ++ "^" ++ show i

instance Show FuncType where
  show = \(FT (it, ot)) -> show it ++ " => " ++ show ot

instance Show InOrOutType where
  show = \case
    TIOV2 tiov -> show tiov
    TA2 ta -> show ta
    PoT2 pt -> show pt
    PT2 pt -> show pt
    FT2 ft -> "(" ++ show ft ++ ")"

instance Show Condition where
  show = \(Co pn) -> show pn ++ " ==> "

-- TypeDef, TypeNickname
instance Show TypeDef where
  show = \case
    TTD1 ttd -> show ttd
    OTD1 otd -> show otd

instance Show TupleTypeDef where
  show = \(TTD (tn, pcsis, ttde)) ->
    "tuple_type " ++ show tn ++ "\nvalue\n  " ++ show pcsis ++ " : " ++
    show ttde

instance Show ProdOrPowerType where
  show = \case
    PT4 pt -> show pt
    PoT4 pt -> show pt

instance Show TypeName where
  show = \(TN (maybe_pvip1, tid, pvip_str_pairs, maybe_pvip2)) ->
    show_maybe maybe_pvip1 ++ show tid ++
    concatMap (\(pvip, str) -> show pvip ++ str) pvip_str_pairs ++
    show_maybe maybe_pvip2

instance Show ParamVarsInParen where
  show = \(PVIP (ptv, ptvs)) -> "(" ++ show ptv ++ show_list_comma ptvs ++ ")"

instance Show IdTuple where
  show = \(PCSIs (sid, sids)) -> "(" ++ show sid ++ show_list_comma sids ++ ")"

instance Show OrTypeDef where
  show =
    \(OTD (tn, sid, mst, id_mst_pairs)) ->
    "or_type " ++ show tn ++
    "\nvalues\n  " ++ show sid ++ show_mst mst ++
    concatMap show_id_mst_pair id_mst_pairs
    where
    show_mst :: Maybe SimpleType -> String
    show_mst = \case
      Nothing -> ""
      Just st -> ":" ++ show st

    show_id_mst_pair :: (SimpleId, Maybe SimpleType) -> String
    show_id_mst_pair = \(sid, mst) -> " | " ++ show sid ++ show_mst mst

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
    show pnl ++ "\nequivalent\n  " ++ show pn ++ show_list_comma pns

instance Show PropNameLine where
  show = \(PNL pn) ->
    "type_proposition " ++ show pn

instance Show PropName where
  show = \case
    NPStart1 (c, np_ahvip_pairs, maybe_np) ->
      [c] ++ show_pair_list np_ahvip_pairs ++ show_maybe maybe_np
    AHVIPStart1 (ahvip_np_pairs, maybe_ahvip) ->
      show_pair_list ahvip_np_pairs ++ show_maybe maybe_ahvip

instance Show AdHocVarsInParen where
  show =
    \(AHVIP (ahtv, ahtvs)) -> "(" ++ show ahtv ++ show_list_comma ahtvs ++ ")"

instance Show NamePart where
  show = \(NP str) -> str

-- TypeTheo 
instance Show TypeTheo where
  show = \(TT (pnws, maybe_pnws, proof)) ->
    "type_theorem " ++ show pnws ++ show_mpnws maybe_pnws ++
    "\nproof" ++ show proof
    where
    show_mpnws :: Maybe PropNameWithSubs -> String
    show_mpnws = \case
      Nothing -> ""
      Just pnws -> " => " ++ show pnws

instance Show PropNameWithSubs where
  show = \case
    NPStart2 (c, np_sip_pairs, maybe_np) ->
      [c] ++ show_pair_list np_sip_pairs ++ show_maybe maybe_np
    SIPStart (sip_np_pairs, maybe_sip) ->
      show_pair_list sip_np_pairs ++ show_maybe maybe_sip

instance Show SubsInParen where
  show = \(SIP (tvs, tvss)) -> "(" ++ show tvs ++ show_list_comma tvss ++ ")"

instance Show TVarSub where
  show = \case
    TIOV4 tiov -> show tiov
    TAS1 tas -> show tas
    PoTS1 pts -> show pts
    PTS1 pts -> show pts
    FTS1 fts -> show fts

instance Show TypeAppSub where
  show = \case
    TIWS1 (maybe_souip1, tiws, maybe_souip2) ->
      show_maybe maybe_souip1 ++ show tiws ++ show_maybe maybe_souip2
    SOUIP_TI (souip, tid_or_tv, maybe_souip) ->
      show souip ++ show tid_or_tv ++ show_maybe maybe_souip
    TI_SOUIP (tid_or_tv, souip) ->
      show tid_or_tv ++ show souip

instance Show TypeIdWithSubs where
  show = \(TIWS (tid, souip_str_pairs)) ->
    show tid ++ concatMap (\(souip, str) -> show souip ++ str) souip_str_pairs

instance Show SubsOrUndersInParen where
  show = \(SOUIP (sou, sous)) -> "(" ++ show sou ++ show_list_comma sous ++ ")"

instance Show SubOrUnder where
  show = \case
    TVS1 tvs -> show tvs
    Underscore4 -> "_"

instance Show PowerTypeSub where
  show = \(PoTS (pbts, i)) -> show pbts ++ "^" ++ show i

instance Show PowerBaseTypeSub where
  show = \case
    Underscore5 -> "_"
    TIOV5 tid_or_var -> show tid_or_var
    TAS2 tas -> show tas
    IPTS1 ipts -> "(" ++ show ipts ++ ")"

instance Show InParenTSub where
  show = \case
    PTS2 pts -> show pts
    FTS2 fts -> show fts

instance Show ProdTypeSub where
  show = \(PTS (fts, fts_l)) -> show fts ++ show_list_sep " x " fts_l

instance Show FieldTypeSub where
  show = \case
    PBTS1 pbts -> show pbts
    PoTS2 pots -> show pots

instance Show FuncTypeSub where
  show = \(FTS (ioots1, ioots2)) -> show ioots1 ++ " => " ++ show ioots2 

instance Show InOrOutTypeSub where
  show = \case
    Underscore6 -> "_"
    TIOV6 tiov -> show tiov
    TAS3 tas -> show tas
    PoTS3 pots -> show pots
    PTS3 pts -> show pts
    FTS3 fts -> show fts

instance Show Proof where
  show = \case
    P1 (iooe, le) -> " " ++ show iooe ++ " " ++ show le
    P2 (iooe, ttve) -> "\n  " ++ show iooe ++ show ttve

instance Show IdOrOpEq where
  show = \(IOOE (id, maybe_op_id)) ->
    show id ++ show_moi maybe_op_id ++ " ="
    where
    show_moi :: Maybe (Op, Identifier) -> String
    show_moi = \case
      Nothing -> ""
      Just (op, id) -> show op ++ show id

instance Show TTValueExpr where
  show = \case
    LE2 le -> " " ++ show le
    VEMWE (ve, mwe) -> "\n    " ++ show ve ++ show_maybe mwe

-- Program
instance Show Program where
  show = \(P (pp, pps)) -> show pp ++ show_list_sep "\n\n" pps

instance Show ProgramPart where
  show = \case
    VD2 vd -> show vd
    GVDs2 gvds -> show gvds
    TD td -> show td
    TNN1 tnn -> show tnn
    TPD tpd -> show tpd
    TT1 tt -> show tt

-- For fast vim navigation
-- Parsers.hs
-- Testing.hs
-- ASTTypes.hs
