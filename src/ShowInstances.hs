{-
This file implements a Show instance for every type of the AST
-}

{-# language LambdaCase, TypeSynonymInstances, FlexibleInstances #-}

module ShowInstances where

import Prelude ((++), (-), (.))
import Prelude qualified as P
import Data.List qualified as L

import ASTTypes qualified as T

-- helpers

show_maybe :: P.Show a => P.Maybe a -> P.String
show_maybe = \case
  P.Nothing -> ""
  P.Just a -> P.show a

show_list :: P.Show a => [a] -> P.String
show_list = P.concatMap P.show

show_list_sep :: P.Show a => P.String -> [a] -> P.String
show_list_sep = \sep -> P.concatMap ((sep ++) . P.show)

show_list_comma :: P.Show a => [a] -> P.String
show_list_comma = show_list_sep ", "

show_pair_list :: (P.Show a, P.Show b) => [(a, b)] -> P.String
show_pair_list = P.concatMap (\(a, b) -> P.show a ++ P.show b)

show_md :: P.Maybe P.Char -> P.String
show_md = \case
  P.Nothing -> ""
  P.Just c -> [c]

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp

instance P.Show T.Literal where
  show = \case
    T.Int i -> P.show i
    T.R r -> P.show r
    T.Ch c -> P.show c
    T.S s -> P.show s

instance P.Show T.Identifier where
  show = \(T.Id (maybe_uip1, id_start, id_conts, maybe_digit, maybe_uip2)) ->
    show_maybe maybe_uip1 ++ P.show id_start ++ show_list id_conts ++
    show_md maybe_digit ++ show_maybe maybe_uip2

instance P.Show T.SimpleId where
  show = \(T.SId (id_start, maybe_digit)) ->
    P.show id_start ++ show_md maybe_digit

instance P.Show T.IdStart where
  show = \(T.IS str) -> str

instance P.Show T.IdCont where
  show = \(T.IC (uip, str)) -> P.show uip ++ str

instance P.Show T.UndersInParen where
  show = \(T.UIP i) -> "(_" ++ P.concat (P.replicate (i-1) ", _") ++")"

instance P.Show T.ParenExpr where
  show = \(T.PE pei) -> "(" ++ P.show pei ++ ")"

instance P.Show T.InsideParenExpr where
  show = \case
    T.LOE1 soe -> P.show soe
    T.LFE1 sfe -> P.show sfe

instance P.Show T.Tuple where
  show = \(T.T (loue, loues)) ->
    "(" ++ P.show loue ++ ", " ++ P.show loues ++ ")"

instance P.Show T.LineExprOrUnders where
  show = \(T.LEOUs (loue, loues)) -> P.show loue ++ show_list_comma loues

instance P.Show T.LineExprOrUnder where
  show = \case
    T.LE1 le -> P.show le
    T.Underscore1 -> "_"

instance P.Show T.LineExpr where
  show = \case
    T.BOAE1 npoa -> P.show npoa
    T.LOE2 soe -> P.show soe
    T.LFE2 sfe -> P.show sfe

instance P.Show T.BasicOrAppExpr where
  show = \case
    T.BE3 be -> P.show be
    T.PrFA1 prfa -> P.show prfa
    T.PoFA1 pofa -> P.show pofa

instance P.Show T.BasicExpr where
  show = \case
    T.Lit1 lit -> P.show lit
    T.PFAOI1 pfaoi -> P.show pfaoi
    T.T1 tuple -> P.show tuple
    T.L1 list -> P.show list
    T.SI1 sid -> P.show sid

instance P.Show T.BigTuple where
  show = \(T.BT (loue, btsplit, loues, loues_l)) ->
    "( " ++ P.show loue ++ P.show btsplit ++ ", " ++ P.show loues ++
    show_list_sep "\n, " loues_l ++ "\n)"

instance P.Show T.BigTupleSplit where
  show = \case
    T.NoSplit -> ""
    T.Split -> "\n"

instance P.Show T.List where
  show = \(T.L maybe_loues) -> "[" ++ show_maybe maybe_loues ++ "]"

instance P.Show T.BigList where
  show = \(T.BL (loues, loues_l)) ->
    "[ " ++ P.show loues ++ show_list_sep "\n, " loues_l ++ "\n]"

show_as :: T.ArgsStr -> P.String
show_as = \(args, str) -> P.show args ++ str

instance P.Show T.ParenFuncAppOrId where
  show = \(T.PFAOI (margs1, id_start, args_str_pairs, maybe_digit, margs2)) ->
    show_maybe margs1 ++ P.show id_start ++
    P.concatMap show_as args_str_pairs ++ show_md maybe_digit ++
    show_maybe margs2

instance P.Show T.Arguments where
  show = \(T.As loues) -> "(" ++ P.show loues ++ ")"

-- Values: PreFunc, PostFunc, BasicExpr, Change
instance P.Show T.PreFunc where
  show = \(T.PF sid) -> P.show sid ++ ":"

instance P.Show T.PreFuncApp where
  show = \(T.PrFA (pf, oper)) -> P.show pf ++ P.show oper

instance P.Show T.PostFunc where
  show = \case
    T.SId1 sid -> "." ++ P.show sid
    T.SI2 sid -> "." ++ P.show sid

instance P.Show T.SpecialId where
  show = \case
    T.First -> "1st"
    T.Second -> "2nd"
    T.Third -> "3rd"
    T.Fourth -> "4th"
    T.Fifth -> "5th"

instance P.Show T.PostFuncApp where
  show = \(T.PoFA (pfa, pfae)) -> P.show pfa ++ P.show pfae

instance P.Show T.PostFuncArg where
  show = \case
    T.PE2 pe -> P.show pe
    T.BE2 be -> P.show be
    T.Underscore2 -> "_"

instance P.Show T.PostFuncAppEnd where
  show = \case
    T.DC1 dc -> P.show dc
    T.PFsMDC (pfs, mdc) -> show_list pfs ++ show_maybe mdc

instance P.Show T.DotChange where
  show = \(T.DC (fc, fcs)) ->
    ".change{" ++ P.show fc ++ show_list_comma fcs ++ "}"

instance P.Show T.FieldChange where
  show = \(T.FC (f, le)) -> P.show f ++ " = " ++ P.show le

instance P.Show T.Field where
  show = \case
    T.SId2 sid -> P.show sid
    T.SI3 sid -> P.show sid

-- Values: OpExpr
instance P.Show T.OpExpr where
  show = \case
    T.LOE3 soe -> P.show soe
    T.BOE1 boe -> P.show boe

instance P.Show T.OpExprStart where
  show = \(T.OES oper_op_pairs) -> show_pair_list oper_op_pairs

instance P.Show T.LineOpExpr where
  show = \(T.LOE (oes, loee)) -> P.show oes ++ P.show loee

instance P.Show T.LineOpExprEnd where
  show = \case
    T.O1 o -> P.show o
    T.LFE3 sfe -> P.show sfe

instance P.Show T.BigOpExpr where
  show = \case
    T.BOEOS1 boeos -> P.show boeos
    T.BOEFS1 boefs -> P.show boefs

instance P.Show T.BigOpExprOpSplit where
  show = \(T.BOEOS (osls, maybe_oes, ose)) ->
    show_list osls ++ show_maybe maybe_oes ++ P.show ose

instance P.Show T.OpSplitLine where
  show = \case
    T.OESMOFCO (oes, mofco) -> P.show oes ++ show_maybe mofco ++ "\n  "
    T.OFCO1 ofco -> P.show ofco ++ "\n  "

instance P.Show T.OperFCO where
  show = \(T.OFCO (oper, fco)) -> P.show oper ++ " " ++ P.show fco

instance P.Show T.OpSplitEnd where
  show = \case
    T.O2 o -> P.show o
    T.FE1 fe -> P.show fe

instance P.Show T.BigOpExprFuncSplit where
  show = \(T.BOEFS (oes, boefs)) -> P.show oes ++ P.show boefs

instance P.Show T.BigOrCasesFuncExpr where
  show = \case
    T.BFE1 bfe -> P.show bfe
    T.CFE1 cfe -> P.show cfe

instance P.Show T.Operand where
  show = \case
    T.BOAE2 npoa -> P.show npoa
    T.PE3 pe -> P.show pe
    T.Underscore3 -> "_"

instance P.Show T.Op where
  show = \case
    T.FCO3 co -> " " ++ P.show co ++ " "
    T.OSO oso -> " " ++ P.show oso ++ " "

instance P.Show T.FuncCompOp where
  show = \case
    T.RightComp -> "o>"
    T.LeftComp -> "<o"

instance P.Show T.OptionalSpacesOp where
  show = \case
    T.RightApp -> "->"
    T.LeftApp -> "<-"
    T.Power -> "^"
    T.Mult -> "*"
    T.Div -> "/"
    T.Plus -> "+"
    T.Minus -> "-"
    T.Equal -> "=="
    T.NotEqual -> "/="
    T.Greater -> ">"
    T.Less -> "<"
    T.GrEq -> ">="
    T.LeEq -> "<="
    T.And -> "&"
    T.Or -> "|"
    T.Use -> ">>"
    T.Then -> ";"

-- Values: FuncExpr
instance P.Show T.FuncExpr where
  show = \case
    T.LFE4 sfe -> P.show sfe
    T.BFE2 bfe -> P.show bfe
    T.CFE2 cfe -> P.show cfe

instance P.Show T.LineFuncExpr where
  show = \(T.LFE (params, lfb)) -> P.show params ++ " =>" ++ P.show lfb

instance P.Show T.BigFuncExpr where
  show = \(T.BFE (params, bfb)) -> P.show params ++ " =>" ++ P.show bfb

instance P.Show T.Parameters where
  show = \case
    T.ParamId id -> P.show id
    T.Star1 -> "*"
    T.Params (params, params_l) ->
      "(" ++ P.show params ++ show_list_comma params_l ++ ")"

instance P.Show T.LineFuncBody where
  show = \case
    T.BOAE3 npoa -> " " ++ P.show npoa
    T.LOE4 soe -> " " ++ P.show soe
    T.LFE5 lfe -> " (" ++ P.show lfe ++ ")"

instance P.Show T.BigFuncBody where
  show = \case
    T.BOAE4 npoa -> "\n" ++ P.show npoa
    T.OE1 oe -> "\n" ++ P.show oe
    T.LFE6 lfe -> "\n(" ++ P.show lfe ++ ")"

instance P.Show T.CasesFuncExpr where
  show = \(T.CFE (cps, cs, maybe_ec)) ->
    P.show cps ++ show_list cs ++ show_maybe maybe_ec

instance P.Show T.CasesParams where
  show = \case
    T.CParamId id -> P.show id
    T.QuestionMark -> "?"
    T.Star2 -> "*"
    T.CParams (cps, cps_l) -> "(" ++ P.show cps ++ show_list_comma cps_l ++ ")"

instance P.Show T.Case where
  show = \(T.Ca (om, cb)) -> "\n" ++ P.show om ++ " =>" ++ P.show cb

instance P.Show T.EndCase where
  show = \(T.EC (ecp, cb)) -> "\n" ++ P.show ecp ++ " =>" ++ P.show cb

instance P.Show T.OuterMatching where
  show = \case
    T.SId3 sid -> P.show sid
    T.M1 m -> P.show m

instance P.Show T.EndCaseParam where
  show = \case
    T.Id1 id -> P.show id
    T.Ellipsis -> "..."

instance P.Show T.Matching where
  show = \case
    T.Lit2 lit -> P.show lit
    T.PFM (pf, mos) -> P.show pf ++ P.show mos
    T.TM1 tm -> P.show tm
    T.LM1 lm -> P.show lm

instance P.Show T.InnerMatching where
  show = \case
    T.Star -> "*"
    T.Id2 id -> P.show id
    T.M2 m -> P.show m

instance P.Show T.TupleMatching where
  show = \(T.TM (mos, mos_l)) ->
   "(" ++ P.show mos ++ show_list_comma mos_l ++ ")"

instance P.Show T.ListMatching where
  show (T.LM (maybe_inside_list)) =
    "[" ++ maybe_inside_list_str ++ "]"
    where
    maybe_inside_list_str :: P.String
    maybe_inside_list_str =
      case maybe_inside_list of
        P.Nothing -> ""
        P.Just (mos, mos_l, maybe_rlm) ->
          P.show mos ++ show_list_comma mos_l ++ show_maybe maybe_rlm

instance P.Show T.RestListMatching where
  show (T.RLM maybe_sid) =
    ", " ++ maybe_sid_str ++ "..."
    where
    maybe_sid_str :: P.String
    maybe_sid_str =
      case maybe_sid of
        P.Nothing -> ""
        P.Just sid -> P.show sid ++ " = "

instance P.Show T.CaseBody where
  show = \case
    T.LFB1 lfb -> P.show lfb
    T.BFB1 (bfb, maybe_we) -> P.show bfb ++ show_maybe maybe_we

-- Values: ValueDef, GroupedValueDefs, WhereExpr
instance P.Show T.ValueDef where
  show = \(T.VD (id, t, ve, maybe_we)) ->
    P.show id ++ "\n  : " ++ P.show t ++ "\n  = " ++ P.show ve ++
    show_maybe maybe_we

instance P.Show T.ValueExpr where
  show = \case
    T.BOAE5 npoa -> P.show npoa
    T.OE2 oe -> P.show oe
    T.FE2 fe -> P.show fe
    T.BT1 bt -> P.show bt
    T.BL1 bl -> P.show bl

instance P.Show T.GroupedValueDefs where
  show = \(T.GVDs (id, ids, ts, csles, csles_l)) ->
    P.show id ++ show_list_comma ids ++
    "\n  : " ++ P.show ts ++
    "\n  = " ++ P.show csles ++ show_list_sep "\n  , " csles_l

instance P.Show T.Types where
  show = \case
    T.Ts (t, ts) -> P.show t ++ show_list_comma ts
    T.All t -> "all " ++ P.show t

instance P.Show T.LineExprs where
  show = \(T.LEs (le, les)) -> P.show le ++ show_list_comma les

instance P.Show T.WhereExpr where
  show =
    \(T.WE (wde, wdes)) -> "\nwhere\n" ++ P.show wde ++ show_list_sep "\n\n" wdes

instance P.Show T.WhereDefExpr where
  show = \case
    T.VD1 vd -> P.show vd
    T.GVDs1 gvd -> P.show gvd

-- Type
instance P.Show T.Type where
  show = \(T.Ty (maybe_c, st)) -> show_maybe maybe_c ++ P.show st

instance P.Show T.SimpleType where
  show = \case
    T.PTV1 ptv -> P.show ptv
    T.TAIOA1 ta -> P.show ta
    T.PoT1 pt -> P.show pt
    T.PT1 pt -> P.show pt
    T.FT1 ft -> P.show ft

instance P.Show T.TypeId where
  show = \(T.TId str) -> str

instance P.Show T.ParamTVar where
  show = \(T.PTV i) -> "T" ++ P.show i

instance P.Show T.AdHocTVar where
  show = \(T.AHTV c) -> "@" ++ [c]

instance P.Show T.TypeAppIdOrAHTV where
  show = \(T.TAIOA (mtip1, taioam, mtip2)) ->
    show_maybe mtip1 ++ P.show taioam ++ show_maybe mtip2

instance P.Show T.TAIOAMiddle where
  show = \case
    T.TIdStart1 (tid, tip_str_pairs) ->
      P.show tid ++ P.concatMap (\(tip, str) -> P.show tip ++ str) tip_str_pairs
    T.AHTV2 ahtv -> P.show ahtv

instance P.Show T.TypesInParen where
  show = \(T.TIP (st, sts)) -> "(" ++ P.show st ++ show_list_comma sts ++ ")"

instance P.Show T.ProdType where
  show = \(T.PT (fopt, fopts)) -> P.show fopt ++ show_list_sep " x " fopts

instance P.Show T.FieldType where
  show = \case
    T.PBT1 ft -> P.show ft
    T.PoT2 pt -> P.show pt

instance P.Show T.PowerBaseType where
  show = \case
    T.PTV2 ptv -> P.show ptv
    T.TAIOA2 ta -> P.show ta
    T.IPT ipt -> P.show ipt

instance P.Show T.InParenT where
  show = \case
    T.PT3 pt -> "(" ++ P.show pt ++ ")"
    T.FT3 ft -> "(" ++ P.show ft ++ ")"

instance P.Show T.PowerType where
  show = \(T.PoT (ft, i)) -> P.show ft ++ "^" ++ P.show i

instance P.Show T.FuncType where
  show = \(T.FT (it, ot)) -> P.show it ++ " => " ++ P.show ot

instance P.Show T.InOrOutType where
  show = \case
    T.PTV3 ptv -> P.show ptv
    T.TAIOA3 ta -> P.show ta
    T.PoT4 pt -> P.show pt
    T.PT2 pt -> P.show pt
    T.FT2 ft -> "(" ++ P.show ft ++ ")"

instance P.Show T.Condition where
  show = \(T.Co pn) -> P.show pn ++ " ==> "

-- TypeDef, TypeNickname
instance P.Show T.TypeDef where
  show = \case
    T.TTD1 ttd -> P.show ttd
    T.OTD1 otd -> P.show otd

instance P.Show T.TupleTypeDef where
  show = \(T.TTD (tn, pcsis, ttde)) ->
    "tuple_type " ++ P.show tn ++ "\nvalue\n  " ++ P.show pcsis ++ " : " ++
    P.show ttde

instance P.Show T.ProdOrPowerType where
  show = \case
    T.PT4 pt -> P.show pt
    T.PoT5 pt -> P.show pt

instance P.Show T.TypeName where
  show = \(T.TN (maybe_pvip1, tid, pvip_str_pairs, maybe_pvip2)) ->
    show_maybe maybe_pvip1 ++ P.show tid ++
    P.concatMap (\(pvip, str) -> P.show pvip ++ str) pvip_str_pairs ++
    show_maybe maybe_pvip2

instance P.Show T.ParamVarsInParen where
  show = \(T.PVIP (ptv, ptvs)) ->
    "(" ++ P.show ptv ++ show_list_comma ptvs ++ ")"

instance P.Show T.FieldNames where
  show = \(T.PCSIs (sid, sids)) ->
    "(" ++ P.show sid ++ show_list_comma sids ++ ")"

instance P.Show T.OrTypeDef where
  show = \(T.OTD (tn, pv, pvs)) ->
    "or type: " ++ P.show tn ++
    "\nvalues:\n  " ++ P.show pv ++ show_list_sep " | " pvs

instance P.Show T.PossibleValue where
  show = \(T.PV (sid, maybe_with_val)) ->
    P.show sid ++ case maybe_with_val of
      P.Nothing -> ""
      P.Just (id, st) -> "--<" ++ P.show id ++ " : " ++ P.show st ++ ">"

instance P.Show T.TypeNickname where
  show = \(T.TNN (tn, st)) ->
    "type nickname: " ++ P.show tn ++ " = " ++ P.show st

-- TypePropDef
instance P.Show T.TypePropDef where
  show = \case
    T.APD1 apd -> P.show apd
    T.RPD1 rpd -> P.show rpd

instance P.Show T.AtomPropDef where
  show = \(T.APD (pnl, id, st)) ->
    P.show pnl ++ "\nvalue\n  " ++ P.show id ++ " : " ++ P.show st

instance P.Show T.RenamingPropDef where
  show = \(T.RPD (pnl, pn, pns)) ->
    P.show pnl ++ "\nequivalent\n  " ++ P.show pn ++ show_list_comma pns

instance P.Show T.PropNameLine where
  show = \(T.PNL pn) ->
    "type proposition: " ++ P.show pn

instance P.Show T.PropName where
  show = \case
    T.NPStart1 (c, np_tip_pairs, maybe_np) ->
      [c] ++ show_pair_list np_tip_pairs ++ show_maybe maybe_np
    T.TIPStart (tip_np_pairs, maybe_tip) ->
      show_pair_list tip_np_pairs ++ show_maybe maybe_tip

instance P.Show T.NamePart where
  show = \(T.NP str) -> str

-- TypeTheo
instance P.Show T.TypeTheo where
  show = \(T.TT (pnws, maybe_pnws, proof)) ->
    "type_theorem " ++ P.show pnws ++ show_mpnws maybe_pnws ++
    "\nproof" ++ P.show proof
    where
    show_mpnws :: P.Maybe T.PropNameWithSubs -> P.String
    show_mpnws = \case
      P.Nothing -> ""
      P.Just pnws -> " => " ++ P.show pnws

instance P.Show T.PropNameWithSubs where
  show = \case
    T.NPStart2 (c, np_sip_pairs, maybe_np) ->
      [c] ++ show_pair_list np_sip_pairs ++ show_maybe maybe_np
    T.SIPStart (sip_np_pairs, maybe_sip) ->
      show_pair_list sip_np_pairs ++ show_maybe maybe_sip

instance P.Show T.SubsInParen where
  show = \(T.SIP (tvs, tvss)) -> "(" ++ P.show tvs ++ show_list_comma tvss ++ ")"

instance P.Show T.TVarSub where
  show = \case
    T.PTV4 ptv -> P.show ptv
    T.TAIOAS1 tas -> P.show tas
    T.PoTS1 pts -> P.show pts
    T.PTS1 pts -> P.show pts
    T.FTS1 fts -> P.show fts

instance P.Show T.TypeAppIdOrAHTVSub where
  show = \(T.TAIOAS (msouip1, taioasm, msouip2)) ->
    show_maybe msouip1 ++ P.show taioasm ++ show_maybe msouip2

instance P.Show T.TAIOASMiddle where
  show = \case
    T.TIdStart2 (tid, souip_str_pairs) ->
      P.show tid ++
      P.concatMap (\(souip, str) -> P.show souip ++ str) souip_str_pairs
    T.AHTV3 ahtv -> P.show ahtv

instance P.Show T.SubsOrUndersInParen where
  show = \(T.SOUIP (sou, sous)) ->
    "(" ++ P.show sou ++ show_list_comma sous ++ ")"

instance P.Show T.SubOrUnder where
  show = \case
    T.TVS1 tvs -> P.show tvs
    T.Underscore4 -> "_"

instance P.Show T.PowerTypeSub where
  show = \(T.PoTS (pbts, i)) -> P.show pbts ++ "^" ++ P.show i

instance P.Show T.PowerBaseTypeSub where
  show = \case
    T.Underscore5 -> "_"
    T.PTV5 ptv -> P.show ptv
    T.TAIOAS2 tas -> P.show tas
    T.IPTS ipts -> "(" ++ P.show ipts ++ ")"

instance P.Show T.InParenTSub where
  show = \case
    T.PTS2 pts -> P.show pts
    T.FTS2 fts -> P.show fts

instance P.Show T.ProdTypeSub where
  show = \(T.PTS (fts, fts_l)) -> P.show fts ++ show_list_sep " x " fts_l

instance P.Show T.FieldTypeSub where
  show = \case
    T.PBTS1 pbts -> P.show pbts
    T.PoTS2 pots -> P.show pots

instance P.Show T.FuncTypeSub where
  show = \(T.FTS (ioots1, ioots2)) -> P.show ioots1 ++ " => " ++ P.show ioots2

instance P.Show T.InOrOutTypeSub where
  show = \case
    T.Underscore6 -> "_"
    T.PTV6 ptv -> P.show ptv
    T.TAIOAS3 tas -> P.show tas
    T.PoTS3 pots -> P.show pots
    T.PTS3 pts -> P.show pts
    T.FTS3 fts -> P.show fts

instance P.Show T.Proof where
  show = \case
    T.P1 (iooe, le) -> " " ++ P.show iooe ++ " " ++ P.show le
    T.P2 (iooe, ttve) -> "\n  " ++ P.show iooe ++ P.show ttve

instance P.Show T.IdOrOpEq where
  show = \(T.IOOE (id, maybe_op_id)) ->
    P.show id ++ show_moi maybe_op_id ++ " ="
    where
    show_moi :: P.Maybe (T.Op, T.Identifier) -> P.String
    show_moi = \case
      P.Nothing -> ""
      P.Just (op, id) -> P.show op ++ P.show id

instance P.Show T.TTValueExpr where
  show = \case
    T.LE2 le -> " " ++ P.show le
    T.VEMWE (ve, mwe) -> "\n    " ++ P.show ve ++ show_maybe mwe

-- Program
instance P.Show T.Program where
  show = \(T.P (pp, pps)) -> P.show pp ++ show_list_sep "\n\n" pps

instance P.Show T.ProgramPart where
  show = \case
    T.VD2 vd -> P.show vd
    T.GVDs2 gvds -> P.show gvds
    T.TD td -> P.show td
    T.TNN1 tnn -> P.show tnn
    T.TPD tpd -> P.show tpd
    T.TT1 tt -> P.show tt

{-
For fast vim file navigation:
Parsing/TypesAndHelpers.hs
Parsing/AST.hs
ASTTypes.hs
-}
