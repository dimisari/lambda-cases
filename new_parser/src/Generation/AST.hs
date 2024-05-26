{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module Generation.AST where

import Control.Monad.State.Lazy
import Control.Monad

import Data.List
import Data.Char

import ASTTypes
import Helpers
import ShowInstances

import Generation.TypesAndHelpers
import Generation.Collect

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId
instance ToHaskell Char where
  to_haskell = (:[])

instance ToHaskell Literal where
  to_haskell = \case
    Int i -> show i
    R r -> show r
    Ch c -> show c
    S s -> show s

instance ToHaskell Identifier where
  to_haskell (Id (muip1, id_start, id_conts, mdigit, muip2)) =
    muip1_hs ++ to_haskell id_start ++ to_haskell id_conts ++
    to_haskell mdigit ++ to_haskell muip2
    where
    muip1_hs = case muip1 of
      Nothing -> ""
      Just uip -> id_prefix ++ to_haskell uip

instance ToHaskell SimpleId where
  to_haskell = \(SId (id_start, mdigit)) ->
    to_haskell id_start ++ to_haskell mdigit

instance ToHaskell IdStart where
  to_haskell = \(IS str) -> str

instance ToHaskell IdCont where
  to_haskell = \(IC (uip, str)) -> to_haskell uip ++ str

instance ToHaskell UndersInParen where
  to_haskell = \(UIP i) -> replicate i '\''

instance ToHaskell ParenExpr where
  to_haskell = \(PE ipe) -> "(" ++ to_haskell ipe ++ ")"

instance ToHaskell InsideParenExpr where
  to_haskell = \case
    LOE1 loe -> to_haskell loe
    LFE1 lfe -> to_haskell lfe

instance ToHaskell Tuple where
  to_haskell (T (leou, leous)) =
    run_generator $ add_params_to tuple_hs_gen
    where
    tuple_hs_gen :: WithParamNum Haskell
    tuple_hs_gen =
      to_hs_wpn leou >>= \leou_hs ->
      to_hs_wpn leous >>= \leous_hs ->
      return $ "ft" ++ show size ++ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

    size :: Int
    size = case leous of
      LEOUs (_, l) -> length l + 2

instance ToHsWithParamNum LineExprOrUnders where
  to_hs_wpn = \(LEOUs (leou, leous)) ->
    to_hs_wpn_list (leou : leous) >$> intercalate ", "

instance ToHsWithParamNum LineExprOrUnder where
  to_hs_wpn = \case
    LE1 le -> return $ to_haskell le
    Underscore1 -> get_next_param

instance ToHaskell LineExpr where
  to_haskell = \case
    BOAE1 boae -> to_haskell boae
    LOE2 loe -> to_haskell loe
    LFE2 lfe -> to_haskell lfe

instance ToHaskell BasicOrAppExpr where
  to_haskell = \case
    BE3 be -> to_haskell be
    PrFA1 prfa -> to_haskell prfa
    PoFA1 pofa -> to_haskell pofa

instance ToHaskell BasicExpr where
  to_haskell = \case
    Lit1 lit -> to_haskell lit
    PFAOI1 pfaoi -> to_haskell pfaoi
    T1 tuple -> to_haskell tuple
    L1 list -> to_haskell list
    SI1 spid -> error $ "special id in basic expr:" ++ show spid

instance ToHsWithIndentLvl BigTuple where
  to_hs_wil (BT (leou, btsplit, leous, leous_l)) =
    indent_all_and_concat big_tuple_hs_list
    where
    big_tuple_hs_list :: [Haskell]
    big_tuple_hs_list =
      run_generator $ add_params_to_list big_tuple_hs_list_gen

    big_tuple_hs_list_gen :: WithParamNum [Haskell]
    big_tuple_hs_list_gen =
      to_hs_wpn leou >>= \leou_hs ->
      to_hs_wpn leous >>= \leous_hs ->
      to_hs_wpn_list leous_l >>= \leous_hs_l ->
      return $ ["ft" ++ show size] ++
        case btsplit of
          NoSplit ->
            ["( " ++ leou_hs ++ ", " ++ leous_hs] ++
            map (", " ++) leous_hs_l ++
            [")"]
          Split ->
            ["( " ++ leou_hs] ++ map (", " ++) (leous_hs : leous_hs_l) ++ [")"]

    size :: Int
    size = (leous : leous_l) &> map leous_size &> sum &> (+ 1)

    leous_size :: LineExprOrUnders -> Int
    leous_size = \(LEOUs (_, l)) -> length l + 1

instance ToHaskell List where
  to_haskell (L maybe_leous) =
    run_generator $ add_params_to $ "[" ++> to_hs_wpn maybe_leous <++ "]"

instance ToHsWithIndentLvl BigList where
  to_hs_wil (BL (leous, leous_l)) =
    indent_all_and_concat big_list_hs_list
    where
    big_list_hs_list :: [Haskell]
    big_list_hs_list = run_generator $ add_params_to_list big_list_hs_list_gen

    big_list_hs_list_gen :: WithParamNum [Haskell]
    big_list_hs_list_gen =
      to_hs_wpn leous >>= \leous_hs ->
      to_hs_wpn_list leous_l >>= \leous_hs_l ->
      return $ ["[ " ++ leous_hs] ++ map (", " ++) leous_hs_l ++ ["]"]

instance ToHaskell ParenFuncAppOrId where
  to_haskell = \case
    (PFAOI (margs1, id_start, args_str_pairs, mdigit, margs2)) ->
      run_generator $ add_params_to paren_func_app_or_id_hs_gen
      where
      paren_func_app_or_id_hs_gen :: WithParamNum Haskell
      paren_func_app_or_id_hs_gen =
        change_id_hs_if_needed total_id_hs ++>
        to_hs_wpn (calc_args_list (margs1, margs2) args_str_pairs)

      total_id_hs :: Haskell
      total_id_hs =
        prefix_maybe_quotes id_prefix margs1 ++ to_haskell id_start ++
        quotes_strs_hs args_str_pairs ++ to_haskell mdigit ++
        maybe_quotes margs2

instance ToHsWithParamNum [Arguments] where
  to_hs_wpn = \args_l ->
    to_hs_wpn_list args_l >$> \case
      [] -> ""
      args_hs_list -> "(" ++ intercalate ", " args_hs_list ++ ")"

instance ToHsWithParamNum Arguments where
  to_hs_wpn = \(As leous) -> to_hs_wpn leous

-- Values: PreFunc, PostFunc, BasicExpr, Change
instance ToHaskell PreFunc where
  to_haskell = \(PF id) -> constructor_prefix ++ to_haskell id

instance ToHaskell PreFuncApp where
  to_haskell = \(PrFA (pf, oper)) ->
    run_generator $
    add_params_to $ (to_haskell pf ++ "(") ++> to_hs_wpn oper <++ ")"

instance ToHaskell PostFunc where
  to_haskell = \case
    SId1 sid -> to_haskell sid
    SI2 spid -> spid_projection_prefix ++ to_haskell spid

instance ToHaskell SpecialId where
  to_haskell = \case
    First -> "1st"
    Second -> "2nd"
    Third -> "3rd"
    Fourth -> "4th"
    Fifth -> "5th"

instance ToHaskell PostFuncApp where
  to_haskell (PoFA (pfa, pfae)) =
    case pfa of
      Underscore2 -> "(\\" ++ under_pfarg_param ++ " -> " ++ inside_hs ++")"
      _ -> inside_hs
    where
    pfa_hs :: Haskell
    pfa_hs = to_haskell pfa

    inside_hs :: Haskell
    inside_hs = case pfae of
      DC1 dc -> to_haskell (dc, pfa_hs)
      PFsMDC (pfs, mdc) -> case mdc of
        Nothing -> pfa_pfs_hs
        Just dc -> to_haskell (dc, pfa_pfs_hs)
        where
        pfa_pfs_hs :: Haskell
        pfa_pfs_hs = pfa_pfs_to_hs $ reverse pfs

        pfa_pfs_to_hs :: [PostFunc] -> Haskell
        pfa_pfs_to_hs = \case
          [] -> pfa_hs
          pf : pfs -> to_haskell pf ++ "(" ++ pfa_pfs_to_hs pfs ++ ")"

instance ToHaskell PostFuncArg where
  to_haskell = \case
    PE2 pe -> to_haskell pe
    BE2 be -> to_haskell be
    Underscore2 -> under_pfarg_param

instance ToHaskell (DotChange, DotChangeArgHs) where
  to_haskell (DC (fc, fcs), dcahs) =
    run_generator $ add_params_to (change_hs_gen <++ (" " ++ dcahs))
    where
    change_hs_gen :: WithParamNum Haskell
    change_hs_gen =
      to_hs_wpn_list (fc : fcs) >$> \case
        [] -> error "should be impossible"
        [fc_hs] -> fc_hs
        fcs_hs_list -> "(" ++ intercalate " .> " fcs_hs_list ++ ")"

instance ToHsWithParamNum FieldChange where
  to_hs_wpn = \(FC (f, leou)) ->
    (to_haskell f ++ "(") ++> to_hs_wpn leou <++ ")"

instance ToHaskell Field where
  to_haskell = \case
    SId2 id -> change_prefix ++ to_haskell id
    SI3 spid -> spid_change_prefix ++ to_haskell spid

-- Values: OpExpr
instance ToHsWithIndentLvl OpExpr where
  to_hs_wil = \case
    LOE3 loe -> indent <++ to_haskell loe
    BOE1 boe -> to_hs_wil boe

instance ToHsWithParamNum OpExprStart where
  to_hs_wpn (OES oper_op_pairs) = to_hs_wpn_list oper_op_pairs >$> concat

instance ToHsWithParamNum (Operand, Op) where
  to_hs_wpn = \(oper, op) -> to_hs_wpn oper <++ to_haskell op

instance ToHaskell LineOpExpr where
  to_haskell = \(LOE (oes, loee)) ->
    run_generator $ add_params_to $ to_hs_wpn oes >++< to_hs_wpn loee

instance ToHsWithParamNum LineOpExprEnd where
  to_hs_wpn = \case
    O1 o -> to_hs_wpn o
    LFE3 lfe -> return $ to_haskell lfe

instance ToHsWithIndentLvl BigOpExpr where
  to_hs_wil = \case
    BOEOS1 boeos -> to_hs_wil boeos
    BOEFS1 boefs -> to_hs_wil boefs

instance ToHsWithIndentLvl BigOpExprOpSplit where
  to_hs_wil (BOEOS (osls, maybe_oes, ose)) =
    indent_all_and_concat boeos_hs_list >++< to_hs_wil ose
    where
    boeos_hs_list :: [Haskell]
    boeos_hs_list = run_generator $ add_params_to_list boeos_hs_list_gen

    boeos_hs_list_gen :: WithParamNum [Haskell]
    boeos_hs_list_gen =
      to_hs_wpn_list osls >>= \osls_hs ->
      to_hs_wpn maybe_oes >>= \maybe_oes_hs ->
      to_hs_wpn ose >>= \ose_hs ->
      return $ osls_hs ++ [maybe_oes_hs ++ ose_hs]

instance ToHsWithParamNum OpSplitLine where
  to_hs_wpn = \case
    OESMOFCO (oes, mofco) -> to_hs_wpn oes >++< to_hs_wpn mofco
    OFCO1 ofco -> to_hs_wpn ofco

instance ToHsWithParamNum OperFCO where
  to_hs_wpn = \(OFCO (oper, fco)) ->
    to_hs_wpn oper <++ (" " ++ to_haskell fco ++ " ")

instance ToHsWithParamNum OpSplitEnd where
  to_hs_wpn = \case
    O2 o -> to_hs_wpn o
    _ -> return ""

instance ToHsWithIndentLvl OpSplitEnd where
  to_hs_wil = \case
    FE1 fe -> to_hs_wil (fe, NoWhereExpr)
    _ -> return ""

instance ToHsWithIndentLvl BigOpExprFuncSplit where
  to_hs_wil (BOEFS (oes, bocfe)) =
    indent_all_and_concat params_and_oes_hs_list >++< to_hs_wil bocfe
    where
    params_and_oes_hs_list :: [Haskell]
    params_and_oes_hs_list =
      run_generator $ add_params_to_list $ to_hs_wpn_list [oes]

instance ToHsWithIndentLvl BigOrCasesFuncExpr where
  to_hs_wil = \case
    BFE1 bfe -> to_hs_wil (bfe, NoWhereExpr)
    CFE1 cfe -> to_hs_wil (cfe, NoWhereExpr)

instance ToHsWithParamNum Operand where
  to_hs_wpn = \case
    BOAE2 boae -> return $ to_haskell boae
    PE3 pe -> return $ to_haskell pe
    Underscore3 -> get_next_param

instance ToHaskell Op where
  to_haskell = \case
    FCO3 fco -> " " ++ to_haskell fco ++ " "
    OSO oso -> " " ++ to_haskell oso ++ " "

instance ToHaskell FuncCompOp where
  to_haskell = \case
    RightComp -> ".>"
    LeftComp -> "<."

instance ToHaskell OptionalSpacesOp where
  to_haskell = \case
    RightApp -> "&>"
    LeftApp -> "<&"
    Power -> "!^"
    Mult -> "!*"
    Div -> "!/"
    Plus -> "!+"
    Minus -> "!-"
    Equal -> "!=="
    NotEqual -> "!!="
    Greater -> "!>"
    Less -> "!<"
    GrEq -> "!>="
    LeEq -> "!<="
    And -> "!&"
    Or -> "!|"
    Use -> "!>>="
    Then -> "!>>"

-- Values: FuncExpr
instance ToHsWithIndentLvl (FuncExpr, PossiblyWhereExpr) where
  to_hs_wil = \(fe, pwe) -> case fe of
    LFE4 lfe -> case pwe of
      NoWhereExpr -> return $ to_haskell lfe
      HasWhereExpr we -> to_hs_wil (lfe, we)
    BFE2 bfe -> to_hs_wil (bfe, pwe)
    CFE2 cfe -> to_hs_wil (cfe, pwe)

instance ToHaskell LineFuncExpr where
  to_haskell = \(LFE (params, lfb)) ->
    to_haskell (Whole params) ++ " " ++ to_haskell lfb

instance ToHsWithIndentLvl (LineFuncExpr, WhereExpr) where
  to_hs_wil = \(LFE (params, lfb), we) ->
    (to_haskell (Whole params) ++ "\n") ++> to_hs_wil we >++<
    indent <++ to_haskell lfb

instance ToHsWithIndentLvl (BigFuncExpr, PossiblyWhereExpr) where
  to_hs_wil (BFE (params, bfb), pwe) =
    params_hs ++> to_hs_wil pwe >++< to_hs_wil bfb
    where
    params_hs :: Haskell
    params_hs = to_haskell (Whole params) ++ "\n"

instance ToHaskell WholeParams where
  to_haskell = \(Whole params) -> "\\" ++ to_haskell params ++ " ->"

instance ToHaskell Parameters where
  to_haskell = \case
    ParamId id -> to_haskell id
    Star1 -> "_"
    Params (params, params_l) ->
      "(" ++ to_haskell params ++ to_hs_prepend_list ", " params_l ++ ")"

instance ToHaskell LineFuncBody where
  to_haskell = \case
    BOAE3 boae -> to_haskell boae
    LOE4 loe -> to_haskell loe
    LFE5 lfe -> "(" ++ to_haskell lfe ++ ")"

instance ToHsWithIndentLvl BigFuncBody where
  to_hs_wil = \case
    BOAE4 boae -> indent <++ to_haskell boae
    OE1 oe -> to_hs_wil oe
    LFE6 lfe -> indent <++ ("(" ++ to_haskell lfe ++ ")")

instance ToHsWithIndentLvl (CasesFuncExpr, PossiblyWhereExpr) where
  to_hs_wil (CFE (cps, cs, maybe_ec), pwe) =
    params_hs ++> to_hs_wil pwe >++<
    indent <++ case_of_hs >++<
    deeper (to_hs_wil (cs, maybe_ec))
    where
    (params_hs, case_of_hs) = run_generator params_and_case_of_hs_gen
      :: HsPair

    params_and_case_of_hs_gen :: WithParamNum HsPair
    params_and_case_of_hs_gen =
      to_hs_wpn cps >>= \cps_hs ->
      case_of_inner_hs_gen >>= \case_of_inner_hs ->
      return $
        ("\\" ++ cps_hs ++ " ->\n", "case " ++ case_of_inner_hs ++ " of")

instance ToHsWithIndentLvl ([Case], Maybe EndCase) where
  to_hs_wil = \(cs, maybe_ec) ->
    to_hs_wil_list cs >$> concat >++< to_hs_wil maybe_ec

instance ToHsWithParamNum CasesParams where
  to_hs_wpn = \case
    CParamId id -> return $ to_haskell id
    CasesKeyword -> get_next_param
    Star2 -> return "_"
    CParams (cps, cps_l) ->
      to_hs_wpn_list (cps : cps_l) >$> \cps_l_hs ->
      "(" ++ intercalate ", " cps_l_hs ++ ")"

instance ToHsWithIndentLvl Case where
  to_hs_wil = \(Ca (om, cb)) ->
    "\n" ++> indent <++ (to_haskell om ++ " ->") >++< deeper (to_hs_wil cb)

instance ToHsWithIndentLvl EndCase where
  to_hs_wil = \(EC (ecp, cb)) ->
    "\n" ++> indent <++ (to_haskell ecp ++ " ->") >++< deeper (to_hs_wil cb)

instance ToHaskell OuterMatching where
  to_haskell = \case
    SId3 sid -> to_haskell sid
    M1 m -> to_haskell (NoParen, m)

instance ToHaskell EndCaseParam where
  to_haskell = \case
    Id1 id -> to_haskell id
    Ellipsis -> "_"

instance ToHaskell (NeedsParenBool, Matching) where
  to_haskell = \(needs_paren, m) -> case m of
    Lit2 lit -> to_haskell lit
    PFM (pf, im) ->
      in_paren_if needs_paren $ to_haskell pf ++ " " ++ to_haskell (Paren, im)
    TM1 tm -> to_haskell tm
    LM1 lm -> to_haskell lm

instance ToHaskell (NeedsParenBool, InnerMatching) where
  to_haskell = \(needs_paren, im) -> case im of
    Star -> "_"
    Id2 id -> to_haskell id
    M2 m -> to_haskell (needs_paren, m)

instance ToHaskell TupleMatching where
  to_haskell (TM (im, im_l)) =
    "(" ++ ims_hs ++ ")"
    where
    ims_hs :: Haskell
    ims_hs =
      (im : im_l) &> map (\im -> to_haskell (NoParen, im)) &> intercalate ", "

instance ToHaskell ListMatching where
  to_haskell (LM m_list_internals) =
    case m_list_internals of
      Nothing -> "[]"
      Just (im, im_l, Nothing) -> "[" ++ commas_ims_hs (im, im_l) ++ "]"
      Just (im, im_l, Just rlm) -> colons_ims_hs (im, im_l) ++ to_haskell rlm
    where
    commas_ims_hs :: (InnerMatching, [InnerMatching]) -> Haskell
    commas_ims_hs = \(im, im_l) ->
      (im : im_l) &> map (\im -> to_haskell (NoParen, im)) &> intercalate ", "

    colons_ims_hs :: (InnerMatching, [InnerMatching]) -> Haskell
    colons_ims_hs = \(im, im_l) ->
      (im : im_l) &> map (\im -> to_haskell (Paren, im)) &>
      concatMap (++ " : ")


instance ToHaskell RestListMatching where
  to_haskell = \(RLM msid) ->
    case msid of
      Nothing -> "_"
      Just sid -> to_haskell sid

instance ToHsWithIndentLvl CaseBody where
  to_hs_wil = \case
    LFB1 lfb -> return $ " " ++ to_haskell lfb
    BFB1 (bfb, maybe_we) -> "\n" ++> to_hs_wil maybe_we >++< to_hs_wil bfb

-- Values: ValueDef, GroupedValueDefs, WhereExpr
instance ToHsWithIndentLvl ValueDef where
  to_hs_wil (VD (id, t, ve, maybe_we)) =
    get >>= \il ->
    indent <++
    (to_haskell id ++ " :: " ++ forall_hs il ++ to_haskell t ++ "\n") >++<
    indent <++ (to_haskell id ++ " =\n") >++<
    deeper (to_hs_wil (ve, mwe_to_pwe maybe_we))
    where
    forall_hs :: Int -> Haskell
    forall_hs = \case
      0 ->
        case concatMap (" " ++) param_t_vars_hs_list of
          "" -> ""
          hs -> "forall" ++ hs ++ ". "
      _ -> ""

    param_t_vars_hs_list :: [Haskell]
    param_t_vars_hs_list = param_t_vars t &> map to_haskell

instance ToHsWithIndentLvl (ValueExpr, PossiblyWhereExpr) where
  to_hs_wil = \(ve, pwe) -> case ve of
    FE2 fe -> indent >++< to_hs_wil (fe, pwe)
    _ ->
      to_hs_wil pwe >++< case ve of
        BOAE5 boae -> indent <++ to_haskell boae
        OE2 oe -> to_hs_wil oe
        BT1 bt -> to_hs_wil bt
        BL1 bl -> to_hs_wil bl
        _ ->
          error "should be impossible: value expr possibilities not exhausted"

instance ToHsWithIndentLvl GroupedValueDefs where
  to_hs_wil (GVDs (id, ids, ts, les, les_l)) =
    to_hs_wil_list vd_list >$> intercalate "\n\n"
    where
    vd_list :: [ValueDef]
    vd_list = to_val_def_list (total_ids, t_list, total_le_list)

    total_ids :: [Identifier]
    total_ids = id : ids

    t_list :: [Type]
    t_list = case ts of
      Ts (t, ts) -> t : ts
      All t -> replicate (length total_ids) t

    total_le_list :: [LineExpr]
    total_le_list = concatMap (\(LEs (le, le_l)) -> le : le_l) (les : les_l)

    to_val_def_list :: ([Identifier], [Type], [LineExpr]) -> [ValueDef]
    to_val_def_list = \case
      ([], [], []) -> []
      (id : ids, t : ts, le : les) ->
        VD (id, t, le_to_ve le, Nothing) : to_val_def_list (ids, ts, les)
      _ ->
        error $
          "identifiers, types and expressions don't match in number " ++
          "in grouped value definitions"

    le_to_ve :: LineExpr -> ValueExpr
    le_to_ve = \case
      BOAE1 boae -> BOAE5 boae
      LOE2 loe -> OE2 $ LOE3 loe
      LFE2 lfe -> FE2 $ LFE4 lfe

instance ToHsWithIndentLvl WhereExpr where
  to_hs_wil (WE (wde, wdes)) =
    indent <++ "let\n" >++<
    (to_hs_wil_list (wde : wdes) >$> intercalate "\n\n") <++ "\n" >++<
    indent <++ "in\n"

instance ToHsWithIndentLvl WhereDefExpr where
  to_hs_wil = \case
    VD1 vd -> to_hs_wil vd
    GVDs1 gvd -> to_hs_wil gvd

-- Type
instance ToHaskell Type where
  to_haskell = \(Ty (maybe_c, st)) ->
    to_haskell maybe_c ++ to_haskell (NoParen, st)

instance ToHaskell (NeedsParenBool, SimpleType) where
  to_haskell = \(needs_paren, st) -> case st of
    PTV1 ptv -> to_haskell ptv
    TAIOA1 taioa -> to_haskell (needs_paren, taioa)
    PoT1 pt -> to_haskell pt
    PT1 pt -> to_haskell pt
    FT1 ft -> in_paren_if needs_paren $ to_haskell ft

instance ToHaskell TypeId where
  to_haskell = \(TId str) -> str

instance ToHaskell TypeVar where
  to_haskell = \case
    PTV2 ptv -> to_haskell ptv
    AHTV1 ahtv -> to_haskell ahtv

instance ToHaskell ParamTVar where
  to_haskell = \(PTV i) -> param_t_var_prefix ++ show i

instance ToHaskell AdHocTVar where
  to_haskell = \(AHTV c) -> ad_hoc_t_var_prefix ++ show (ord c - 65)

instance ToHaskell (NeedsParenBool, TypeAppIdOrAHTV) where
  to_haskell (needs_paren, TAIOA (mtip1, taioam, mtip2)) = case taioam of
    AHTV2 ahtv ->
      in_paren_if_needed (to_haskell ahtv) tip_hs
      where
      tip_hs :: Haskell
      tip_hs = to_haskell mtip1 ++ to_haskell mtip2

    TIdStart (tid, tip_str_pairs) ->
      in_paren_if_needed tid_hs tip_hs
      where
      tid_hs :: Haskell
      tid_hs =
        prefix_maybe_quotes tid_prefix mtip1 ++ to_haskell tid ++
        quotes_strs_hs tip_str_pairs ++ maybe_quotes mtip2

      tip_hs :: Haskell
      tip_hs =
        to_haskell mtip1 ++ to_haskell (map fst tip_str_pairs) ++
        to_haskell mtip2
    where
    in_paren_if_needed :: Haskell -> Haskell -> Haskell
    in_paren_if_needed = \hs tip_hs ->
      case tip_hs of
        "" -> hs
        _ -> in_paren_if needs_paren $ hs ++ tip_hs

instance ToHaskell TypesInParen where
  to_haskell = \(TIP (st, sts)) ->
    to_hs_prepend_list " " $ map (\st -> (Paren, st)) $ st : sts

instance ToHaskell ProdType where
  to_haskell = \(PT (ft, fts)) ->
    "(" ++ ((ft : fts) &> map to_haskell &> intercalate ", ") ++ ")"

instance ToHaskell FieldType where
  to_haskell = \case
    PBT1 ft -> to_haskell ft
    PoT3 pt -> to_haskell pt

instance ToHaskell PowerBaseType where
  to_haskell = \case
    PTV3 ptv -> to_haskell ptv
    TAIOA2 taioa -> to_haskell (NoParen, taioa)
    IPT ipt -> to_haskell ipt

instance ToHaskell InParenT where
  to_haskell = \case
    PT3 pt -> to_haskell pt
    FT3 ft -> to_haskell ft

instance ToHaskell PowerType where
  to_haskell = \(PoT (ft, i)) ->
    "(" ++ (replicate i ft &> map to_haskell &> intercalate ", ") ++ ")"

instance ToHaskell FuncType where
  to_haskell = \(FT (it, ot)) -> to_haskell it ++ " -> " ++ to_haskell ot

instance ToHaskell InOrOutType where
  to_haskell = \case
    PTV4 ptv -> to_haskell ptv
    TAIOA3 taioa -> to_haskell (NoParen, taioa)
    PoT2 pt -> to_haskell pt
    PT2 pt -> to_haskell pt
    FT2 ft -> "(" ++ to_haskell ft ++ ")"

instance ToHaskell Condition where
  to_haskell = \(Co pn) -> to_haskell pn ++ " => "

-- TypeDef, TypeNickname
instance ToHaskell TypeDef where
  to_haskell = \case
    TTD1 ttd -> to_haskell ttd
    OTD1 otd -> to_haskell otd

instance ToHaskell TupleTypeDef where
  to_haskell (TTD (tn, PCSIs (si, sis), popt)) =
    data_hs ++ "\n\n" ++ instance_hs ++ "\n" ++
    change_types_hs ++ change_defs_hs
    where
    data_hs :: Haskell
    data_hs =
      "data " ++ tn_hs ++ " =\n  " ++ cons_hs ++
      " { " ++ projections_and_types_hs ++ " }"

    instance_hs :: Haskell
    instance_hs =
      "instance FromTuple" ++ size_hs ++ types_hs ++ " " ++
      to_haskell (Paren, tn) ++ " where\n" ++
      "  ft" ++ size_hs ++
      " = \\(" ++ intercalate ", " params_list ++ ") -> " ++
      cons_hs ++ concatMap (" " ++) params_list

    change_types_hs :: Haskell
    change_types_hs = combine_with_ts change_hs_list change_types_hs_list

    change_defs_hs :: Haskell
    change_defs_hs = combine_with_defs change_hs_list sid_hs_list

    sid_hs_list :: [Haskell]
    sid_hs_list = map to_haskell $ si : sis

    change_hs_list :: [Haskell]
    change_hs_list = map (change_prefix ++) sid_hs_list

    change_types_hs_list :: [Haskell]
    change_types_hs_list = map to_change_type [0..4]

    to_change_type :: Int -> Haskell
    to_change_type = \i ->
      (types_hs_list !! i) ++ " -> " ++ tn_hs  ++ " -> " ++ tn_hs

    tn_hs :: Haskell
    tn_hs = to_haskell (NoParen, tn)

    types_list :: [SimpleType]
    types_list = case popt of
      PT4 (PT (ft, fts)) -> map ft_to_st $ ft : fts
      PoT4 (PoT (pbt, i)) -> replicate i $ pbt_to_st pbt

    types_hs_list :: [Haskell]
    types_hs_list = map (\st -> to_haskell (NoParen, st)) types_list

    types_hs :: Haskell
    types_hs = concatMap (\st -> " " ++ to_haskell (Paren, st)) types_list

    size :: Int
    size = length types_hs_list

    size_hs :: Haskell
    size_hs = show size

    projections_and_types_hs :: Haskell
    projections_and_types_hs =
      zipWith (\a b -> a ++ " :: " ++ b) sid_hs_list types_hs_list &>
      intercalate ", "

    cons_hs :: Haskell
    cons_hs = tn_to_cons_hs tn

    params_list :: [Haskell]
    params_list = [1..size] &> map (\i -> "x" ++ show i)

instance ToHaskell ProdOrPowerType where
  to_haskell = \case
    PT4 pt -> to_haskell pt
    PoT4 pt -> to_haskell pt

instance ToHaskell (NeedsParenBool, TypeName) where
  to_haskell (needs_paren, tn@(TN (mpvip1, _, pvip_str_pairs, mpvip2))) =
    case pvips_hs of
      "" -> tn_to_tid_hs tn
      _ -> in_paren_if needs_paren $ tn_to_tid_hs tn ++ pvips_hs
    where
    pvips_hs :: Haskell
    pvips_hs = to_haskell mpvip1 ++ to_haskell pvips ++ to_haskell mpvip2

    pvips :: [ParamVarsInParen]
    pvips = map fst pvip_str_pairs

instance ToHaskell ParamVarsInParen where
  to_haskell = \(PVIP (ptv, ptvs)) -> to_hs_prepend_list " " $ ptv : ptvs

instance ToHaskell OrTypeDef where
  to_haskell (OTD (tn, id, mst, id_mst_pairs)) =
    "data " ++ to_haskell (NoParen, tn) ++ " =\n  " ++
    (map id_mst_to_hs ((id, mst) : id_mst_pairs) &> intercalate " |\n  ")
    where
    id_mst_to_hs :: (SimpleId, Maybe SimpleType) -> Haskell
    id_mst_to_hs = \(id, mst) ->
      constructor_prefix ++ to_haskell id ++ case mst of
        Nothing -> ""
        Just st -> " " ++ to_haskell (Paren, st)

instance ToHaskell TypeNickname where
  to_haskell = \(TNN (tn, st)) ->
    "type " ++ to_haskell (NoParen, tn) ++ " = " ++ to_haskell (NoParen, st)

-- TypePropDef
instance ToHaskell TypePropDef where
  to_haskell = \case
    APD1 apd -> to_haskell apd
    RPD1 rpd -> to_haskell rpd

instance ToHaskell AtomPropDef where
  to_haskell = \(APD (pnl, id, st)) ->
    "class " ++ to_haskell pnl ++ " where\n  " ++
    to_haskell id ++ " :: " ++ to_haskell (NoParen, st)

instance ToHaskell RenamingPropDef where
  to_haskell = show

instance ToHaskell PropNameLine where
  to_haskell = \(PNL pn) -> to_haskell pn

instance ToHaskell PropName where
  to_haskell = \case
    NPStart1 np_start -> to_haskell np_start
    AHVIPStart ahvip_start -> to_haskell ahvip_start

instance ToHaskell NPStart1 where
  to_haskell (c, np_ahvip_pairs, maybe_np) =
    [c] ++ nps_quotes_hs np_ahvip_pairs ++ to_hs_maybe_np maybe_np ++
    to_haskell ahvips
    where
    ahvips :: [TypesInParen]
    ahvips = map snd np_ahvip_pairs

instance ToHaskell AHVIPStart where
  to_haskell (ahvip_np_pairs, maybe_ahvip) =
    quotes_nps_hs ahvip_np_pairs ++ to_haskell ahvips ++ to_haskell maybe_ahvip
    where
    ahvips :: [TypesInParen]
    ahvips = map fst ahvip_np_pairs

instance ToHaskell NamePart where
  to_haskell = \(NP str) -> str

-- TypeTheo
instance ToHaskell TypeTheo where
  to_haskell = \(TT (pnws, maybe_pnws, proof)) ->
    "instance {-# OVERLAPS #-} " ++ to_haskell pnws ++
    mpnws_to_hs maybe_pnws ++ " where\n  " ++ to_haskell proof
    where
    mpnws_to_hs :: Maybe PropNameWithSubs -> String
    mpnws_to_hs = \case
      Nothing -> ""
      Just pnws -> " => " ++ to_haskell pnws

instance ToHaskell PropNameWithSubs where
  to_haskell = \case
    NPStart2 np_start -> to_haskell np_start
    SIPStart sip_start -> to_haskell sip_start

instance ToHaskell NPStart2 where
  to_haskell (c, np_sip_pairs, maybe_np) =
    change_prop_hs_if_needed prop_hs ++ to_haskell sips
    where
    prop_hs :: Haskell
    prop_hs = [c] ++ nps_quotes_hs np_sip_pairs ++ to_hs_maybe_np maybe_np

    sips :: [SubsInParen]
    sips = map snd np_sip_pairs

instance ToHaskell SIPStart where
  to_haskell (sip_np_pairs, maybe_sip) =
    change_prop_hs_if_needed prop_hs ++ to_haskell sips ++ to_haskell maybe_sip
    where
    prop_hs :: Haskell
    prop_hs = quotes_nps_hs sip_np_pairs

    sips :: [SubsInParen]
    sips = map fst sip_np_pairs

instance ToHaskell SubsInParen where
  to_haskell = \(SIP (tvs, tvss)) -> to_hs_prepend_list " " $ tvs : tvss

instance ToHaskell TVarSub where
  to_haskell = \case
    TV1 tv -> to_haskell tv
    TASOI1 tasoi -> to_haskell (Paren, tasoi)
    PoTS1 pts -> to_haskell pts
    PTS1 pts -> to_haskell pts
    FTS1 fts -> "(" ++ to_haskell fts  ++ ")"

instance ToHaskell (NeedsParenBool, TypeAppSubOrId) where
  to_haskell (needs_paren, TASOI (msouip1, tid, souip_str_pairs, msouip2)) =
    case souip_hs of
      "" -> tid_hs
      _ -> in_paren_if needs_paren $ tid_hs ++ souip_hs
    where
    tid_hs :: Haskell
    tid_hs =
      prefix_maybe_quotes tid_prefix msouip1 ++ to_haskell tid ++
      quotes_strs_hs souip_str_pairs ++ maybe_quotes msouip2

    souip_hs :: Haskell
    souip_hs =
      to_haskell msouip1 ++ to_haskell (map fst souip_str_pairs) ++
      to_haskell msouip2

instance ToHaskell SubsOrUndersInParen where
  to_haskell = \(SOUIP (sou, sous)) -> to_haskell $ sou : sous

instance ToHaskell SubOrUnder where
  to_haskell = \case
    TVS1 tvs -> " " ++ to_haskell tvs
    Underscore4 -> ""

instance ToHaskell PowerTypeSub where
  to_haskell = \(PoTS (pbts, i)) -> to_haskell pbts ++ "^" ++ show i

instance ToHaskell PowerBaseTypeSub where
  to_haskell = \case
    Underscore5 -> undefined
    TV2 tv -> to_haskell tv
    TASOI2 tasoi -> to_haskell (NoParen, tasoi)
    IPTS1 ipts -> to_haskell ipts

instance ToHaskell InParenTSub where
  to_haskell = \case
    PTS2 pts -> "(" ++ to_haskell pts ++ ")"
    FTS2 fts -> "(" ++ to_haskell fts ++ ")"

instance ToHaskell ProdTypeSub where
  to_haskell = \(PTS (fts, fts_l)) ->
    "(" ++ ((fts : fts_l) &> map to_haskell &> intercalate ", ") ++ ")"

instance ToHaskell FieldTypeSub where
  to_haskell = \case
    PBTS1 pbts -> to_haskell pbts
    PoTS2 pots -> to_haskell pots

instance ToHaskell FuncTypeSub where
  to_haskell = \(FTS (in_ts, out_ts)) ->
    to_haskell in_ts ++ " -> " ++ to_haskell out_ts

instance ToHaskell InOrOutTypeSub where
  to_haskell = \case
    Underscore6 -> undefined
    TV3 tv -> to_haskell tv
    TASOI3 tasoi -> to_haskell (NoParen, tasoi)
    PoTS3 pots -> to_haskell pots
    PTS3 pts -> to_haskell pts
    FTS3 fts -> to_haskell fts

instance ToHaskell Proof where
  to_haskell = \case
    P1 (iooe, le) -> to_haskell iooe ++ " " ++ to_haskell le
    P2 (iooe, ttve) -> to_haskell iooe ++ to_haskell ttve

instance ToHaskell IdOrOpEq where
  to_haskell (IOOE (id, maybe_op_id)) =
    change_id_hs_if_needed (to_haskell id) ++ maybe_op_id_hs ++ " ="
    where
    maybe_op_id_hs :: Haskell
    maybe_op_id_hs = case maybe_op_id of
      Nothing -> ""
      Just (op, id) -> to_haskell op ++ to_haskell id

instance ToHaskell TTValueExpr where
  to_haskell = \case
    LE2 le -> " " ++ to_haskell le
    VEMWE (ve, maybe_we) ->
      "\n" ++
      run_generator (twice_deeper (to_hs_wil (ve, mwe_to_pwe maybe_we)))

-- Program
instance ToHaskell Program where
  to_haskell = \(P (pp, pps)) -> to_haskell pp ++ to_hs_prepend_list "\n\n" pps

instance ToHaskell ProgramPart where
  to_haskell = \case
    VD2 vd -> run_generator $ to_hs_wil vd
    GVDs2 gvds -> run_generator $ to_hs_wil gvds
    TD td -> to_haskell td
    TNN1 tnn -> to_haskell tnn
    TPD tpd -> to_haskell tpd
    TT1 tt -> to_haskell tt

-- Helper instances
instance ToHsWithIndentLvl PossiblyWhereExpr where
  to_hs_wil = \case
    NoWhereExpr -> return ""
    HasWhereExpr we -> to_hs_wil we

-- For fast vim navigation
-- ASTTypes.hs
-- TypesAndHelpers.hs
-- Test.hs
