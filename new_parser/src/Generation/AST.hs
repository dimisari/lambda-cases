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
import Generation.FieldIds

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId
instance ToHaskell Char where
  to_haskell = (:[])

instance ToHaskell (NeedsAnnotationBool, Literal) where
  to_haskell = \(needs_annot, lit) -> case lit of
    Int i ->
      case needs_annot of
        NoAnnotation -> show i
        Annotation -> "(" ++ show i ++ " :: Int)"
    R r ->
      case needs_annot of
        NoAnnotation -> show r
        Annotation -> "(" ++ show r ++ " :: Double)"
    Ch c -> show c
    S s -> show s

instance ToHaskell Identifier where
  to_haskell (Id (muip1, id_start, id_conts, mdigit, muip2)) =
    muip1_hs ++ to_haskell id_start ++ to_haskell id_conts ++
    to_haskell mdigit ++ to_haskell muip2
    where
    muip1_hs = case muip1 of
      Nothing -> ""
      Just uip -> "a0" ++ to_haskell uip

instance ToHaskell SimpleId where
  to_haskell = \(SId sid_tuple) ->
    case sid_tuple of
      (IS "true", Nothing) -> "True"
      (IS "false", Nothing) -> "False"
      (id_start, mdigit) -> to_haskell id_start ++ to_haskell mdigit

instance ToHaskell IdStart where
  to_haskell = \(IS str) -> str

instance ToHaskell IdCont where
  to_haskell = \(IC (uip, str)) -> to_haskell uip ++ str

instance ToHaskell UndersInParen where
  to_haskell = \(UIP i) -> replicate i '\''

instance ToHaskell (ParenExpr, PossiblyDCAHs) where
  to_haskell = \(PE ipe, pdcahs) -> "(" ++ to_haskell (ipe, pdcahs) ++ ")"

instance ToHsWithDCS ParenExpr where
  to_hs_wdcs = \(PE ipe) -> "(" ++> to_hs_wdcs ipe <++ ")"

instance ToHaskell (InsideParenExpr, PossiblyDCAHs) where
  to_haskell = \(ipe, pdcahs) -> case ipe of
    LOE1 loe -> to_haskell (loe, pdcahs)
    LFE1 lfe -> to_haskell (lfe, pdcahs)

instance ToHsWithDCS InsideParenExpr where
  to_hs_wdcs = \case
    LOE1 loe -> to_hs_wdcs loe
    LFE1 lfe -> to_hs_wdcs lfe

instance ToHaskell (Tuple, PossiblyDCAHs) where
  to_haskell (T (leou, leous), pdcahs) =
    run_generator $ add_params_to tuple_hs_gen
    where
    tuple_hs_gen :: WithParamNum Haskell
    tuple_hs_gen =
      to_hs_wpn (leou, pdcahs) >>= \leou_hs ->
      to_hs_wpn (leous, pdcahs) >>= \leous_hs ->
      return $ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

instance ToHsWithDCS Tuple where
  to_hs_wdcs (T (leou, leous)) =
    run_pndc_gen $ add_params_to_pndc tuple_hs_gen
    where
    tuple_hs_gen :: PNAndDCState Haskell
    tuple_hs_gen =
      to_hs_wpndcs leou >>= \leou_hs ->
      to_hs_wpndcs leous >>= \leous_hs ->
      return $ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

instance ToHsWithParamNum (LineExprOrUnders, PossiblyDCAHs) where
  to_hs_wpn (LEOUs (leou, leous), pdcahs) =
    to_hs_wpn_list leous_with_hs $> intercalate ", "
    where
    leous_with_hs :: [(LineExprOrUnder, PossiblyDCAHs)]
    leous_with_hs = map (\leou -> (leou, pdcahs)) $ leou : leous

instance ToHsWithPNDCS LineExprOrUnders where
  to_hs_wpndcs (LEOUs (leou, leous)) =
    to_hs_wpndcs_list (leou : leous) $> intercalate ", "

instance ToHsWithParamNum (LineExprOrUnder, PossiblyDCAHs) where
  to_hs_wpn = \(loue, pdcahs) -> case loue of
    LE1 le -> return $ to_haskell (le, pdcahs)
    Underscore1 -> get_next_param

instance ToHsWithPNDCS LineExprOrUnder where
  to_hs_wpndcs = \case
    LE1 le -> dcs_gen_to_pndc_gen $ to_hs_wdcs le
    Underscore1 -> get_next_param_pndc

instance ToHaskell (LineExpr, PossiblyDCAHs) where
  to_haskell = \(le, pdcahs) -> case le of
    BOAE1 boae -> to_haskell (boae, pdcahs)
    LOE2 loe -> to_haskell (loe, pdcahs)
    LFE2 lfe -> to_haskell (lfe, pdcahs)

instance ToHsWithDCS LineExpr where
  to_hs_wdcs = \case
    BOAE1 boae -> to_hs_wdcs boae
    LOE2 loe -> to_hs_wdcs loe
    LFE2 lfe -> to_hs_wdcs lfe

instance ToHaskell (BasicOrAppExpr, PossiblyDCAHs) where
  to_haskell = \(boae, pdcahs) -> case boae of
    BE3 be -> to_haskell (be, pdcahs)
    PrFA1 prfa -> to_haskell (prfa, pdcahs)
    PoFA1 pofa -> to_haskell pofa

instance ToHsWithDCS BasicOrAppExpr where
  to_hs_wdcs = \case
    BE3 be -> to_hs_wdcs be
    PrFA1 prfa -> to_hs_wdcs prfa
    PoFA1 pofa -> return $ to_haskell pofa

instance ToHaskell (BasicExpr, PossiblyDCAHs) where
  to_haskell = \(be, pdcahs) -> case be of
    Lit1 lit -> to_haskell (Annotation, lit)
    PFAOI1 pfaoi -> to_haskell (pfaoi, pdcahs)
    T1 tuple -> to_haskell (tuple, pdcahs)
    L1 list -> to_haskell (list, pdcahs)
    SI1 sp_id -> case pdcahs of
      NoDCAHs -> to_haskell sp_id
      WithDCAHs dcahs -> to_haskell (sp_id, dcahs)

instance ToHsWithDCS BasicExpr where
  to_hs_wdcs = \case
    Lit1 lit -> return $ to_haskell (Annotation, lit)
    PFAOI1 pfaoi -> to_hs_wdcs pfaoi
    T1 tuple -> to_hs_wdcs tuple
    L1 list -> to_hs_wdcs list
    SI1 sp_id -> to_hs_wdcs sp_id

instance ToHsWithIndentLvl BigTuple where
  to_hs_wil (BT (leou, leous, leous_l)) =
    indent_all_and_concat big_tuple_hs_list
    where
    big_tuple_hs_list :: [Haskell]
    big_tuple_hs_list = run_generator $ add_params_to2 big_tuple_hs_list_gen

    big_tuple_hs_list_gen :: WithParamNum [Haskell]
    big_tuple_hs_list_gen =
      to_hs_wpn (leou, NoDCAHs) >>= \leou_hs ->
      to_hs_wpn_list (map no_dca_hs $ leous : leous_l) >>= \leous_hs_l ->
      return $ ["( " ++ leou_hs] ++ map (", " ++) leous_hs_l ++ [")"]

instance ToHaskell (List, PossiblyDCAHs) where
  to_haskell (L maybe_leous, pdcahs) =
    run_generator $
    add_params_to $ "[" ++> to_hs_wpn (maybe_leous, pdcahs) <++ "]"

instance ToHsWithDCS List where
  to_hs_wdcs (L maybe_leous) =
    run_pndc_gen $ add_params_to_pndc $ "[" ++> to_hs_wpndcs maybe_leous <++ "]"

instance ToHsWithIndentLvl BigList where
  to_hs_wil (BL (leous, leous_l)) =
    indent_all_and_concat big_list_hs_list
    where
    big_list_hs_list :: [Haskell]
    big_list_hs_list = run_generator $ add_params_to2 big_list_hs_list_gen

    big_list_hs_list_gen :: WithParamNum [Haskell]
    big_list_hs_list_gen =
      to_hs_wpn (leous, NoDCAHs) >>= \leous_hs ->
      to_hs_wpn_list (map no_dca_hs leous_l) >>= \leous_hs_l ->
      return $ ["[ " ++ leous_hs] ++ map (", " ++) leous_hs_l ++ ["]"]

instance ToHaskell (ParenFuncAppOrId, PossiblyDCAHs) where
  to_haskell
    (PFAOI (margs1, id_start, args_str_pairs, mdigit, margs2), pdcahs) =
    run_generator $ add_params_to paren_func_app_or_id_hs_gen
    where
    paren_func_app_or_id_hs_gen :: WithParamNum Haskell
    paren_func_app_or_id_hs_gen =
      total_id_hs ++>
      to_hs_wpn (calc_args_list (margs1, margs2) args_str_pairs, pdcahs)

    total_id_hs :: Haskell
    total_id_hs =
      margs1_to_id_hs margs1 ++ to_haskell id_start ++
      args_str_pairs_to_id_hs args_str_pairs ++ to_haskell mdigit ++
      margs2_to_id_hs margs2

instance ToHsWithDCS ParenFuncAppOrId where
  to_hs_wdcs (PFAOI (margs1, id_start, args_str_pairs, mdigit, margs2)) =
    get_pdcahs >>= \pdcahs ->
    case (pdcahs, msid) of
      (WithDCAHs dcahs, Just sid) -> dcahs_sid_gen (dcahs, sid)
      _ -> run_pndc_gen $ add_params_to_pndc paren_func_app_or_id_hs_gen
    where
    paren_func_app_or_id_hs_gen :: PNAndDCState Haskell
    paren_func_app_or_id_hs_gen =
      total_id_hs ++>
      to_hs_wpndcs (calc_args_list (margs1, margs2) args_str_pairs)

    total_id_hs :: Haskell
    total_id_hs =
      margs1_to_id_hs margs1 ++ to_haskell id_start ++
      args_str_pairs_to_id_hs args_str_pairs ++ to_haskell mdigit ++
      margs2_to_id_hs margs2

    msid :: Maybe SimpleId
    msid = case (margs1, args_str_pairs, margs2) of
      (Nothing, [], Nothing) -> Just $ SId (id_start, mdigit)
      _ -> Nothing

    dcahs_sid_gen :: (DotChangeArgHs, SimpleId) -> DotChangeState Haskell
    dcahs_sid_gen = \(dcahs, sid) ->
      check_if_in_fids sid $> \case
        True -> "c0" ++ to_haskell sid ++ "(" ++ dcahs ++ ")"
        False -> to_haskell sid

instance ToHsWithParamNum ([Arguments], PossiblyDCAHs) where
  to_hs_wpn (args_l, pdcahs) =
    to_hs_wpn_list args_with_hs_l $> \case
      [] -> ""
      args_hs_list -> "(" ++ intercalate ", " args_hs_list ++ ")"
    where
    args_with_hs_l :: [(Arguments, PossiblyDCAHs)]
    args_with_hs_l = map (\args -> (args, pdcahs)) args_l

instance ToHsWithPNDCS [Arguments] where
  to_hs_wpndcs args_l =
    to_hs_wpndcs_list args_l $> \case
      [] -> ""
      args_hs_list -> "(" ++ intercalate ", " args_hs_list ++ ")"

instance ToHsWithParamNum (Arguments, PossiblyDCAHs) where
  to_hs_wpn = \(As leous, pdcahs) -> to_hs_wpn (leous, pdcahs)

instance ToHsWithPNDCS Arguments where
  to_hs_wpndcs = \(As leous) -> to_hs_wpndcs leous

-- Values: PreFunc, PostFunc, BasicExpr, Change
instance ToHaskell PreFunc where
  to_haskell = \(PF id) -> "C" ++ to_haskell id

instance ToHaskell (PreFuncApp, PossiblyDCAHs) where
  to_haskell = \(PrFA (pf, oper), pdcahs) ->
    run_generator $
    add_params_to $ (to_haskell pf ++ "(") ++> to_hs_wpn (oper, pdcahs) <++ ")"

instance ToHsWithDCS PreFuncApp where
  to_hs_wdcs = \(PrFA (pf, oper)) ->
    run_pndc_gen $
    add_params_to_pndc $ (to_haskell pf ++ "(") ++> to_hs_wpndcs oper <++ ")"

instance ToHaskell PostFunc where
  to_haskell = \case
    SId1 sid -> "p0" ++ to_haskell sid
    SI2 sp_id -> "p" ++  to_haskell sp_id

instance ToHaskell SpecialId where
  to_haskell = \case
    First -> "1st"
    Second -> "2nd"
    Third -> "3rd"
    Fourth -> "4th"
    Fifth -> "5th"

instance ToHaskell (SpecialId, DotChangeArgHs) where
  to_haskell = \(sp_id, dcahs) ->
    "p" ++ to_haskell sp_id ++ "(" ++ dcahs ++ ")"

instance ToHsWithDCS SpecialId where
  to_hs_wdcs = \sp_id ->
    get_pdcahs $> \case
      NoDCAHs -> to_haskell sp_id
      WithDCAHs dcahs -> "p" ++ to_haskell sp_id ++ "(" ++ dcahs ++ ")"

instance ToHaskell PostFuncApp where
  to_haskell (PoFA (pfa, pfae)) =
    maybe_param_hs ++ to_haskell (pfae, to_haskell pfa)
    where
    maybe_param_hs :: Haskell
    maybe_param_hs = case pfa of
      Underscore2 -> "\\x' -> "
      _ -> ""

instance ToHaskell PostFuncArg where
  to_haskell = \case
    PE2 pe -> run_no_dcahs $ to_hs_wdcs pe
    BE2 be -> run_no_dcahs $ to_hs_wdcs be
    Underscore2 -> "x'"

instance ToHaskell (PostFuncAppEnd, PostFuncArgHs) where
  to_haskell = \(pfae, pfa_hs) -> case pfae of
    DC1 dc -> to_haskell (dc, pfa_hs)
    PFsMDC (pfs, mdc) ->
      to_haskell (mdc, pfs_pfa_to_haskell $ reverse pfs)
      where
      pfs_pfa_to_haskell :: [PostFunc] -> Haskell
      pfs_pfa_to_haskell = \case
        [] -> pfa_hs
        pf:pfs -> to_haskell pf ++ "(" ++ pfs_pfa_to_haskell pfs ++ ")"

instance ToHaskell (Maybe DotChange, PFAWithPostFuncsHs) where
  to_haskell (mdc, pfawpf_hs) = case mdc of
    Nothing -> pfawpf_hs
    Just dc -> to_haskell (dc, pfawpf_hs)

instance ToHaskell (DotChange, DotChangeArgHs) where
  to_haskell (DC (fc, fcs), dcahs) =
    run_generator $ add_params_to change_hs_gen
    where
    change_hs_gen :: WithParamNum Haskell
    change_hs_gen =
      to_hs_wpn_list fcs_with_hs $> \case
        [] -> error "should be impossible"
        [fc_hs] -> fc_hs ++ " " ++ dcahs
        fcs_hs_list -> "(" ++ intercalate " .> " fcs_hs_list ++ ") " ++ dcahs

    fcs_with_hs :: [(FieldChange, DotChangeArgHs)]
    fcs_with_hs = map (\fc -> (fc, dcahs)) $ fc : fcs

instance ToHsWithParamNum (FieldChange, DotChangeArgHs) where
  to_hs_wpn = \(FC (f, leou), dcahs) ->
    (to_haskell f ++ "(") ++> to_hs_wpn (leou, WithDCAHs dcahs) <++ ")"

instance ToHsWithPNDCS FieldChange where
  to_hs_wpndcs = \(FC (f, leou)) ->
    (to_haskell f ++ "(") ++> to_hs_wpndcs leou <++ ")"

instance ToHaskell Field where
  to_haskell = \case
    SId2 id -> "c0" ++ to_haskell id
    SI3 sp_id -> "c" ++ to_haskell sp_id

-- Values: OpExpr
instance ToHsWithIndentLvl OpExpr where
  to_hs_wil = \case
    LOE3 loe -> indent <++ to_haskell (loe, NoDCAHs)
    BOE1 boe -> to_hs_wil boe

instance ToHsWithParamNum (OpExprStart, PossiblyDCAHs) where
  to_hs_wpn (OES oper_op_pairs, pdcahs) =
    to_hs_wpn_list (map (\(o, op) -> (o, op, pdcahs)) oper_op_pairs) $> concat

instance ToHsWithPNDCS OpExprStart where
  to_hs_wpndcs (OES oper_op_pairs) = to_hs_wpndcs_list oper_op_pairs $> concat

instance ToHsWithParamNum (Operand, Op, PossiblyDCAHs) where
  to_hs_wpn = \(oper, op, pdcahs) -> to_hs_wpn (oper, pdcahs) <++ to_haskell op

instance ToHsWithPNDCS (Operand, Op) where
  to_hs_wpndcs = \(oper, op) -> to_hs_wpndcs oper <++ to_haskell op

instance ToHaskell (LineOpExpr, PossiblyDCAHs) where
  to_haskell (LOE (oes, loee), pdcahs) =
    run_generator $ add_params_to $ to_hs_wpn (oes, pdcahs) >++<
    to_hs_wpn (loee, pdcahs)

instance ToHsWithDCS LineOpExpr where
  to_hs_wdcs (LOE (oes, loee)) =
    run_pndc_gen $ add_params_to_pndc $ to_hs_wpndcs oes >++<
    to_hs_wpndcs loee

instance ToHsWithParamNum (LineOpExprEnd, PossiblyDCAHs) where
  to_hs_wpn = \(loee, pdcahs) -> case loee of
    O1 o -> to_hs_wpn (o, pdcahs)
    LFE3 lfe -> return $ to_haskell (lfe, pdcahs)

instance ToHsWithPNDCS LineOpExprEnd where
  to_hs_wpndcs = \case
    O1 o -> to_hs_wpndcs o
    LFE3 lfe -> dcs_gen_to_pndc_gen $ to_hs_wdcs lfe

instance ToHsWithIndentLvl BigOpExpr where
  to_hs_wil = \case
    BOEOS1 boeos -> to_hs_wil boeos
    BOEFS1 boefs -> to_hs_wil boefs

instance ToHsWithIndentLvl BigOpExprOpSplit where
  to_hs_wil (BOEOS (osls, maybe_oes, ose)) =
    indent_all_and_concat boeos_hs_list >++< to_hs_wil ose
    where
    boeos_hs_list :: [Haskell]
    boeos_hs_list = run_generator $ add_params_to2 boeos_hs_list_gen

    boeos_hs_list_gen :: WithParamNum [Haskell]
    boeos_hs_list_gen =
      to_hs_wpn_list osls >>= \osls_hs ->
      to_hs_wpn (no_dca_hs <$> maybe_oes) >>= \maybe_oes_hs ->
      to_hs_wpn ose >>= \ose_hs ->
      return $ osls_hs ++ [maybe_oes_hs ++ ose_hs]

instance ToHsWithParamNum OpSplitLine where
  to_hs_wpn = \(OSL (oes, mofco)) ->
    to_hs_wpn (oes, NoDCAHs) >++< to_hs_wpn mofco

instance ToHsWithParamNum OperFCO where
  to_hs_wpn = \(OFCO (oper, fco)) ->
    to_hs_wpn (oper, NoDCAHs) <++ to_haskell fco

instance ToHsWithParamNum OpSplitEnd where
  to_hs_wpn = \case
    O2 o -> to_hs_wpn (o, NoDCAHs)
    _ -> return ""

instance ToHsWithIndentLvl OpSplitEnd where
  to_hs_wil = \case
    FE1 fe -> to_hs_wil fe
    _ -> return ""

instance ToHsWithIndentLvl BigOpExprFuncSplit where
  to_hs_wil (BOEFS (oes, bocfe)) =
    indent_all_and_concat params_and_oes_hs_list >++< to_hs_wil bocfe
    where
    params_and_oes_hs_list :: [Haskell]
    params_and_oes_hs_list =
      run_generator $ add_params_to2 $ to_hs_wpn_list [(oes, NoDCAHs)]

instance ToHsWithIndentLvl BigOrCasesFuncExpr where
  to_hs_wil = \case
    BFE1 bfe -> to_hs_wil bfe
    CFE1 cfe -> to_hs_wil cfe

instance ToHsWithParamNum (Operand, PossiblyDCAHs) where
  to_hs_wpn = \(o, pdcahs) -> case o of
    BOAE2 boae -> return $ to_haskell (boae, pdcahs)
    PE3 pe -> return $ to_haskell (pe, pdcahs)
    Underscore3 -> get_next_param

instance ToHsWithPNDCS Operand where
  to_hs_wpndcs = \case
    BOAE2 boae -> dcs_gen_to_pndc_gen $ to_hs_wdcs boae
    PE3 pe -> dcs_gen_to_pndc_gen $ to_hs_wdcs pe
    Underscore3 -> get_next_param_pndc

instance ToHaskell Op where
  to_haskell = \case
    FCO3 co -> " " ++ to_haskell co ++ " "
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
instance ToHsWithIndentLvl FuncExpr where
  to_hs_wil = \case
    LFE4 lfe -> return $ to_haskell (lfe, NoDCAHs)
    BFE2 bfe -> to_hs_wil bfe
    CFE2 cfe -> to_hs_wil cfe

instance ToHsWithIndentLvl (FuncExpr, WhereExpr) where
  to_hs_wil = \(fe, we) -> indent >++< case fe of
    LFE4 lfe -> to_hs_wil (lfe, we)
    BFE2 bfe -> to_hs_wil (bfe, we)
    CFE2 cfe -> to_hs_wil (cfe, we)

instance ToHaskell (LineFuncExpr, PossiblyDCAHs) where
  to_haskell = \(LFE (params, lfb), pdcahs) ->
    to_haskell (Whole params) ++ " " ++ to_haskell (lfb, pdcahs)

instance ToHsWithDCS LineFuncExpr where
  to_hs_wdcs = \(LFE (params, lfb)) ->
    (to_haskell (Whole params) ++ " ") ++> to_hs_wdcs lfb

instance ToHsWithIndentLvl (LineFuncExpr, WhereExpr) where
  to_hs_wil = \(LFE (params, lfb), we) ->
    (to_haskell (Whole params) ++ "\n") ++> to_hs_wil we >++<
    indent <++ to_haskell (lfb, NoDCAHs)

instance ToHsWithIndentLvl BigFuncExpr where
  to_hs_wil = \(BFE (params, bfb)) ->
    (to_haskell (Whole params) ++ "\n") ++> to_hs_wil bfb

instance ToHsWithIndentLvl (BigFuncExpr, WhereExpr) where
  to_hs_wil = \(BFE (params, bfb), we) ->
    (to_haskell (Whole params) ++ "\n") ++> to_hs_wil we >++< to_hs_wil bfb

instance ToHaskell WholeParams where
  to_haskell = \(Whole params) -> "\\" ++ to_haskell params ++ " ->"

instance ToHaskell Parameters where
  to_haskell = \case
    ParamId id -> to_haskell id
    Star1 -> "_"
    Params (params, params_l) ->
      "(" ++ to_haskell params ++ to_hs_prepend_list ", " params_l ++ ")"

instance ToHaskell (LineFuncBody, PossiblyDCAHs) where
  to_haskell = \(lfb, pdcahs) -> case lfb of
    BOAE3 boae -> to_haskell (boae, pdcahs)
    LOE4 loe -> to_haskell (loe, pdcahs)

instance ToHsWithDCS LineFuncBody where
  to_hs_wdcs = \case
    BOAE3 boae -> to_hs_wdcs boae
    LOE4 loe -> to_hs_wdcs loe

instance ToHsWithIndentLvl BigFuncBody where
  to_hs_wil = \case
    BOAE4 boae -> indent <++ to_haskell (boae, NoDCAHs)
    OE1 oe -> to_hs_wil oe

instance ToHsWithIndentLvl CasesFuncExpr where
  to_hs_wil = \(CFE (cps, cs, maybe_ec)) ->
    to_haskell (CaseOf cps) ++> deeper (to_hs_wil (cs, maybe_ec))

instance ToHsWithIndentLvl (CasesFuncExpr, WhereExpr) where
  to_hs_wil = \(CFE (cps, cs, maybe_ec), we) ->
    to_haskell (CaseOf cps) ++> to_hs_wil we >++<
    deeper (to_hs_wil (cs, maybe_ec))

instance ToHsWithIndentLvl ([Case], Maybe EndCase) where
  to_hs_wil = \(cs, maybe_ec) ->
    foldM (\hs c -> hs ++> to_hs_wil c) "" cs >++< to_hs_wil maybe_ec

instance ToHaskell CaseOf where
  to_haskell (CaseOf cps) =
    run_generator case_of_hs_gen
    where
    case_of_hs_gen :: WithParamNum Haskell
    case_of_hs_gen =
      to_hs_wpn cps >>= \cps_hs ->
      case_of_inner_hs_gen >>= \case_of_inner_hs ->
      return $ "\\" ++ cps_hs ++ " -> case " ++ case_of_inner_hs ++ " of"

    case_of_inner_hs_gen :: WithParamNum Haskell
    case_of_inner_hs_gen =
      get $> \case
        0 -> error "should be impossible: no cases param"
        1 -> "x0'"
        i -> to_params_in_paren_hs i

instance ToHsWithParamNum CasesParams where
  to_hs_wpn = \case
    CParamId id -> return $ to_haskell id
    CasesKeyword -> get_next_param
    Star2 -> return "_"
    CParams (cps, cps_l) ->
      to_hs_wpn_list (cps : cps_l) $> \cps_l_hs ->
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
    Lit2 lit -> to_haskell (NoAnnotation, lit)
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
  to_haskell (LM maybe_ims) =
    "[" ++ maybe_ims_hs ++ "]"
    where
    maybe_ims_hs :: Haskell
    maybe_ims_hs = case maybe_ims of
      Nothing -> ""
      Just (im, im_l) ->
        (im : im_l) &> map (\im -> to_haskell (NoParen, im)) &>
        intercalate ", "

instance ToHsWithIndentLvl CaseBody where
  to_hs_wil = \case
    LFB1 lfb -> return $ " " ++ to_haskell (lfb, NoDCAHs)
    BFB1 (bfb, maybe_we) -> "\n" ++> to_hs_wil maybe_we >++< to_hs_wil bfb

-- Values: ValueDef, GroupedValueDefs, WhereExpr
instance ToHsWithIndentLvl ValueDef where
  to_hs_wil (VD (id, t, ve, maybe_we)) =
    indent <++ (to_haskell id ++ " :: " ++ to_haskell t ++ "\n") >++<
    indent <++ (to_haskell id ++ " =\n") >++<
    deeper (to_hs_wil (ve, maybe_we))

instance ToHsWithIndentLvl (ValueExpr, Maybe WhereExpr) where
  to_hs_wil = \(ve, maybe_we) -> case maybe_we of
    Nothing -> to_hs_wil ve
    Just we -> to_hs_wil (ve, we)

instance ToHsWithIndentLvl ValueExpr where
  to_hs_wil = \case
    BOAE5 boae -> indent <++ to_haskell (boae, NoDCAHs)
    OE2 oe -> to_hs_wil oe
    FE2 fe -> indent >++< to_hs_wil fe
    BT1 bt -> to_hs_wil bt
    BL1 bl -> to_hs_wil bl

instance ToHsWithIndentLvl (ValueExpr, WhereExpr) where
  to_hs_wil = \(ve, we) -> case ve of
    FE2 fe -> to_hs_wil (fe, we)
    _ -> to_hs_wil we >++< to_hs_wil ve

instance ToHsWithIndentLvl GroupedValueDefs where
  to_hs_wil (GVDs (id, ids, ts, les, les_l)) =
    to_hs_wil_list vd_list $> intercalate "\n\n"
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
    indent <++ "let\n" >++< (wdes_gen <++ "\n") >++< indent <++ "in\n"
    where
    wdes_gen :: WithIndentLvl Haskell
    wdes_gen = to_hs_wil_list (wde : wdes) $> intercalate "\n\n"

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
    TIOV1 tiov -> to_haskell tiov
    TA1 ta -> in_paren_if needs_paren $ to_haskell ta
    PoT1 pt -> to_haskell pt
    PT1 pt -> to_haskell pt
    FT1 ft -> in_paren_if needs_paren $ to_haskell ft

instance ToHaskell TypeIdOrVar where
  to_haskell = \case
    TId1 tid -> to_haskell tid
    TV1 tv -> to_haskell tv

instance ToHaskell TypeId where
  to_haskell = \(TId str) -> str

instance ToHaskell TypeVar where
  to_haskell = \case
    PTV1 ptv -> to_haskell ptv
    AHTV1 ahtv -> to_haskell ahtv

instance ToHaskell ParamTVar where
  to_haskell = \(PTV i) -> "a" ++ show i

instance ToHaskell AdHocTVar where
  to_haskell = \(AHTV c) -> "b" ++ show (ord c - 65)

instance ToHaskell TypeApp where
  to_haskell = \case
    TIWA1 tiwa -> to_haskell tiwa
    TIPTI tipti -> to_haskell tipti
    TITIP (tid_or_tv, tip) -> to_haskell tid_or_tv ++ to_haskell tip

instance ToHaskell TIWATypeApp where
  to_haskell (maybe_tip1, TIWA (tid, tip_str_pairs), maybe_tip2) =
    to_haskell tid ++ tid_cont_hs ++
    to_haskell maybe_tip1 ++ tip_hs ++ to_haskell maybe_tip2
    where
    (tid_cont_hs, tip_hs) = foldl add_tip_str_to_hs_pair ("", "") tip_str_pairs
      :: HsPair

    add_tip_str_to_hs_pair :: HsPair -> TIPSTR -> HsPair
    add_tip_str_to_hs_pair = \hs_pair (tip, str) ->
      add_to_hs_pair ("'" ++ str, to_haskell tip) hs_pair

instance ToHaskell TIPTITypeApp where
  to_haskell = \(tip, tioahtv, maybe_tip) ->
    to_haskell tioahtv ++ to_haskell tip ++ to_haskell maybe_tip

instance ToHaskell TIdOrAdHocTVar where
  to_haskell = \case
    TId2 tid -> to_haskell tid
    AHTV2 ahtv -> to_haskell ahtv

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
    TIOV3 tiov -> to_haskell tiov
    TA3 ta -> to_haskell ta
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
    TIOV2 tiov -> to_haskell tiov
    TA2 ta -> to_haskell ta
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
    "type " ++ to_haskell tn ++ " = " ++ popt_hs ++
    proj_types_hs ++ proj_defs_hs ++ change_types_hs ++ change_defs_hs
    where
    popt_hs :: Haskell
    popt_hs = case popt of
      PT4 pt -> to_haskell pt
      PoT4 pt -> to_haskell pt

    sid_hs_list :: [Haskell]
    sid_hs_list = map to_haskell $ si : sis

    proj_hs_list :: [Haskell]
    proj_hs_list = to_proj_hs_list sid_hs_list

    change_hs_list :: [Haskell]
    change_hs_list = to_change_hs_list sid_hs_list

    hs_of_field_type :: Int -> Haskell
    hs_of_field_type = \i -> case popt of
      PT4 (PT (ft, fts)) -> to_haskell $ (ft : fts) !! i
      PoT4 (PoT (pbt, _)) -> to_haskell pbt

    proj_types_hs :: Haskell
    proj_types_hs = combine_with_ts proj_hs_list proj_types_hs_list

    proj_defs_hs :: Haskell
    proj_defs_hs = combine_with_defs proj_hs_list general_proj_hs_list

    change_types_hs :: Haskell
    change_types_hs = combine_with_ts change_hs_list change_types_hs_list

    change_defs_hs :: Haskell
    change_defs_hs = combine_with_defs change_hs_list general_change_hs_list

    proj_types_hs_list :: [Haskell]
    proj_types_hs_list = map to_proj_type [0..4]

    change_types_hs_list :: [Haskell]
    change_types_hs_list = map to_change_type [0..4]

    to_proj_type :: Int -> Haskell
    to_proj_type = \i -> popt_hs ++ " -> " ++ hs_of_field_type i

    to_change_type :: Int -> Haskell
    to_change_type = \i ->
      hs_of_field_type i ++ " -> " ++ popt_hs ++ " -> " ++ popt_hs

instance ToHaskell TypeName where
  to_haskell (TN (maybe_pvip1, tid, pvip_str_pairs, maybe_pvip2)) =
    to_haskell tid ++ tid_cont_hs ++
    to_haskell maybe_pvip1 ++ pvip_hs2 ++ to_haskell maybe_pvip2
    where
    (tid_cont_hs, pvip_hs2) =
      foldl add_pvip_str_to_hs_pair ("", "") pvip_str_pairs
      :: HsPair

    add_pvip_str_to_hs_pair :: HsPair -> PVIPStr -> HsPair
    add_pvip_str_to_hs_pair = \hs_pair (pvip, str) ->
      add_to_hs_pair ("'" ++ str, to_haskell pvip) hs_pair

instance ToHaskell ParamVarsInParen where
  to_haskell = \(PVIP (ptv, ptvs)) -> to_hs_prepend_list " " $ ptv : ptvs

instance ToHaskell OrTypeDef where
  to_haskell (OTD (tn, id, mst, id_mst_pairs)) =
    "data " ++ to_haskell tn ++ " =\n  " ++
    (map id_mst_to_hs ((id, mst) : id_mst_pairs) &> intercalate " |\n  ")
    where
    id_mst_to_hs :: (SimpleId, Maybe SimpleType) -> Haskell
    id_mst_to_hs = \(id, mst) ->
      "C" ++ to_haskell id ++ case mst of
        Nothing -> ""
        Just st -> " " ++ to_haskell (Paren, st)

instance ToHaskell TypeNickname where
  to_haskell = \(TNN (tn, st)) ->
    "type " ++ to_haskell tn ++ " = " ++ to_haskell (NoParen, st)

-- TypePropDef
instance ToHaskell TypePropDef where
  to_haskell = \case
    APD1 apd -> to_haskell apd
    RPD1 rpd -> to_haskell rpd

instance ToHaskell AtomPropDef where
  to_haskell = \(APD (PNL pn, id, st)) ->
    "class " ++ to_haskell pn ++ " where\n  " ++
    to_haskell id ++ " :: " ++ to_haskell (NoParen, st)

instance ToHaskell RenamingPropDef where
  to_haskell = show
--   to_haskell = \(RPD (pnl, pn, pns)) ->
--     to_haskell pnl ++ "\nequivalent\n  " ++ to_haskell pn ++
--     to_hs_prepend_list ", " pns

instance ToHaskell PropName where
  to_haskell = \case
    NPStart1 np_start -> to_haskell np_start
    AHVIPStart ahvip_start -> to_haskell ahvip_start

instance ToHaskell NPStart1 where
  to_haskell (c, np_ahvip_pairs, maybe_np) =
    [c] ++ (map to_haskell nps &> intercalate "'") ++
    to_hs_maybe_np maybe_np ++ to_haskell ahvips
    where
    (nps, ahvips) = unzip np_ahvip_pairs
      :: ([NamePart], [AdHocVarsInParen])

instance ToHaskell AHVIPStart where
  to_haskell (ahvip_np_pairs, maybe_ahvip) =
    (map to_haskell nps &> intercalate "'") ++ to_haskell ahvips ++
    to_haskell maybe_ahvip
    where
    (ahvips, nps) = unzip ahvip_np_pairs
      :: ([AdHocVarsInParen], [NamePart])

instance ToHaskell AdHocVarsInParen where
  to_haskell = \(AHVIP (ahtv, ahtvs)) -> to_hs_prepend_list " " $ ahtv : ahtvs

instance ToHaskell NamePart where
  to_haskell = \(NP str) -> str

-- TypeTheo
instance ToHaskell TypeTheo where
  to_haskell = \(TT (pnws, maybe_pnws, proof)) ->
    "instance " ++ to_haskell pnws ++ mpnws_to_hs maybe_pnws ++ " where\n  " ++
    to_haskell proof
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
    [c] ++ (map to_haskell nps &> intercalate "'") ++
    to_hs_maybe_np maybe_np ++ to_haskell sips
    where
    (nps, sips) = unzip np_sip_pairs
      :: ([NamePart], [SubsInParen])

instance ToHaskell SIPStart where
  to_haskell (sip_np_pairs, maybe_sip) =
    (map to_haskell nps &> intercalate "'") ++ to_haskell sips ++
    to_haskell maybe_sip
    where
    (sips, nps) = unzip sip_np_pairs
      :: ([SubsInParen], [NamePart])

instance ToHaskell SubsInParen where
  to_haskell = \(SIP (tvs, tvss)) -> to_hs_prepend_list " " $ tvs : tvss

instance ToHaskell TVarSub where
  to_haskell = \case
    TIOV4 tiov -> to_haskell tiov
    TAS1 tas -> to_haskell tas
    PoTS1 pts -> to_haskell pts
    PTS1 pts -> to_haskell pts
    FTS1 fts -> to_haskell fts

instance ToHaskell TypeAppSub where
  to_haskell = \case
    TIWS_TAS tiws_tas -> to_haskell tiws_tas
    SOUIP_TI souip_ti_tas -> to_haskell souip_ti_tas
    TI_SOUIP (tid_or_tv, souip) -> to_haskell tid_or_tv ++ to_haskell souip

instance ToHaskell TIWS_TAS where
  to_haskell (maybe_souip1, TIWS (tid, souip_str_pairs), maybe_souip2) =
    to_haskell tid ++ tid_cont_hs ++
    to_haskell maybe_souip1 ++ souip_hs ++ to_haskell maybe_souip2
    where
    (tid_cont_hs, souip_hs) =
      foldl add_souip_str_to_hs_pair ("", "") souip_str_pairs
      :: HsPair

    add_souip_str_to_hs_pair :: HsPair -> SOUIPSTR -> HsPair
    add_souip_str_to_hs_pair = \hs_pair (souip, str) ->
      add_to_hs_pair ("'" ++ str, to_haskell souip) hs_pair

instance ToHaskell SOUIP_TI_TAS where
  to_haskell = \(souip, tid_or_tv, maybe_souip) ->
    to_haskell tid_or_tv ++ to_haskell souip ++ to_haskell maybe_souip

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
    TIOV5 tid_or_var -> to_haskell tid_or_var
    TAS2 tas -> to_haskell tas
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
    TIOV6 tiov -> to_haskell tiov
    TAS3 tas -> to_haskell tas
    PoTS3 pots -> to_haskell pots
    PTS3 pts -> to_haskell pts
    FTS3 fts -> to_haskell fts

instance ToHaskell Proof where
  to_haskell = \case
    P1 (iooe, le) -> to_haskell iooe ++ " " ++ to_haskell (le, NoDCAHs)
    P2 (iooe, ttve) -> to_haskell iooe ++ to_haskell ttve

instance ToHaskell IdOrOpEq where
  to_haskell (IOOE (id, maybe_op_id)) =
    to_haskell id ++ maybe_op_id_hs ++ " ="
    where
    maybe_op_id_hs :: Haskell
    maybe_op_id_hs = case maybe_op_id of
      Nothing -> ""
      Just (op, id) -> to_haskell op ++ to_haskell id

instance ToHaskell TTValueExpr where
  to_haskell = \case
    LE2 le -> " " ++ to_haskell (le, NoDCAHs)
    VEMWE (ve, maybe_we) ->
      "\n" ++ run_generator (deeper2 (to_hs_wil (ve, maybe_we)))

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

-- For fast vim navigation
-- ASTTypes.hs
-- TypesAndHelpers.hs
-- Test.hs
