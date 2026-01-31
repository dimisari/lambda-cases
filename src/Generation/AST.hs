{-
This file contains instances for every AST type for one of the following
classes:
- GTH.ToHaskell: simply outputs the final haskell code for the AST type
- GTH.ToHsWithParamNum: outputs the final haskell code but also keeps track
  of how many new parameters need to be created in the haskell code
- GTH.ToHsWithIndentLvl: outputs the final haskell code but also keeps track
  of the indentation level of every line in the output
The AST provided to this code as the preprocessed AST.
-}

{-# language LambdaCase, FlexibleInstances #-}

module Generation.AST where

import Prelude (($), (++), (!!), (-), (<$>), (>>=), (+))
import Prelude qualified as P
import Control.Monad.State.Lazy qualified as MS
import Control.Monad qualified as M

import Data.List qualified as L
import Data.Char qualified as CH

import ASTTypes qualified as T
import Helpers ((&>), (<++), (>++<), (>$>), (++>))
import Helpers qualified as H
import ShowInstances qualified as S

import Generation.TypesAndHelpers qualified as GTH
import Preprocessing.Collect qualified as C
import Generation.PrefixesAndHardcoded qualified as GPH

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

instance GTH.ToHaskell P.Char where
  to_haskell = (:[])

instance GTH.ToHaskell (GTH.NeedsAnnotBool, T.Literal) where
  to_haskell = \(needs_annot, lit) -> case lit of
    T.Int i -> GTH.to_hs_needs_annot needs_annot i GPH.integer
    T.R r -> GTH.to_hs_needs_annot needs_annot r GPH.double
    T.Ch c -> P.show c
    T.S s -> P.show s

instance GTH.ToHaskell T.Identifier where
  to_haskell (T.Id (muip1, id_start, id_conts, mdigit, muip2)) =
    GTH.maybe_prefix_args_hs GPH.lower_prefix muip1 ++ GTH.to_haskell id_start ++
    GTH.to_haskell id_conts ++ GTH.to_haskell mdigit ++ GTH.single_quotes_hs muip2

instance GTH.ToHaskell T.SimpleId where
  to_haskell = \(T.SId (id_start, mdigit)) ->
    GTH.to_haskell id_start ++ GTH.to_haskell mdigit

instance GTH.ToHaskell T.IdStart where
  to_haskell = \(T.IS str) -> str

instance GTH.ToHaskell T.IdCont where
  to_haskell = \(T.IC (uip, str)) -> GTH.single_quotes_hs uip ++ str

instance GTH.ToHaskell T.ParenExpr where
  to_haskell = \(T.PE ipe) -> "(" ++ GTH.to_haskell ipe ++ ")"

instance GTH.ToHaskell T.InsideParenExpr where
  to_haskell = \case
    T.LOE1 loe -> GTH.to_haskell loe
    T.LFE1 lfe -> GTH.to_haskell lfe

instance GTH.ToHaskell T.Tuple where
  to_haskell (T.T (leou, leous)) =
    GTH.run_generator $ GTH.add_params_to tuple_hs_gen
    where
    tuple_hs_gen :: GTH.WithParamNum GTH.Haskell
    tuple_hs_gen =
      GTH.to_hs_wpn leou >>= \leou_hs ->
      GTH.to_hs_wpn leous >>= \leous_hs ->
      P.return $ "ft" ++ P.show size ++ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

    size :: P.Int
    size = case leous of
      T.LEOUs (_, l) -> P.length l + 2

instance GTH.ToHsWithParamNum T.LineExprOrUnders where
  to_hs_wpn = \(T.LEOUs (leou, leous)) ->
    GTH.to_hs_wpn_list (leou : leous) >$> L.intercalate ", "

instance GTH.ToHsWithParamNum T.LineExprOrUnder where
  to_hs_wpn = \case
    T.LE1 le -> P.return $ GTH.to_haskell le
    T.Underscore1 -> GTH.generate_next_param

instance GTH.ToHaskell T.LineExpr where
  to_haskell = \case
    T.BOAE1 boae -> GTH.to_haskell boae
    T.LOE2 loe -> GTH.to_haskell loe
    T.LFE2 lfe -> GTH.to_haskell lfe

instance GTH.ToHaskell T.BasicOrAppExpr where
  to_haskell = \case
    T.BE3 be -> GTH.to_haskell be
    T.PrFA1 prfa -> GTH.to_haskell prfa
    T.PoFA1 pofa -> GTH.to_haskell pofa

instance GTH.ToHaskell T.BasicExpr where
  to_haskell = \case
    T.Lit1 lit -> GTH.to_haskell (GTH.Annot, lit)
    T.PFAOI1 pfaoi -> GTH.to_haskell pfaoi
    T.T1 tuple -> GTH.to_haskell tuple
    T.L1 list -> GTH.to_haskell list
    T.SI1 spid -> P.error $ "special id in basic expr:" ++ P.show spid

instance GTH.ToHsWithIndentLvl T.BigTuple where
  to_hs_wil (T.BT (leou, btsplit, leous, leous_l)) =
    GTH.indent_all_and_concat big_tuple_hs_list
    where
    big_tuple_hs_list :: [GTH.Haskell]
    big_tuple_hs_list =
      GTH.run_generator $ GTH.add_params_to_list big_tuple_hs_list_gen

    big_tuple_hs_list_gen :: GTH.WithParamNum [GTH.Haskell]
    big_tuple_hs_list_gen =
      GTH.to_hs_wpn leou >>= \leou_hs ->
      GTH.to_hs_wpn leous >>= \leous_hs ->
      GTH.to_hs_wpn_list leous_l >>= \leous_hs_l ->
      P.return $ ["ft" ++ P.show size] ++
        case btsplit of
          T.NoSplit ->
            ["( " ++ leou_hs ++ ", " ++ leous_hs] ++
            P.map (", " ++) leous_hs_l ++
            [")"]
          T.Split ->
            ["( " ++ leou_hs] ++ P.map (", " ++) (leous_hs : leous_hs_l) ++ [")"]

    size :: P.Int
    size = (leous : leous_l) &> P.map leous_size &> P.sum &> (+ 1)

    leous_size :: T.LineExprOrUnders -> P.Int
    leous_size = \(T.LEOUs (_, l)) -> P.length l + 1

instance GTH.ToHaskell T.List where
  to_haskell (T.L maybe_leous) =
    GTH.run_generator $ GTH.add_params_to $ "[" ++> GTH.to_hs_wpn maybe_leous <++ "]"

instance GTH.ToHsWithIndentLvl T.BigList where
  to_hs_wil (T.BL (leous, leous_l)) =
    GTH.indent_all_and_concat big_list_hs_list
    where
    big_list_hs_list :: [GTH.Haskell]
    big_list_hs_list = GTH.run_generator $ GTH.add_params_to_list big_list_hs_list_gen

    big_list_hs_list_gen :: GTH.WithParamNum [GTH.Haskell]
    big_list_hs_list_gen =
      GTH.to_hs_wpn leous >>= \leous_hs ->
      GTH.to_hs_wpn_list leous_l >>= \leous_hs_l ->
      P.return $ ["[ " ++ leous_hs] ++ P.map (", " ++) leous_hs_l ++ ["]"]

instance GTH.ToHaskell T.ParenFuncAppOrId where
  to_haskell = \case
    (T.PFAOI (margs1, id_start, args_str_pairs, mdigit, margs2)) ->
      GTH.run_generator $ GTH.add_params_to paren_func_app_or_id_hs_gen
      where
      paren_func_app_or_id_hs_gen :: GTH.WithParamNum GTH.Haskell
      paren_func_app_or_id_hs_gen =
        GTH.change_id_hs_if_needed2 total_id_hs ++>
        GTH.to_hs_wpn (GTH.calc_args_list (margs1, margs2) args_str_pairs)

      total_id_hs :: GTH.Haskell
      total_id_hs =
        GTH.maybe_prefix_args_hs GPH.lower_prefix margs1 ++ GTH.to_haskell id_start ++
        GTH.args_strs_hs args_str_pairs ++ GTH.to_haskell mdigit ++
        GTH.single_quotes_hs margs2

instance GTH.ToHsWithParamNum [T.Arguments] where
  to_hs_wpn = \args_l ->
    GTH.to_hs_wpn_list args_l >$> \case
      [] -> ""
      args_hs_list -> "(" ++ L.intercalate ", " args_hs_list ++ ")"

instance GTH.ToHsWithParamNum T.Arguments where
  to_hs_wpn = \(T.As leous) -> GTH.to_hs_wpn leous

-- Values: PreFunc, PostFunc, BasicExpr, Change

instance GTH.ToHaskell T.PreFunc where
  to_haskell = \(T.PF id) ->
    case id of
      T.SId (T.IS "a_value", P.Nothing) -> GPH.just
      T.SId (T.IS "error", P.Nothing) -> GPH.left
      T.SId (T.IS "result", P.Nothing) -> GPH.right
      _ -> GPH.constructor_prefix ++ GTH.to_haskell id

instance GTH.ToHaskell T.PreFuncApp where
  to_haskell = \(T.PrFA (pf, oper)) ->
    GTH.run_generator $
    GTH.add_params_to $ (GTH.to_haskell pf ++ "(") ++> GTH.to_hs_wpn oper <++ ")"

instance GTH.ToHaskell T.PostFunc where
  to_haskell = \case
    T.SId1 sid -> GTH.to_haskell sid
    T.SI2 spid -> GPH.spid_projection_prefix ++ GTH.to_haskell spid

instance GTH.ToHaskell T.SpecialId where
  to_haskell = \case
    T.First -> "1st"
    T.Second -> "2nd"
    T.Third -> "3rd"
    T.Fourth -> "4th"
    T.Fifth -> "5th"

instance GTH.ToHaskell T.PostFuncApp where
  to_haskell (T.PoFA (pfa, pfae)) =
    case pfa of
      T.Underscore2 -> "(\\" ++ GPH.under_pfarg_param ++ " -> " ++ inside_hs ++")"
      _ -> inside_hs
    where
    pfa_hs :: GTH.Haskell
    pfa_hs = GTH.to_haskell pfa

    inside_hs :: GTH.Haskell
    inside_hs = case pfae of
      T.DC1 dc -> GTH.to_haskell (dc, pfa_hs)
      T.PFsMDC (pfs, mdc) -> case mdc of
        P.Nothing -> pfa_pfs_hs
        P.Just dc -> GTH.to_haskell (dc, pfa_pfs_hs)
        where
        pfa_pfs_hs :: GTH.Haskell
        pfa_pfs_hs = pfa_pfs_to_hs $ P.reverse pfs

        pfa_pfs_to_hs :: [T.PostFunc] -> GTH.Haskell
        pfa_pfs_to_hs = \case
          [] -> pfa_hs
          pf : pfs -> GTH.to_haskell pf ++ "(" ++ pfa_pfs_to_hs pfs ++ ")"

instance GTH.ToHaskell T.PostFuncArg where
  to_haskell = \case
    T.PE2 pe -> GTH.to_haskell pe
    T.BE2 be -> GTH.to_haskell be
    T.Underscore2 -> GPH.under_pfarg_param

instance GTH.ToHaskell (T.DotChange, GTH.DotChangeArgHs) where
  to_haskell (T.DC (fc, fcs), dcahs) =
    GTH.run_generator $ GTH.add_params_to (change_hs_gen <++ (" " ++ dcahs))
    where
    change_hs_gen :: GTH.WithParamNum GTH.Haskell
    change_hs_gen =
      GTH.to_hs_wpn_list (fc : fcs) >$> \case
        [] -> P.error "should be impossible"
        [fc_hs] -> fc_hs
        fcs_hs_list -> "(" ++ L.intercalate " .> " fcs_hs_list ++ ")"

instance GTH.ToHsWithParamNum T.FieldChange where
  to_hs_wpn = \(T.FC (f, leou)) ->
    (GTH.to_haskell f ++ "(") ++> GTH.to_hs_wpn leou <++ ")"

instance GTH.ToHaskell T.Field where
  to_haskell = \case
    T.SId2 id -> GPH.change_prefix ++ GTH.to_haskell id
    T.SI3 spid -> GPH.spid_change_prefix ++ GTH.to_haskell spid

-- Values: OpExpr

instance GTH.ToHsWithIndentLvl T.OpExpr where
  to_hs_wil = \case
    T.LOE3 loe -> GTH.indent <++ GTH.to_haskell loe
    T.BOE1 boe -> GTH.to_hs_wil boe

instance GTH.ToHsWithParamNum T.OpExprStart where
  to_hs_wpn (T.OES oper_op_pairs) = GTH.to_hs_wpn_list oper_op_pairs >$> P.concat

instance GTH.ToHsWithParamNum (T.Operand, T.Op) where
  to_hs_wpn = \(oper, op) -> GTH.to_hs_wpn oper <++ GTH.to_haskell op

instance GTH.ToHaskell T.LineOpExpr where
  to_haskell = \(T.LOE (oes, loee)) ->
    GTH.run_generator $ GTH.add_params_to $ GTH.to_hs_wpn oes >++< GTH.to_hs_wpn loee

instance GTH.ToHsWithParamNum T.LineOpExprEnd where
  to_hs_wpn = \case
    T.O1 o -> GTH.to_hs_wpn o
    T.LFE3 lfe -> P.return $ GTH.to_haskell lfe

instance GTH.ToHsWithIndentLvl T.BigOpExpr where
  to_hs_wil = \case
    T.BOEOS1 boeos -> GTH.to_hs_wil boeos
    T.BOEFS1 boefs -> GTH.to_hs_wil boefs

instance GTH.ToHsWithIndentLvl T.BigOpExprOpSplit where
  to_hs_wil (T.BOEOS (osls, maybe_oes, ose)) =
    GTH.indent_all_and_concat boeos_hs_list >++< GTH.to_hs_wil ose
    where
    boeos_hs_list :: [GTH.Haskell]
    boeos_hs_list = GTH.run_generator $ GTH.add_params_to_list boeos_hs_list_gen

    boeos_hs_list_gen :: GTH.WithParamNum [GTH.Haskell]
    boeos_hs_list_gen =
      GTH.to_hs_wpn_list osls >>= \osls_hs ->
      GTH.to_hs_wpn maybe_oes >>= \maybe_oes_hs ->
      GTH.to_hs_wpn ose >>= \ose_hs ->
      P.return $ osls_hs ++ [maybe_oes_hs ++ ose_hs]

instance GTH.ToHsWithParamNum T.OpSplitLine where
  to_hs_wpn = \case
    T.OESMOFCO (oes, mofco) -> GTH.to_hs_wpn oes >++< GTH.to_hs_wpn mofco
    T.OFCO1 ofco -> GTH.to_hs_wpn ofco

instance GTH.ToHsWithParamNum T.OperFCO where
  to_hs_wpn = \(T.OFCO (oper, fco)) ->
    GTH.to_hs_wpn oper <++ (" " ++ GTH.to_haskell fco ++ " ")

instance GTH.ToHsWithParamNum T.OpSplitEnd where
  to_hs_wpn = \case
    T.O2 o -> GTH.to_hs_wpn o
    _ -> P.return ""

instance GTH.ToHsWithIndentLvl T.OpSplitEnd where
  to_hs_wil = \case
    T.FE1 fe -> GTH.to_hs_wil (fe, GTH.NoWhereExpr)
    _ -> P.return ""

instance GTH.ToHsWithIndentLvl T.BigOpExprFuncSplit where
  to_hs_wil (T.BOEFS (oes, bocfe)) =
    GTH.indent_all_and_concat params_and_oes_hs_list >++< GTH.to_hs_wil bocfe
    where
    params_and_oes_hs_list :: [GTH.Haskell]
    params_and_oes_hs_list =
      GTH.run_generator $ GTH.add_params_to_list $ GTH.to_hs_wpn_list [oes]

instance GTH.ToHsWithIndentLvl T.BigOrCasesFuncExpr where
  to_hs_wil = \case
    T.BFE1 bfe -> GTH.to_hs_wil (bfe, GTH.NoWhereExpr)
    T.CFE1 cfe -> GTH.to_hs_wil (cfe, GTH.NoWhereExpr)

instance GTH.ToHsWithParamNum T.Operand where
  to_hs_wpn = \case
    T.BOAE2 boae -> P.return $ GTH.to_haskell boae
    T.PE3 pe -> P.return $ GTH.to_haskell pe
    T.Underscore3 -> GTH.generate_next_param

instance GTH.ToHaskell T.Op where
  to_haskell = \case
    T.FCO3 fco -> " " ++ GTH.to_haskell fco ++ " "
    T.OSO oso -> " " ++ GTH.to_haskell oso ++ " "

instance GTH.ToHaskell T.FuncCompOp where
  to_haskell = \case
    T.RightComp -> ".>"
    T.LeftComp -> "<."

instance GTH.ToHaskell T.OptionalSpacesOp where
  to_haskell = \case
    T.RightApp -> "&>"
    T.LeftApp -> "<&"
    T.Power -> "!^"
    T.Mult -> "!*"
    T.Div -> "!/"
    T.Plus -> "!+"
    T.Minus -> "!-"
    T.Equal -> "!=="
    T.NotEqual -> "!!="
    T.Greater -> "!>"
    T.Less -> "!<"
    T.GrEq -> "!>="
    T.LeEq -> "!<="
    T.And -> "!&"
    T.Or -> "!|"
    T.Use -> "!>>="
    T.Then -> "!>>"

-- Values: FuncExpr

instance GTH.ToHsWithIndentLvl (T.FuncExpr, GTH.PossiblyWhereExpr) where
  to_hs_wil = \(fe, pwe) -> case fe of
    T.LFE4 lfe -> GTH.to_hs_wil (lfe, pwe)
    T.BFE2 bfe -> GTH.to_hs_wil (bfe, pwe)
    T.CFE2 cfe -> GTH.to_hs_wil (cfe, pwe)

instance GTH.ToHaskell T.LineFuncExpr where
  to_haskell = \(T.LFE (params, lfb)) ->
    GTH.to_haskell (GTH.Whole params) ++ " " ++ GTH.to_haskell lfb

instance GTH.ToHsWithIndentLvl (T.LineFuncExpr, GTH.PossiblyWhereExpr) where
  to_hs_wil = \(lfe@(T.LFE (params, lfb)), pwe) ->
    case pwe of
      GTH.NoWhereExpr -> P.return $ GTH.to_haskell lfe
      GTH.HasWhereExpr we ->
        (GTH.to_haskell (GTH.Whole params) ++ "\n") ++> GTH.to_hs_wil we >++<
        GTH.indent <++ GTH.to_haskell lfb

instance GTH.ToHsWithIndentLvl (T.BigFuncExpr, GTH.PossiblyWhereExpr) where
  to_hs_wil (T.BFE (params, bfb), pwe) =
    params_hs ++> GTH.to_hs_wil pwe >++< GTH.to_hs_wil bfb
    where
    params_hs :: GTH.Haskell
    params_hs = GTH.to_haskell (GTH.Whole params) ++ "\n"

instance GTH.ToHaskell GTH.WholeParams where
  to_haskell = \(GTH.Whole params) -> "\\" ++ GTH.to_haskell params ++ " ->"

instance GTH.ToHaskell T.Parameters where
  to_haskell = \case
    T.ParamId id -> GTH.to_haskell id
    T.Star1 -> "_"
    T.Params (params, params_l) ->
      "(" ++ GTH.to_haskell params ++ GTH.to_hs_prepend_list ", " params_l ++ ")"

instance GTH.ToHaskell T.LineFuncBody where
  to_haskell = \case
    T.BOAE3 boae -> GTH.to_haskell boae
    T.LOE4 loe -> GTH.to_haskell loe
    T.LFE5 lfe -> "(" ++ GTH.to_haskell lfe ++ ")"

instance GTH.ToHsWithIndentLvl T.BigFuncBody where
  to_hs_wil = \case
    T.BOAE4 boae -> GTH.indent <++ GTH.to_haskell boae
    T.OE1 oe -> GTH.to_hs_wil oe
    T.LFE6 lfe -> GTH.indent <++ ("(" ++ GTH.to_haskell lfe ++ ")")

instance GTH.ToHsWithIndentLvl (T.CasesFuncExpr, GTH.PossiblyWhereExpr) where
  to_hs_wil (T.CFE (cps, cs, maybe_ec), pwe) =
    params_hs ++> GTH.to_hs_wil pwe >++<
    GTH.indent <++ case_of_hs >++<
    GTH.deeper (GTH.to_hs_wil (cs, maybe_ec))
    where
    (params_hs, case_of_hs) = GTH.run_generator params_and_case_of_hs_gen
      :: (GTH.Haskell, GTH.Haskell)

    params_and_case_of_hs_gen :: GTH.WithParamNum (GTH.Haskell, GTH.Haskell)
    params_and_case_of_hs_gen =
      GTH.to_hs_wpn cps >>= \cps_hs ->
      GTH.case_of_inner_hs_gen >>= \case_of_inner_hs ->
      P.return $
        ("\\" ++ cps_hs ++ " ->\n", "case " ++ case_of_inner_hs ++ " of")

instance GTH.ToHsWithIndentLvl ([T.Case], P.Maybe T.EndCase) where
  to_hs_wil = \(cs, maybe_ec) ->
    GTH.to_hs_wil_list cs >$> P.concat >++< GTH.to_hs_wil maybe_ec

instance GTH.ToHsWithParamNum T.CasesParams where
  to_hs_wpn = \case
    T.CParamId id -> P.return $ GTH.to_haskell id
    T.QuestionMark -> GTH.generate_next_param
    T.Star2 -> P.return "_"
    T.CParams (cps, cps_l) ->
      GTH.to_hs_wpn_list (cps : cps_l) >$> \cps_l_hs ->
      "(" ++ L.intercalate ", " cps_l_hs ++ ")"

instance GTH.ToHsWithIndentLvl T.Case where
  to_hs_wil = \(T.Ca (om, cb)) ->
    "\n" ++> GTH.indent <++ (GTH.to_haskell om ++ " ->") >++<
    GTH.deeper (GTH.to_hs_wil cb)

instance GTH.ToHsWithIndentLvl T.EndCase where
  to_hs_wil = \(T.EC (ecp, cb)) ->
    "\n" ++> GTH.indent <++ (GTH.to_haskell ecp ++ " ->") >++<
    GTH.deeper (GTH.to_hs_wil cb)

instance GTH.ToHaskell T.OuterMatching where
  to_haskell = \case
    T.SId3 sid -> GTH.to_haskell sid
    T.M1 m -> GTH.to_haskell (GTH.NoParen, m)

instance GTH.ToHaskell T.EndCaseParam where
  to_haskell = \case
    T.Id1 id -> GTH.to_haskell id
    T.Ellipsis -> "_"

instance GTH.ToHaskell (GTH.NeedsParenBool, T.Matching) where
  to_haskell = \(needs_paren, m) -> case m of
    T.Lit2 lit -> GTH.to_haskell (GTH.NoAnnot, lit)
    T.PFM (pf, im) ->
      GTH.in_paren_if needs_paren $
        GTH.to_haskell pf ++ " " ++ GTH.to_haskell (GTH.Paren, im)
    T.TM1 tm -> GTH.to_haskell tm
    T.LM1 lm -> GTH.to_haskell lm

instance GTH.ToHaskell (GTH.NeedsParenBool, T.InnerMatching) where
  to_haskell = \(needs_paren, im) -> case im of
    T.Star -> "_"
    T.Id2 id -> GTH.to_haskell id
    T.M2 m -> GTH.to_haskell (needs_paren, m)

instance GTH.ToHaskell T.TupleMatching where
  to_haskell (T.TM (im, im_l)) =
    "(" ++ ims_hs ++ ")"
    where
    ims_hs :: GTH.Haskell
    ims_hs =
      (im : im_l) &> P.map (\im -> GTH.to_haskell (GTH.NoParen, im)) &>
      L.intercalate ", "

instance GTH.ToHaskell T.ListMatching where
  to_haskell (T.LM m_list_internals) =
    case m_list_internals of
      P.Nothing -> "[]"
      P.Just (im, im_l, P.Nothing) -> "[" ++ commas_ims_hs (im, im_l) ++ "]"
      P.Just (im, im_l, P.Just rlm) ->
        colons_ims_hs (im, im_l) ++ GTH.to_haskell rlm
    where
    commas_ims_hs :: (T.InnerMatching, [T.InnerMatching]) -> GTH.Haskell
    commas_ims_hs = \(im, im_l) ->
      (im : im_l) &> P.map (\im -> GTH.to_haskell (GTH.NoParen, im)) &>
      L.intercalate ", "

    colons_ims_hs :: (T.InnerMatching, [T.InnerMatching]) -> GTH.Haskell
    colons_ims_hs = \(im, im_l) ->
      (im : im_l) &> P.map (\im -> GTH.to_haskell (GTH.Paren, im)) &>
      P.concatMap (++ " : ")


instance GTH.ToHaskell T.RestListMatching where
  to_haskell = \(T.RLM msid) ->
    case msid of
      P.Nothing -> "_"
      P.Just sid -> GTH.to_haskell sid

instance GTH.ToHsWithIndentLvl T.CaseBody where
  to_hs_wil = \case
    T.LFB1 lfb -> P.return $ " " ++ GTH.to_haskell lfb
    T.BFB1 (bfb, maybe_we) ->
      "\n" ++> GTH.to_hs_wil maybe_we >++< GTH.to_hs_wil bfb

-- Values: ValueDef, GroupedValueDefs, WhereExpr

instance GTH.ToHsWithIndentLvl T.ValueDef where
  to_hs_wil (T.VD (id, t, ve, maybe_we)) =
    GTH.indent <++ (GTH.to_haskell id ++ " :: ") >++<
    (forall_hs <$> MS.get) <++ (GTH.to_haskell t ++ "\n") >++<
    GTH.indent <++ (GTH.to_haskell id ++ " =\n") >++<
    GTH.deeper (GTH.to_hs_wil (ve, GTH.mwe_to_pwe maybe_we))
    where
    forall_hs :: P.Int -> GTH.Haskell
    forall_hs = \case
      0 ->
        case P.concatMap (" " ++) param_t_vars_hs_list of
          "" -> ""
          hs -> "forall" ++ hs ++ ". "
      _ -> ""

    param_t_vars_hs_list :: [GTH.Haskell]
    param_t_vars_hs_list = C.param_t_vars t &> P.map GTH.to_haskell

instance GTH.ToHsWithIndentLvl (T.ValueExpr, GTH.PossiblyWhereExpr) where
  to_hs_wil = \(ve, pwe) -> case ve of
    T.FE2 fe -> GTH.indent >++< GTH.to_hs_wil (fe, pwe)
    _ ->
      GTH.to_hs_wil pwe >++< case ve of
        T.BOAE5 boae -> GTH.indent <++ GTH.to_haskell boae
        T.OE2 oe -> GTH.to_hs_wil oe
        T.BT1 bt -> GTH.to_hs_wil bt
        T.BL1 bl -> GTH.to_hs_wil bl

instance GTH.ToHsWithIndentLvl T.GroupedValueDefs where
  to_hs_wil (T.GVDs (id, ids, ts, les, les_l)) =
    GTH.to_hs_wil_list vd_list >$> L.intercalate "\n\n"
    where
    vd_list :: [T.ValueDef]
    vd_list = to_val_def_list (total_ids, t_list, total_le_list)

    total_ids :: [T.Identifier]
    total_ids = id : ids

    t_list :: [T.Type]
    t_list = case ts of
      T.Ts (t, ts) -> t : ts
      T.All t -> L.replicate (P.length total_ids) t

    total_le_list :: [T.LineExpr]
    total_le_list = P.concatMap (\(T.LEs (le, le_l)) -> le : le_l) (les : les_l)

    to_val_def_list :: ([T.Identifier], [T.Type], [T.LineExpr]) -> [T.ValueDef]
    to_val_def_list = \case
      ([], [], []) -> []
      (id : ids, t : ts, le : les) ->
        T.VD (id, t, le_to_ve le, P.Nothing) : to_val_def_list (ids, ts, les)
      _ ->
        P.error $
          "identifiers, types and expressions don't match in number " ++
          "in grouped value definitions"

    le_to_ve :: T.LineExpr -> T.ValueExpr
    le_to_ve = \case
      T.BOAE1 boae -> T.BOAE5 boae
      T.LOE2 loe -> T.OE2 $ T.LOE3 loe
      T.LFE2 lfe -> T.FE2 $ T.LFE4 lfe

instance GTH.ToHsWithIndentLvl T.WhereExpr where
  to_hs_wil (T.WE (wde, wdes)) =
    GTH.indent <++ "let\n" >++<
    (GTH.to_hs_wil_list (wde : wdes) >$> L.intercalate "\n\n") <++ "\n" >++<
    GTH.indent <++ "in\n"

instance GTH.ToHsWithIndentLvl T.WhereDefExpr where
  to_hs_wil = \case
    T.VD1 vd -> GTH.to_hs_wil vd
    T.GVDs1 gvd -> GTH.to_hs_wil gvd

-- Type

instance GTH.ToHaskell T.Type where
  to_haskell = \(T.Ty (maybe_c, st)) ->
    GTH.to_haskell maybe_c ++ GTH.to_haskell (GTH.NoParen, st)

instance GTH.ToHaskell (GTH.NeedsParenBool, T.SimpleType) where
  to_haskell = \(needs_paren, st) -> case st of
    T.PTV1 ptv -> GTH.to_haskell ptv
    T.TAIOA1 taioa -> GTH.to_haskell (needs_paren, taioa)
    T.PoT1 pt -> GTH.to_haskell pt
    T.PT1 pt -> GTH.to_haskell pt
    T.FT1 ft -> GTH.in_paren_if needs_paren $ GTH.to_haskell ft

instance GTH.ToHaskell T.TypeId where
  to_haskell = \(T.TId str) -> str

instance GTH.ToHaskell T.ParamTVar where
  to_haskell = \(T.PTV i) -> GPH.param_t_var_prefix ++ P.show i

instance GTH.ToHaskell T.AdHocTVar where
  to_haskell = \(T.AHTV c) -> GPH.ad_hoc_t_var_prefix ++ P.show (CH.ord c - 65)

instance GTH.ToHaskell (GTH.NeedsParenBool, T.TypeAppIdOrAHTV) where
  to_haskell (needs_paren, T.TAIOA taioa) = case taioa of
    (P.Nothing, T.TIdStart1 (T.TId tid, []), P.Nothing) ->
      tid &> \case
        "Bool" -> GPH.bool
        "Int" -> GPH.integer
        "Real" -> GPH.double
        "Char" -> GPH.char
        "String" -> GPH.string
        _ -> GTH.to_haskell tid

    (mtip1, taioam, mtip2) -> case taioam of
      T.AHTV2 ahtv ->
        in_paren_if_needed (GTH.to_haskell ahtv) tip_hs
        where
        tip_hs :: GTH.Haskell
        tip_hs = GTH.to_haskell mtip1 ++ GTH.to_haskell mtip2

      T.TIdStart1 (tid, tip_str_pairs) ->
        in_paren_if_needed tid_hs tip_hs
        where
        tid_hs :: GTH.Haskell
        tid_hs =
          GTH.maybe_prefix_args_hs GPH.upper_prefix mtip1 ++ GTH.to_haskell tid ++
          GTH.args_strs_hs tip_str_pairs ++ GTH.single_quotes_hs mtip2

        tip_hs :: GTH.Haskell
        tip_hs =
          GTH.to_haskell mtip1 ++ GTH.to_haskell (P.map P.fst tip_str_pairs) ++
          GTH.to_haskell mtip2
      where
      in_paren_if_needed :: GTH.Haskell -> GTH.Haskell -> GTH.Haskell
      in_paren_if_needed = \hs tip_hs ->
        case tip_hs of
          "" -> hs
          _ -> GTH.in_paren_if needs_paren $ hs ++ tip_hs

instance GTH.ToHaskell T.TypesInParen where
  to_haskell = \(T.TIP (st, sts)) ->
    GTH.to_hs_prepend_list " " $ P.map (\st -> (GTH.Paren, st)) $ st : sts

instance GTH.ToHaskell T.ProdType where
  to_haskell = \(T.PT (ft, fts)) ->
    "(" ++ ((ft : fts) &> P.map GTH.to_haskell &> L.intercalate ", ") ++ ")"

instance GTH.ToHaskell T.FieldType where
  to_haskell = \case
    T.PBT1 ft -> GTH.to_haskell ft
    T.PoT2 pt -> GTH.to_haskell pt

instance GTH.ToHaskell T.PowerBaseType where
  to_haskell = \case
    T.PTV2 ptv -> GTH.to_haskell ptv
    T.TAIOA2 taioa -> GTH.to_haskell (GTH.NoParen, taioa)
    T.IPT ipt -> GTH.to_haskell ipt

instance GTH.ToHaskell T.InParenT where
  to_haskell = \case
    T.PT3 pt -> GTH.to_haskell pt
    T.PoT3 pt -> GTH.to_haskell pt
    T.FT3 ft -> GTH.to_haskell ft

instance GTH.ToHaskell T.PowerType where
  to_haskell = \(T.PoT (ft, i)) ->
    "(" ++
    (L.replicate (P.fromIntegral i) ft &> P.map GTH.to_haskell &> L.intercalate ", ") ++
    ")"

instance GTH.ToHaskell T.FuncType where
  to_haskell = \(T.FT (it, ot)) ->
    GTH.to_haskell it ++ " -> " ++ GTH.to_haskell ot

instance GTH.ToHaskell T.InOrOutType where
  to_haskell = \case
    T.PTV3 ptv -> GTH.to_haskell ptv
    T.TAIOA3 taioa -> GTH.to_haskell (GTH.NoParen, taioa)
    T.PoT4 pt -> GTH.to_haskell pt
    T.PT2 pt -> GTH.to_haskell pt
    T.FT2 ft -> "(" ++ GTH.to_haskell ft ++ ")"

instance GTH.ToHaskell T.Condition where
  to_haskell = \(T.Co pn) -> GTH.to_haskell pn ++ " => "

-- TypeDef, TypeNickname

instance GTH.ToHaskell T.TypeDef where
  to_haskell = \case
    T.TTD1 ttd -> GTH.to_haskell ttd
    T.OTD1 otd -> GTH.to_haskell otd

instance GTH.ToHaskell T.TupleTypeDef where
  to_haskell (T.TTD (tn, popt, T.PCSIs (si, sis))) =
    data_hs ++ "\n\n" ++ instance_hs ++ "\n" ++
    change_types_hs ++ change_defs_hs
    where
    data_hs :: GTH.Haskell
    data_hs =
      "data " ++ tn_hs ++ " =\n  " ++ cons_hs ++
      " { " ++ projections_and_types_hs ++ " }"

    instance_hs :: GTH.Haskell
    instance_hs =
      "instance FromTuple" ++ size_hs ++ types_hs ++ " " ++
      GTH.to_haskell (GTH.Paren, tn) ++ " where\n" ++
      "  ft" ++ size_hs ++
      " = \\(" ++ L.intercalate ", " params_list ++ ") -> " ++
      cons_hs ++ P.concatMap (" " ++) params_list

    change_types_hs :: GTH.Haskell
    change_types_hs = GTH.combine_with_ts change_hs_list change_types_hs_list

    change_defs_hs :: GTH.Haskell
    change_defs_hs = GTH.combine_with_defs change_hs_list sid_hs_list

    sid_hs_list :: [GTH.Haskell]
    sid_hs_list = P.map GTH.to_haskell $ si : sis

    change_hs_list :: [GTH.Haskell]
    change_hs_list = P.map (GPH.change_prefix ++) sid_hs_list

    change_types_hs_list :: [GTH.Haskell]
    change_types_hs_list = P.map to_change_type [0..4]

    to_change_type :: P.Int -> GTH.Haskell
    to_change_type = \i ->
      (types_hs_list !! i) ++ " -> " ++ tn_hs  ++ " -> " ++ tn_hs

    tn_hs :: GTH.Haskell
    tn_hs = GTH.to_haskell (GTH.NoParen, tn)

    types_list :: [T.SimpleType]
    types_list = case popt of
      T.PT4 (T.PT (ft, fts)) -> P.map GTH.ft_to_st $ ft : fts
      T.PoT5 (T.PoT (pbt, i)) -> L.replicate (P.fromIntegral i) $ GTH.pbt_to_st pbt

    types_hs_list :: [GTH.Haskell]
    types_hs_list = P.map (\st -> GTH.to_haskell (GTH.NoParen, st)) types_list

    types_hs :: GTH.Haskell
    types_hs = P.concatMap (\st -> " " ++ GTH.to_haskell (GTH.Paren, st)) types_list

    size :: P.Int
    size = P.length types_hs_list

    size_hs :: GTH.Haskell
    size_hs = P.show size

    projections_and_types_hs :: GTH.Haskell
    projections_and_types_hs =
      P.zipWith (\a b -> a ++ " :: " ++ b) sid_hs_list types_hs_list &>
      L.intercalate ", "

    cons_hs :: GTH.Haskell
    cons_hs = GTH.tn_to_cons_hs tn

    params_list :: [GTH.Haskell]
    params_list = [1..size] &> P.map (\i -> "x" ++ P.show i)

instance GTH.ToHaskell T.ProdOrPowerType where
  to_haskell = \case
    T.PT4 pt -> GTH.to_haskell pt
    T.PoT5 pt -> GTH.to_haskell pt

instance GTH.ToHaskell (GTH.NeedsParenBool, T.TypeName) where
  to_haskell (needs_paren, tn@(T.TN (mpvip1, _, pvip_str_pairs, mpvip2))) =
    case pvips_hs of
      "" -> GTH.tn_to_tid_hs tn
      _ -> GTH.in_paren_if needs_paren $ GTH.tn_to_tid_hs tn ++ pvips_hs
    where
    pvips_hs :: GTH.Haskell
    pvips_hs =
      GTH.to_haskell mpvip1 ++ GTH.to_haskell pvips ++ GTH.to_haskell mpvip2

    pvips :: [T.ParamVarsInParen]
    pvips = P.map P.fst pvip_str_pairs

instance GTH.ToHaskell T.ParamVarsInParen where
  to_haskell = \(T.PVIP (ptv, ptvs)) -> GTH.to_hs_prepend_list " " $ ptv : ptvs

instance GTH.ToHaskell T.OrTypeDef where
  to_haskell (T.OTD (tn, pv, pvs)) =
    "data " ++ GTH.to_haskell (GTH.NoParen, tn) ++ " =\n  " ++
    (P.map GTH.to_haskell (pv : pvs) &> L.intercalate " |\n  ")

instance GTH.ToHaskell T.PossibleValue where
  to_haskell = \(T.PV (sid, maybe_with_val)) ->
    GPH.constructor_prefix ++ GTH.to_haskell sid ++ case maybe_with_val of
      P.Nothing -> ""
      P.Just (id, st) -> " " ++ GTH.to_haskell (GTH.Paren, st)

instance GTH.ToHaskell T.TypeNickname where
  to_haskell = \(T.TNN (tn, st)) ->
    "type " ++ GTH.to_haskell (GTH.NoParen, tn) ++ " = " ++
    GTH.to_haskell (GTH.NoParen, st)

-- TypePropDef

instance GTH.ToHaskell T.TypePropDef where
  to_haskell = \case
    T.APD1 apd -> GTH.to_haskell apd
    T.RPD1 rpd -> GTH.to_haskell rpd

instance GTH.ToHaskell T.AtomPropDef where
  to_haskell = \(T.APD (pnl, id, st)) ->
    "class " ++ GTH.to_haskell pnl ++ " where\n  " ++
    GTH.to_haskell id ++ " :: " ++ GTH.to_haskell (GTH.NoParen, st)

instance GTH.ToHaskell T.RenamingPropDef where
  to_haskell = \_ -> ""

instance GTH.ToHaskell T.PropNameLine where
  to_haskell = \(T.PNL pn) -> GTH.to_haskell pn

instance GTH.ToHaskell T.PropName where
  to_haskell = \case
    T.NPStart1 np_start -> GTH.to_haskell np_start
    T.TIPStart tip_start -> GTH.to_haskell tip_start

instance GTH.ToHaskell T.NPStart1 where
  to_haskell (c, np_tip_pairs, maybe_np) =
    [c] ++ GTH.nps_args_hs np_tip_pairs ++ GTH.to_hs_maybe_np maybe_np ++
    GTH.to_haskell tips
    where
    tips :: [T.TypesInParen]
    tips = P.map P.snd np_tip_pairs

instance GTH.ToHaskell T.TIPStart where
  to_haskell (tip_np_pairs, maybe_tip) =
    GTH.args_nps_hs tip_np_pairs ++ GTH.single_quotes_hs maybe_tip ++
    GTH.to_haskell tips ++ GTH.to_haskell maybe_tip
    where
    tips :: [T.TypesInParen]
    tips = P.map P.fst tip_np_pairs

instance GTH.ToHaskell T.NamePart where
  to_haskell = \(T.NP str) -> str

-- TypeTheo

instance GTH.ToHaskell T.TypeTheo where
  to_haskell (T.TT (pnws_l, maybe_pnws, proof)) =
    "instance " ++ pnws_l_hs ++
    mpnws_to_hs maybe_pnws ++ " where\n  " ++ GTH.to_haskell proof
    where
    pnws_l_hs :: GTH.Haskell
    pnws_l_hs = case pnws_l of
      [] -> P.error "to_haskell type_theo: no pnws before arrow"
      [pnws] -> GTH.to_haskell pnws
      _ -> P.map GTH.to_haskell pnws_l &> L.intercalate ", " &> \x -> "(" ++ x ++ ")"

    mpnws_to_hs :: P.Maybe T.PropNameWithSubs -> P.String
    mpnws_to_hs = \case
      P.Nothing -> ""
      P.Just pnws -> " => " ++ GTH.to_haskell pnws

instance GTH.ToHaskell T.PropNameWithSubs where
  to_haskell = \case
    T.NPStart2 np_start -> GTH.to_haskell np_start
    T.SIPStart sip_start -> GTH.to_haskell sip_start

instance GTH.ToHaskell T.NPStart2 where
  to_haskell (c, np_sip_pairs, maybe_np) =
    GTH.change_prop_hs_if_needed prop_hs ++ GTH.to_haskell sips
    where
    prop_hs :: GTH.Haskell
    prop_hs = [c] ++ GTH.nps_args_hs np_sip_pairs ++ GTH.to_hs_maybe_np maybe_np

    sips :: [T.SubsInParen]
    sips = P.map P.snd np_sip_pairs

instance GTH.ToHaskell T.SIPStart where
  to_haskell (sip_np_pairs, maybe_sip) =
    GTH.change_prop_hs_if_needed prop_hs ++ GTH.single_quotes_hs maybe_sip ++
    GTH.to_haskell sips ++ GTH.to_haskell maybe_sip
    where
    prop_hs :: GTH.Haskell
    prop_hs = GTH.args_nps_hs sip_np_pairs

    sips :: [T.SubsInParen]
    sips = P.map P.fst sip_np_pairs

instance GTH.ToHaskell T.SubsInParen where
  to_haskell = \(T.SIP (tvs, tvss)) -> GTH.to_hs_prepend_list " " $ tvs : tvss

instance GTH.ToHaskell T.TVarSub where
  to_haskell = \case
    T.PTV4 tv -> GTH.to_haskell tv
    T.TAIOAS1 tasoi -> GTH.to_haskell (GTH.Paren, tasoi)
    T.PoTS1 pts -> GTH.to_haskell pts
    T.PTS1 pts -> GTH.to_haskell pts
    T.FTS1 fts -> "(" ++ GTH.to_haskell fts  ++ ")"

instance GTH.ToHaskell (GTH.NeedsParenBool, T.TypeAppIdOrAHTVSub) where
  to_haskell (needs_paren, T.TAIOAS taioas) = case taioas of
    (P.Nothing, T.TIdStart2 (T.TId "Real", []), P.Nothing) -> "Double"
    (P.Nothing, T.TIdStart2 (T.TId "Int", []), P.Nothing) -> "Integer"
    (msouip1, taioasm, msouip2) -> case taioasm of
      T.AHTV3 ahtv ->
        in_paren_if_needed (GTH.to_haskell ahtv) souip_hs
        where
        souip_hs :: GTH.Haskell
        souip_hs = GTH.to_haskell msouip1 ++ GTH.to_haskell msouip2

      T.TIdStart2 (tid, souip_str_pairs) ->
        in_paren_if_needed tid_hs souip_hs
        where
        tid_hs :: GTH.Haskell
        tid_hs =
          GTH.maybe_prefix_args_hs GPH.upper_prefix msouip1 ++ GTH.to_haskell tid ++
          GTH.args_strs_hs souip_str_pairs ++ GTH.single_quotes_hs msouip2

        souip_hs :: GTH.Haskell
        souip_hs =
          GTH.to_haskell msouip1 ++ GTH.to_haskell (P.map P.fst souip_str_pairs) ++
          GTH.to_haskell msouip2
      where
      in_paren_if_needed :: GTH.Haskell -> GTH.Haskell -> GTH.Haskell
      in_paren_if_needed = \hs souip_hs ->
        case souip_hs of
          "" -> hs
          _ -> GTH.in_paren_if needs_paren $ hs ++ souip_hs

instance GTH.ToHaskell T.SubsOrUndersInParen where
  to_haskell = \(T.SOUIP (sou, sous)) -> GTH.to_haskell $ sou : sous

instance GTH.ToHaskell T.SubOrUnder where
  to_haskell = \case
    T.TVS1 tvs -> " " ++ GTH.to_haskell tvs
    T.Underscore4 -> ""

instance GTH.ToHaskell T.PowerTypeSub where
  to_haskell = \(T.PoTS (pbts, i)) ->
    "(" ++
    (L.replicate (P.fromIntegral i) pbts &> P.map GTH.to_haskell &> L.intercalate ", ")
    ++
    ")"

instance GTH.ToHaskell T.PowerBaseTypeSub where
  to_haskell = \case
    T.Underscore5 -> P.undefined
    T.PTV5 tv -> GTH.to_haskell tv
    T.TAIOAS2 tasoi -> GTH.to_haskell (GTH.NoParen, tasoi)
    T.IPTS ipts -> GTH.to_haskell ipts

instance GTH.ToHaskell T.InParenTSub where
  to_haskell = \case
    T.PTS2 pts -> "(" ++ GTH.to_haskell pts ++ ")"
    T.FTS2 fts -> "(" ++ GTH.to_haskell fts ++ ")"

instance GTH.ToHaskell T.ProdTypeSub where
  to_haskell = \(T.PTS (fts, fts_l)) ->
    "(" ++ ((fts : fts_l) &> P.map GTH.to_haskell &> L.intercalate ", ") ++ ")"

instance GTH.ToHaskell T.FieldTypeSub where
  to_haskell = \case
    T.PBTS1 pbts -> GTH.to_haskell pbts
    T.PoTS2 pots -> GTH.to_haskell pots

instance GTH.ToHaskell T.FuncTypeSub where
  to_haskell = \(T.FTS (in_ts, out_ts)) ->
    GTH.to_haskell in_ts ++ " -> " ++ GTH.to_haskell out_ts

instance GTH.ToHaskell T.InOrOutTypeSub where
  to_haskell = \case
    T.Underscore6 -> P.undefined
    T.PTV6 tv -> GTH.to_haskell tv
    T.TAIOAS3 tasoi -> GTH.to_haskell (GTH.NoParen, tasoi)
    T.PoTS3 pots -> GTH.to_haskell pots
    T.PTS3 pts -> GTH.to_haskell pts
    T.FTS3 fts -> GTH.to_haskell fts

instance GTH.ToHaskell T.Proof where
  to_haskell = \case
    T.P1 (iooe, le) -> GTH.to_haskell iooe ++ " " ++ GTH.to_haskell le
    T.P2 (iooe, ttve) -> GTH.to_haskell iooe ++ GTH.to_haskell ttve

instance GTH.ToHaskell T.IdOrOpEq where
  to_haskell (T.IOOE (id, maybe_op_id)) =
    GTH.change_id_hs_if_needed1 (GTH.to_haskell id) ++ maybe_op_id_hs ++ " ="
    where
    maybe_op_id_hs :: GTH.Haskell
    maybe_op_id_hs = case maybe_op_id of
      P.Nothing -> ""
      P.Just (op, id) -> GTH.to_haskell op ++ GTH.to_haskell id

instance GTH.ToHaskell T.TTValueExpr where
  to_haskell = \case
    T.LE2 le -> " " ++ GTH.to_haskell le
    T.VEMWE (ve, maybe_we) ->
      "\n" ++
      GTH.run_generator
        (GTH.twice_deeper (GTH.to_hs_wil (ve, GTH.mwe_to_pwe maybe_we)))

-- Program

instance GTH.ToHaskell T.Program where
  to_haskell = \(T.P (pp, pps)) ->
    GTH.to_haskell pp ++ GTH.to_hs_prepend_list "\n\n" pps

instance GTH.ToHaskell T.ProgramPart where
  to_haskell = \case
    T.VD2 vd -> GTH.run_generator $ GTH.to_hs_wil vd
    T.GVDs2 gvds -> GTH.run_generator $ GTH.to_hs_wil gvds
    T.TD td -> GTH.to_haskell td
    T.TNN1 tnn -> GTH.to_haskell tnn
    T.TPD tpd -> GTH.to_haskell tpd
    T.TT1 tt -> GTH.to_haskell tt

-- Helper instances

instance GTH.ToHsWithIndentLvl GTH.PossiblyWhereExpr where
  to_hs_wil = \case
    GTH.NoWhereExpr -> P.return ""
    GTH.HasWhereExpr we -> GTH.to_hs_wil we

{-
For fast vim file navigation:
TypesAndHelpers.hs
Preprocess.hs
-}
