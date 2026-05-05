{-
This file contains instances for every AST type for one of the following
classes:
- GH.ToHaskell: simply outputs the final haskell code for the AST type
- GH.ToHsWithParamNum: outputs the final haskell code but also keeps track
  of how many new parameters need to be created in the haskell code
- GH.ToHsWithIndentLvl: outputs the final haskell code but also keeps track
  of the indentation level of every line in the output
The AST provided to this code is the preprocessed AST.
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

import Generation.TypesAndClasses qualified as GTC
import Generation.Helpers qualified as GH
import Preprocessing.Collect qualified as C
import Generation.PrefixesAndHardcoded qualified as GPH

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncAppOrId

instance GTC.ToHaskell P.Char where
  to_haskell = (:[])

instance GTC.ToHaskell (GTC.NeedsAnnotBool, T.Literal) where
  to_haskell = \(needs_annot, lit) -> case lit of
    T.Int i -> GH.to_hs_needs_annot needs_annot i GPH.integer
    T.R r -> GH.to_hs_needs_annot needs_annot r GPH.double
    T.Ch c -> P.show c
    T.S s -> P.show s

instance GTC.ToHaskell T.Identifier where
  to_haskell (T.Id (muip1, id_start, id_conts, mdigit, muip2)) =
    GH.maybe_prefix_args_hs GPH.lower_prefix muip1 ++ GTC.to_haskell id_start ++
    GTC.to_haskell id_conts ++ GTC.to_haskell mdigit ++
    GH.single_quotes_hs muip2

instance GTC.ToHaskell T.SimpleId where
  to_haskell = \(T.SId (id_start, mdigit)) ->
    GTC.to_haskell id_start ++ GTC.to_haskell mdigit

instance GTC.ToHaskell T.IdStart where
  to_haskell = \(T.IS str) -> str

instance GTC.ToHaskell T.IdCont where
  to_haskell = \(T.IC (uip, str)) -> GH.single_quotes_hs uip ++ str

instance GTC.ToHaskell T.ParenExpr where
  to_haskell = \(T.PE ipe) -> "(" ++ GTC.to_haskell ipe ++ ")"

instance GTC.ToHaskell T.InsideParenExpr where
  to_haskell = \case
    T.LOE1 loe -> GTC.to_haskell loe
    T.LFE1 lfe -> GTC.to_haskell lfe

instance GTC.ToHaskell T.Tuple where
  to_haskell (T.T (leou, leous)) =
    GH.run_generator $ GH.add_params_to tuple_hs_gen
    where
    tuple_hs_gen :: GTC.WithParamNum GTC.Haskell
    tuple_hs_gen =
      GTC.to_hs_wpn leou >>= \leou_hs ->
      GTC.to_hs_wpn leous >>= \leous_hs ->
      P.return $ "ft" ++ P.show size ++ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

    size :: P.Int
    size = case leous of
      T.LEOUs (_, l) -> P.length l + 2

instance GTC.ToHsWithParamNum T.LineExprOrUnders where
  to_hs_wpn = \(T.LEOUs (leou, leous)) ->
    GH.to_hs_wpn_list (leou : leous) >$> L.intercalate ", "

instance GTC.ToHsWithParamNum T.LineExprOrUnder where
  to_hs_wpn = \case
    T.LE1 le -> P.return $ GTC.to_haskell le
    T.Underscore1 -> GH.generate_next_param

instance GTC.ToHaskell T.LineExpr where
  to_haskell = \case
    T.BOAE1 boae -> GTC.to_haskell boae
    T.LOE2 loe -> GTC.to_haskell loe
    T.LFE2 lfe -> GTC.to_haskell lfe

instance GTC.ToHaskell T.BasicOrAppExpr where
  to_haskell = \case
    T.BE3 be -> GTC.to_haskell be
    T.PrFA1 prfa -> GTC.to_haskell prfa
    T.PoFA1 pofa -> GTC.to_haskell pofa

instance GTC.ToHaskell T.BasicExpr where
  to_haskell = \case
    T.Lit1 lit -> GTC.to_haskell (GTC.Annot, lit)
    T.PFAOI1 pfaoi -> GTC.to_haskell pfaoi
    T.T1 tuple -> GTC.to_haskell tuple
    T.L1 list -> GTC.to_haskell list
    T.SI1 spid -> P.error $ "special id in basic expr:" ++ P.show spid

instance GTC.ToHsWithIndentLvl T.BigTuple where
  to_hs_wil (T.BT (leou, btsplit, leous, leous_l)) =
    GH.indent_all_and_concat big_tuple_hs_list
    where
    big_tuple_hs_list :: [GTC.Haskell]
    big_tuple_hs_list =
      GH.run_generator $ GH.add_params_to_list big_tuple_hs_list_gen

    big_tuple_hs_list_gen :: GTC.WithParamNum [GTC.Haskell]
    big_tuple_hs_list_gen =
      GTC.to_hs_wpn leou >>= \leou_hs ->
      GTC.to_hs_wpn leous >>= \leous_hs ->
      GH.to_hs_wpn_list leous_l >>= \leous_hs_l ->
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

instance GTC.ToHaskell T.List where
  to_haskell (T.L maybe_leous) =
    GH.run_generator $ GH.add_params_to $ "[" ++> GTC.to_hs_wpn maybe_leous <++ "]"

instance GTC.ToHsWithIndentLvl T.BigList where
  to_hs_wil (T.BL (leous, leous_l)) =
    GH.indent_all_and_concat big_list_hs_list
    where
    big_list_hs_list :: [GTC.Haskell]
    big_list_hs_list = GH.run_generator $ GH.add_params_to_list big_list_hs_list_gen

    big_list_hs_list_gen :: GTC.WithParamNum [GTC.Haskell]
    big_list_hs_list_gen =
      GTC.to_hs_wpn leous >>= \leous_hs ->
      GH.to_hs_wpn_list leous_l >>= \leous_hs_l ->
      P.return $ ["[ " ++ leous_hs] ++ P.map (", " ++) leous_hs_l ++ ["]"]

instance GTC.ToHaskell T.ParenFuncAppOrId where
  to_haskell = \case
    (T.PFAOI (margs1, id_start, args_str_pairs, mdigit, margs2)) ->
      GH.run_generator $ GH.add_params_to paren_func_app_or_id_hs_gen
      where
      paren_func_app_or_id_hs_gen :: GTC.WithParamNum GTC.Haskell
      paren_func_app_or_id_hs_gen =
        GH.change_id_hs_if_needed2 total_id_hs ++>
        GTC.to_hs_wpn (GH.calc_args_list (margs1, margs2) args_str_pairs)

      total_id_hs :: GTC.Haskell
      total_id_hs =
        GH.maybe_prefix_args_hs GPH.lower_prefix margs1 ++ GTC.to_haskell id_start ++
        GH.args_strs_hs args_str_pairs ++ GTC.to_haskell mdigit ++
        GH.single_quotes_hs margs2

instance GTC.ToHsWithParamNum [T.Arguments] where
  to_hs_wpn = \args_l ->
    GH.to_hs_wpn_list args_l >$> \case
      [] -> ""
      args_hs_list -> "(" ++ L.intercalate ", " args_hs_list ++ ")"

instance GTC.ToHsWithParamNum T.Arguments where
  to_hs_wpn = \(T.As leous) -> GTC.to_hs_wpn leous

-- Values: PreFunc, DotId, BasicExpr, Change

instance GTC.ToHaskell T.PreFunc where
  to_haskell = \(T.PF id) ->
    case id of
      T.SId (T.IS "a_value", P.Nothing) -> GPH.just
      T.SId (T.IS "error", P.Nothing) -> GPH.left
      T.SId (T.IS "result", P.Nothing) -> GPH.right
      _ -> GPH.constructor_prefix ++ GTC.to_haskell id

instance GTC.ToHaskell T.PreFuncApp where
  to_haskell = \(T.PrFA (pf, oper)) ->
    GH.run_generator $
    GH.add_params_to $ (GTC.to_haskell pf ++ "(") ++> GTC.to_hs_wpn oper <++ ")"

instance GTC.ToHaskell T.DotId where
  to_haskell (T.DI sosi) =
    sosi_prefix ++ GTC.to_haskell sosi
    where
    sosi_prefix :: GTC.Haskell
    sosi_prefix = case sosi of
      T.SId1 _ -> ""
      T.SI2 _ -> GPH.spid_projection_prefix

instance GTC.ToHaskell T.SimpleOrSpecialId where
  to_haskell = \case
    T.SId1 sid -> GTC.to_haskell sid
    T.SI2 spid -> GTC.to_haskell spid

instance GTC.ToHaskell T.SpecialId where
  to_haskell = \case
    T.First -> "1st"
    T.Second -> "2nd"
    T.Third -> "3rd"
    T.Fourth -> "4th"
    T.Fifth -> "5th"

instance GTC.ToHaskell T.PostFuncApp where
  to_haskell = \case
    T.DIA1 dia@(T.DIA (T.Underscore2, _)) ->
      GH.add_under_pfarg_param (GTC.to_haskell dia)
    T.DIA1 dia -> GTC.to_haskell dia
    T.DCA1 dca@(T.DCA (T.PFA T.Underscore2, _)) ->
      GH.add_under_pfarg_param (GTC.to_haskell dca)
    T.DCA1 dca@(T.DCA (T.DIA2 (T.DIA (T.Underscore2, _)), _)) ->
      GH.add_under_pfarg_param (GTC.to_haskell dca)
    T.DCA1 dca -> GTC.to_haskell dca

instance GTC.ToHaskell T.DotIdsApp where
  to_haskell (T.DIA (pfa, dis)) =
    pfa_dis_to_hs $ P.reverse dis
    where
    pfa_dis_to_hs :: [T.DotId] -> GTC.Haskell
    pfa_dis_to_hs = \case
      [] -> GTC.to_haskell pfa
      di : dis -> GTC.to_haskell di ++ "(" ++ pfa_dis_to_hs dis ++ ")"

instance GTC.ToHaskell T.PostFuncArg where
  to_haskell = \case
    T.PE2 pe -> GTC.to_haskell pe
    T.BE2 be -> GTC.to_haskell be
    T.Underscore2 -> GPH.under_pfarg_param

instance GTC.ToHaskell T.DotChangeApp where
  to_haskell = \(T.DCA (dca, dc)) ->
    GTC.to_haskell dc ++ " " ++ GTC.to_haskell dca

instance GTC.ToHaskell T.DotChangeArg where
  to_haskell = \case
    T.PFA pfa -> GTC.to_haskell pfa
    T.DIA2 dia -> GTC.to_haskell dia

instance GTC.ToHaskell T.DotChange where
  to_haskell (T.DC (fc, fcs)) =
    GH.run_generator (GH.add_params_to change_hs_gen)
    where
    change_hs_gen :: GTC.WithParamNum GTC.Haskell
    change_hs_gen =
      GH.to_hs_wpn_list (fc : fcs) >$> \case
        [] -> P.error "should be impossible"
        [fc_hs] -> fc_hs
        fcs_hs_list -> "(" ++ L.intercalate " .> " fcs_hs_list ++ ")"

instance GTC.ToHsWithParamNum T.FieldChange where
  to_hs_wpn (T.FC (f, leou)) =
    (f_prefix ++ GTC.to_haskell f ++ "(") ++> GTC.to_hs_wpn leou <++ ")"
    where
    f_prefix :: GTC.Haskell
    f_prefix = case f of
      T.SId1 _ -> GPH.change_prefix
      T.SI2 _ -> GPH.spid_change_prefix

-- Values: OpExpr

instance GTC.ToHsWithIndentLvl T.OpExpr where
  to_hs_wil = \case
    T.LOE3 loe -> GH.indent <++ GTC.to_haskell loe
    T.BOE1 boe -> GTC.to_hs_wil boe

instance GTC.ToHsWithParamNum T.OpExprStart where
  to_hs_wpn (T.OES oper_op_pairs) = GH.to_hs_wpn_list oper_op_pairs >$> P.concat

instance GTC.ToHsWithParamNum (T.Operand, T.Op) where
  to_hs_wpn = \(oper, op) -> GTC.to_hs_wpn oper <++ GTC.to_haskell op

instance GTC.ToHaskell T.LineOpExpr where
  to_haskell = \(T.LOE (oes, loee)) ->
    GH.run_generator $ GH.add_params_to $
    GTC.to_hs_wpn oes >++< GTC.to_hs_wpn loee

instance GTC.ToHsWithParamNum T.LineOpExprEnd where
  to_hs_wpn = \case
    T.O1 o -> GTC.to_hs_wpn o
    T.LFE3 lfe -> P.return $ GTC.to_haskell lfe

instance GTC.ToHsWithIndentLvl T.BigOpExpr where
  to_hs_wil = \case
    T.BOEOS1 boeos -> GTC.to_hs_wil boeos
    T.BOEFS1 boefs -> GTC.to_hs_wil boefs

instance GTC.ToHsWithIndentLvl T.BigOpExprOpSplit where
  to_hs_wil (T.BOEOS (osls, maybe_oes, ose)) =
    GH.indent_all_and_concat boeos_hs_list >++< GTC.to_hs_wil ose
    where
    boeos_hs_list :: [GTC.Haskell]
    boeos_hs_list = GH.run_generator $ GH.add_params_to_list boeos_hs_list_gen

    boeos_hs_list_gen :: GTC.WithParamNum [GTC.Haskell]
    boeos_hs_list_gen =
      GH.to_hs_wpn_list osls >>= \osls_hs ->
      GTC.to_hs_wpn maybe_oes >>= \maybe_oes_hs ->
      GTC.to_hs_wpn ose >>= \ose_hs ->
      P.return $ osls_hs ++ [maybe_oes_hs ++ ose_hs]

instance GTC.ToHsWithParamNum T.OpSplitLine where
  to_hs_wpn = \case
    T.OESMOFCO (oes, mofco) -> GTC.to_hs_wpn oes >++< GTC.to_hs_wpn mofco
    T.OFCO1 ofco -> GTC.to_hs_wpn ofco

instance GTC.ToHsWithParamNum T.OperFCO where
  to_hs_wpn = \(T.OFCO (oper, fco)) ->
    GTC.to_hs_wpn oper <++ (" " ++ GTC.to_haskell fco ++ " ")

instance GTC.ToHsWithParamNum T.OpSplitEnd where
  to_hs_wpn = \case
    T.O2 o -> GTC.to_hs_wpn o
    _ -> P.return ""

instance GTC.ToHsWithIndentLvl T.OpSplitEnd where
  to_hs_wil = \case
    T.FE1 fe -> GTC.to_hs_wil (fe, GTC.NoWhereExpr)
    _ -> P.return ""

instance GTC.ToHsWithIndentLvl T.BigOpExprFuncSplit where
  to_hs_wil (T.BOEFS (oes, bocfe)) =
    GH.indent_all_and_concat params_and_oes_hs_list >++< GTC.to_hs_wil bocfe
    where
    params_and_oes_hs_list :: [GTC.Haskell]
    params_and_oes_hs_list =
      GH.run_generator $ GH.add_params_to_list $ GH.to_hs_wpn_list [oes]

instance GTC.ToHsWithIndentLvl T.BigOrCasesFuncExpr where
  to_hs_wil = \case
    T.BFE1 bfe -> GTC.to_hs_wil (bfe, GTC.NoWhereExpr)
    T.CFE1 cfe -> GTC.to_hs_wil (cfe, GTC.NoWhereExpr)

instance GTC.ToHsWithParamNum T.Operand where
  to_hs_wpn = \case
    T.BOAE2 boae -> P.return $ GTC.to_haskell boae
    T.PE3 pe -> P.return $ GTC.to_haskell pe
    T.Underscore3 -> GH.generate_next_param

instance GTC.ToHaskell T.Op where
  to_haskell = \case
    T.FCO3 fco -> " " ++ GTC.to_haskell fco ++ " "
    T.OSO oso -> " " ++ GTC.to_haskell oso ++ " "

instance GTC.ToHaskell T.FuncCompOp where
  to_haskell = \case
    T.RightComp -> ".>"
    T.LeftComp -> "<."

instance GTC.ToHaskell T.OptionalSpacesOp where
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

instance GTC.ToHsWithIndentLvl (T.FuncExpr, GTC.PossiblyWhereExpr) where
  to_hs_wil = \(fe, pwe) -> case fe of
    T.LFE4 lfe -> GTC.to_hs_wil (lfe, pwe)
    T.BFE2 bfe -> GTC.to_hs_wil (bfe, pwe)
    T.CFE2 cfe -> GTC.to_hs_wil (cfe, pwe)

instance GTC.ToHaskell T.LineFuncExpr where
  to_haskell = \(T.LFE (params, lfb)) ->
    GTC.to_haskell (GTC.Whole params) ++ " " ++ GTC.to_haskell lfb

instance GTC.ToHsWithIndentLvl (T.LineFuncExpr, GTC.PossiblyWhereExpr) where
  to_hs_wil = \(lfe@(T.LFE (params, lfb)), pwe) ->
    case pwe of
      GTC.NoWhereExpr -> P.return $ GTC.to_haskell lfe
      GTC.HasWhereExpr we ->
        (GTC.to_haskell (GTC.Whole params) ++ "\n") ++> GTC.to_hs_wil we >++<
        GH.indent <++ GTC.to_haskell lfb

instance GTC.ToHsWithIndentLvl (T.BigFuncExpr, GTC.PossiblyWhereExpr) where
  to_hs_wil (T.BFE (params, bfb), pwe) =
    params_hs ++> GTC.to_hs_wil pwe >++< GTC.to_hs_wil bfb
    where
    params_hs :: GTC.Haskell
    params_hs = GTC.to_haskell (GTC.Whole params) ++ "\n"

instance GTC.ToHaskell GTC.WholeParams where
  to_haskell = \(GTC.Whole params) -> "\\" ++ GTC.to_haskell params ++ " ->"

instance GTC.ToHaskell T.Parameters where
  to_haskell = \case
    T.ParamId id -> GTC.to_haskell id
    T.Star1 -> "_"
    T.Params (params, params_l) ->
      "(" ++ GTC.to_haskell params ++ GH.to_hs_prepend_list ", " params_l ++ ")"

instance GTC.ToHaskell T.LineFuncBody where
  to_haskell = \case
    T.BOAE3 boae -> GTC.to_haskell boae
    T.LOE4 loe -> GTC.to_haskell loe
    T.PLFE1 plfe -> GTC.to_haskell plfe

instance GTC.ToHaskell T.ParenLineFuncExpr where
  to_haskell = \(T.PLFE lfe) -> "(" ++ GTC.to_haskell lfe ++ ")"

instance GTC.ToHsWithIndentLvl T.BigFuncBody where
  to_hs_wil = \case
    T.BOAE4 boae -> GH.indent <++ GTC.to_haskell boae
    T.OE1 oe -> GTC.to_hs_wil oe
    T.PLFE2 plfe -> GH.indent <++ GTC.to_haskell plfe

instance GTC.ToHsWithIndentLvl (T.CasesFuncExpr, GTC.PossiblyWhereExpr) where
  to_hs_wil (T.CFE (cps, cs, maybe_ec), pwe) =
    params_hs ++> GTC.to_hs_wil pwe >++<
    GH.indent <++ case_of_hs >++<
    GH.deeper (GTC.to_hs_wil (cs, maybe_ec))
    where
    (params_hs, case_of_hs) = GH.run_generator params_and_case_of_hs_gen
      :: (GTC.Haskell, GTC.Haskell)

    params_and_case_of_hs_gen :: GTC.WithParamNum (GTC.Haskell, GTC.Haskell)
    params_and_case_of_hs_gen =
      GTC.to_hs_wpn cps >>= \cps_hs ->
      GH.case_of_inner_hs_gen >>= \case_of_inner_hs ->
      P.return $
        ("\\" ++ cps_hs ++ " ->\n", "case " ++ case_of_inner_hs ++ " of")

instance GTC.ToHsWithIndentLvl ([T.Case], P.Maybe T.EndCase) where
  to_hs_wil = \(cs, maybe_ec) ->
    GH.to_hs_wil_list cs >$> P.concat >++< GTC.to_hs_wil maybe_ec

instance GTC.ToHsWithParamNum T.CasesParams where
  to_hs_wpn = \case
    T.CParamId id -> P.return $ GTC.to_haskell id
    T.QuestionMark -> GH.generate_next_param
    T.Star2 -> P.return "_"
    T.CParams (cps, cps_l) ->
      GH.to_hs_wpn_list (cps : cps_l) >$> \cps_l_hs ->
      "(" ++ L.intercalate ", " cps_l_hs ++ ")"

instance GTC.ToHsWithIndentLvl T.Case where
  to_hs_wil = \(T.Ca (om, cb)) ->
    "\n" ++> GH.indent <++ (GTC.to_haskell om ++ " ->") >++<
    GH.deeper (GTC.to_hs_wil cb)

instance GTC.ToHsWithIndentLvl T.EndCase where
  to_hs_wil = \(T.EC (ecp, cb)) ->
    "\n" ++> GH.indent <++ (GTC.to_haskell ecp ++ " ->") >++<
    GH.deeper (GTC.to_hs_wil cb)

instance GTC.ToHaskell T.OuterMatching where
  to_haskell = \case
    T.SId2 sid -> GTC.to_haskell sid
    T.M1 m -> GTC.to_haskell (GTC.NoParen, m)

instance GTC.ToHaskell T.EndCaseParam where
  to_haskell = \case
    T.Id2 id -> GTC.to_haskell id
    T.Ellipsis -> "_"

instance GTC.ToHaskell (GTC.NeedsParenBool, T.Matching) where
  to_haskell = \(needs_paren, m) -> case m of
    T.Lit2 lit -> GTC.to_haskell (GTC.NoAnnot, lit)
    T.PFM (pf, im) ->
      GH.in_paren_if needs_paren $
        GTC.to_haskell pf ++ " " ++ GTC.to_haskell (GTC.Paren, im)
    T.TM1 tm -> GTC.to_haskell tm
    T.LM1 lm -> GTC.to_haskell lm

instance GTC.ToHaskell (GTC.NeedsParenBool, T.InnerMatching) where
  to_haskell = \(needs_paren, im) -> case im of
    T.Star -> "_"
    T.Id3 id -> GTC.to_haskell id
    T.M2 m -> GTC.to_haskell (needs_paren, m)

instance GTC.ToHaskell T.TupleMatching where
  to_haskell (T.TM (im, im_l)) =
    "(" ++ ims_hs ++ ")"
    where
    ims_hs :: GTC.Haskell
    ims_hs =
      (im : im_l) &> P.map (\im -> (GTC.NoParen, im)) &>
      GH.to_hs_intercalate ", "

instance GTC.ToHaskell T.ListMatching where
  to_haskell (T.LM m_list_internals) =
    case m_list_internals of
      P.Nothing -> "[]"
      P.Just (im, im_l, P.Nothing) -> "[" ++ commas_ims_hs (im, im_l) ++ "]"
      P.Just (im, im_l, P.Just rlm) ->
        colons_ims_hs (im, im_l) ++ GTC.to_haskell rlm
    where
    commas_ims_hs :: (T.InnerMatching, [T.InnerMatching]) -> GTC.Haskell
    commas_ims_hs = \(im, im_l) ->
      (im : im_l) &> P.map (\im -> (GTC.NoParen, im)) &>
      GH.to_hs_intercalate ", "

    colons_ims_hs :: (T.InnerMatching, [T.InnerMatching]) -> GTC.Haskell
    colons_ims_hs = \(im, im_l) ->
      (im : im_l) &> P.map (\im -> GTC.to_haskell (GTC.Paren, im)) &>
      P.concatMap (++ " : ")


instance GTC.ToHaskell T.RestListMatching where
  to_haskell = \(T.RLM msid) ->
    case msid of
      P.Nothing -> "_"
      P.Just sid -> GTC.to_haskell sid

instance GTC.ToHsWithIndentLvl T.CaseBody where
  to_hs_wil = \case
    T.LFB1 lfb -> P.return $ " " ++ GTC.to_haskell lfb
    T.BFB1 (bfb, maybe_we) ->
      "\n" ++> GTC.to_hs_wil maybe_we >++< GTC.to_hs_wil bfb

-- Values: ValueDef, GroupedValueDefs, WhereExpr

instance GTC.ToHsWithIndentLvl T.ValueDef where
  to_hs_wil (T.VD (id, t, maybe_ve)) =
    GH.indent <++ (GTC.to_haskell id ++ " :: ") >++<
    (forall_hs <$> MS.get) <++ (GTC.to_haskell t ++ "\n") >++<
    GH.indent <++ (GTC.to_haskell id ++ " =\n") >++<
    GH.deeper maybe_ve_wil
    where
    forall_hs :: P.Int -> GTC.Haskell
    forall_hs = \case
      0 ->
        case P.concatMap (" " ++) param_t_vars_hs_list of
          "" -> ""
          hs -> "forall" ++ hs ++ ". "
      _ -> ""

    param_t_vars_hs_list :: [GTC.Haskell]
    param_t_vars_hs_list = C.param_t_vars t &> P.map GTC.to_haskell

    maybe_ve_wil :: GTC.WithIndentLvl GTC.Haskell
    maybe_ve_wil = case maybe_ve of
      P.Nothing -> GH.indent <++ "P.undefined"
      P.Just ve -> GTC.to_hs_wil ve

instance GTC.ToHsWithIndentLvl T.ValueEquals where
  to_hs_wil = \(T.VE (ve, maybe_we)) ->
    GTC.to_hs_wil (ve, GH.mwe_to_pwe maybe_we)

instance GTC.ToHsWithIndentLvl (T.ValueExpr, GTC.PossiblyWhereExpr) where
  to_hs_wil = \(ve, pwe) -> case ve of
    T.FE2 fe -> GH.indent >++< GTC.to_hs_wil (fe, pwe)
    _ ->
      GTC.to_hs_wil pwe >++< case ve of
        T.BOAE5 boae -> GH.indent <++ GTC.to_haskell boae
        T.OE2 oe -> GTC.to_hs_wil oe
        T.BT1 bt -> GTC.to_hs_wil bt
        T.BL1 bl -> GTC.to_hs_wil bl

instance GTC.ToHsWithIndentLvl T.GroupedValueDefs where
  to_hs_wil (T.GVDs (T.Ids (id, ids), ts, les, les_l)) =
    GH.to_hs_wil_list vd_list >$> L.intercalate "\n\n"
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
        T.VD (id, t, P.Just $ T.VE (le_to_ve le, P.Nothing)) :
        to_val_def_list (ids, ts, les)
      _ ->
        P.error $
          "identifiers, types and expressions don't match in number " ++
          "in grouped value definitions"

    le_to_ve :: T.LineExpr -> T.ValueExpr
    le_to_ve = \case
      T.BOAE1 boae -> T.BOAE5 boae
      T.LOE2 loe -> T.OE2 $ T.LOE3 loe
      T.LFE2 lfe -> T.FE2 $ T.LFE4 lfe

instance GTC.ToHsWithIndentLvl T.WhereExpr where
  to_hs_wil (T.WE (wde, wdes)) =
    GH.indent <++ "let\n" >++<
    (GH.to_hs_wil_list (wde : wdes) >$> L.intercalate "\n\n") <++ "\n" >++<
    GH.indent <++ "in\n"

instance GTC.ToHsWithIndentLvl T.ValueDefOrDefs where
  to_hs_wil = \case
    T.VD1 vd -> GTC.to_hs_wil vd
    T.GVDs1 gvd -> GTC.to_hs_wil gvd

-- Type

instance GTC.ToHaskell T.Type where
  to_haskell = \(T.Ty (maybe_c, st)) ->
    GTC.to_haskell maybe_c ++ GTC.to_haskell (GTC.NoParen, st)

instance GTC.ToHaskell (GTC.NeedsParenBool, T.SimpleType) where
  to_haskell = \(needs_paren, st) -> case st of
    T.TAIOA1 taioa -> GTC.to_haskell (needs_paren, taioa)
    T.POPT1 popt -> GTC.to_haskell popt
    T.FT1 ft -> GH.in_paren_if needs_paren $ GTC.to_haskell ft

instance GTC.ToHaskell T.ProdOrPowerType where
  to_haskell = \case
    T.PT4 pt -> GTC.to_haskell pt
    T.PoT5 pt -> GTC.to_haskell pt

instance GTC.ToHaskell T.TypeId where
  to_haskell = \(T.TId str) -> str

instance GTC.ToHaskell T.ParamTVar where
  to_haskell = \(T.PTV i) -> GPH.param_t_var_prefix ++ P.show i

instance GTC.ToHaskell T.AdHocTVar where
  to_haskell = \(T.AHTV c) -> GPH.ad_hoc_t_var_prefix ++ P.show (CH.ord c - 65)

instance GTC.ToHaskell (GTC.NeedsParenBool, T.TypeAppIdOrTV) where
  to_haskell (needs_paren, taiot) = case taiot of
    T.PTV1 ptv -> GTC.to_haskell ptv
    T.TAIOA taioa ->
      case taioa of
        (P.Nothing, T.TIdStart1 (T.TId tid, []), P.Nothing) ->
          tid &> \case
            "Bool" -> GPH.bool
            "Int" -> GPH.integer
            "Real" -> GPH.double
            "Char" -> GPH.char
            "String" -> GPH.string
            _ -> GTC.to_haskell tid

        (mtip1, taioam, mtip2) -> case taioam of
          T.AHTV1 ahtv ->
            in_paren_if_needed (GTC.to_haskell ahtv) $ mtip1_hs ++ mtip2_hs

          T.TIdStart1 (tid, tip_str_pairs) ->
            in_paren_if_needed tid_hs tip_hs
            where
            tid_hs :: GTC.Haskell
            tid_hs =
              GH.maybe_prefix_args_hs GPH.upper_prefix mtip1 ++
              GTC.to_haskell tid ++ GH.args_strs_hs tip_str_pairs ++
              GH.single_quotes_hs mtip2

            tip_hs :: GTC.Haskell
            tip_hs =
              mtip1_hs ++ GTC.to_haskell (P.map P.fst tip_str_pairs) ++ mtip2_hs
          where
          in_paren_if_needed :: GTC.Haskell -> GTC.Haskell -> GTC.Haskell
          in_paren_if_needed = \hs tip_hs ->
            case tip_hs of
              "" -> hs
              _ -> GH.in_paren_if needs_paren $ hs ++ tip_hs

          mtip1_hs :: GTC.Haskell
          mtip1_hs = GTC.to_haskell mtip1

          mtip2_hs :: GTC.Haskell
          mtip2_hs = GTC.to_haskell mtip2

instance GTC.ToHaskell T.TypesInParen where
  to_haskell = \(T.TIP (st, sts)) ->
    GH.to_hs_prepend_list " " $ P.map (\st -> (GTC.Paren, st)) $ st : sts

instance GTC.ToHaskell T.ProdType where
  to_haskell = \(T.PT (ft, fts)) ->
    "(" ++ ((ft : fts) &> GH.to_hs_intercalate ", ") ++ ")"

instance GTC.ToHaskell T.FieldType where
  to_haskell = \case
    T.PBT1 ft -> GTC.to_haskell ft
    T.PoT2 pt -> GTC.to_haskell pt

instance GTC.ToHaskell T.PowerBaseType where
  to_haskell = \case
    T.TAIOA2 taioa -> GTC.to_haskell (GTC.NoParen, taioa)
    T.IPT ipt -> GTC.to_haskell ipt

instance GTC.ToHaskell T.InParenT where
  to_haskell = \case
    T.PT3 pt -> GTC.to_haskell pt
    T.PoT3 pt -> GTC.to_haskell pt
    T.FT3 ft -> GTC.to_haskell ft

instance GTC.ToHaskell T.PowerType where
  to_haskell = \(T.PoT (ft, i)) ->
    "(" ++
    (L.replicate (P.fromIntegral i) ft &> GH.to_hs_intercalate ", ") ++
    ")"

instance GTC.ToHaskell T.FuncType where
  to_haskell = \(T.FT (it, ot)) ->
    GTC.to_haskell it ++ " -> " ++ GTC.to_haskell ot

instance GTC.ToHaskell T.InOrOutType where
  to_haskell = \case
    T.TAIOA3 taioa -> GTC.to_haskell (GTC.NoParen, taioa)
    T.POPT2 popt -> GTC.to_haskell popt
    T.FT2 ft -> "(" ++ GTC.to_haskell ft ++ ")"

instance GTC.ToHaskell T.Condition where
  to_haskell = \(T.Co pn) -> GTC.to_haskell pn ++ " => "

-- TypeDef, TypeNickname

instance GTC.ToHaskell T.TypeDef where
  to_haskell = \case
    T.TTD1 ttd -> GTC.to_haskell ttd
    T.OTD1 otd -> GTC.to_haskell otd

instance GTC.ToHaskell T.TupleTypeDef where
  to_haskell (T.TTD (tn, popt, T.PCSIs (T.SIds (sid, sids)))) =
    data_hs ++ "\n\n" ++ instance_hs ++ "\n" ++
    change_types_hs ++ change_defs_hs
    where
    data_hs :: GTC.Haskell
    data_hs =
      "data " ++ tn_hs ++ " =\n  " ++ cons_hs ++
      " { " ++ projections_and_types_hs ++ " }"

    instance_hs :: GTC.Haskell
    instance_hs =
      "instance FromTuple" ++ size_hs ++ types_hs ++ " " ++
      GTC.to_haskell (GTC.Paren, tn) ++ " where\n" ++
      "  ft" ++ size_hs ++
      " = \\(" ++ L.intercalate ", " params_list ++ ") -> " ++
      cons_hs ++ P.concatMap (" " ++) params_list

    change_types_hs :: GTC.Haskell
    change_types_hs = GH.combine_with_ts change_hs_list change_types_hs_list

    change_defs_hs :: GTC.Haskell
    change_defs_hs = GH.combine_with_defs change_hs_list sid_hs_list

    sid_hs_list :: [GTC.Haskell]
    sid_hs_list = P.map GTC.to_haskell $ sid : sids

    change_hs_list :: [GTC.Haskell]
    change_hs_list = P.map (GPH.change_prefix ++) sid_hs_list

    change_types_hs_list :: [GTC.Haskell]
    change_types_hs_list = P.map to_change_type [0..4]

    to_change_type :: P.Int -> GTC.Haskell
    to_change_type = \i ->
      (types_hs_list !! i) ++ " -> " ++ tn_hs  ++ " -> " ++ tn_hs

    tn_hs :: GTC.Haskell
    tn_hs = GTC.to_haskell (GTC.NoParen, tn)

    types_list :: [T.SimpleType]
    types_list = case popt of
      T.PT4 (T.PT (ft, fts)) -> P.map GH.ft_to_st $ ft : fts
      T.PoT5 (T.PoT (pbt, i)) -> L.replicate (P.fromIntegral i) $ GH.pbt_to_st pbt

    types_hs_list :: [GTC.Haskell]
    types_hs_list = P.map (\st -> GTC.to_haskell (GTC.NoParen, st)) types_list

    types_hs :: GTC.Haskell
    types_hs = P.concatMap (\st -> " " ++ GTC.to_haskell (GTC.Paren, st)) types_list

    size :: P.Int
    size = P.length types_hs_list

    size_hs :: GTC.Haskell
    size_hs = P.show size

    projections_and_types_hs :: GTC.Haskell
    projections_and_types_hs =
      P.zipWith (\a b -> a ++ " :: " ++ b) sid_hs_list types_hs_list &>
      L.intercalate ", "

    cons_hs :: GTC.Haskell
    cons_hs = GH.tn_to_cons_hs tn

    params_list :: [GTC.Haskell]
    params_list = [1..size] &> P.map (\i -> "x" ++ P.show i)

instance GTC.ToHaskell (GTC.NeedsParenBool, T.TypeName) where
  to_haskell (needs_paren, tn@(T.TN (mpvip1, _, pvip_str_pairs, mpvip2))) =
    case pvips_hs of
      "" -> GH.tn_to_tid_hs tn
      _ -> GH.in_paren_if needs_paren $ GH.tn_to_tid_hs tn ++ pvips_hs
    where
    pvips_hs :: GTC.Haskell
    pvips_hs =
      GTC.to_haskell mpvip1 ++ GTC.to_haskell pvips ++ GTC.to_haskell mpvip2

    pvips :: [T.ParamVarsInParen]
    pvips = P.map P.fst pvip_str_pairs

instance GTC.ToHaskell T.ParamVarsInParen where
  to_haskell = \(T.PVIP (ptv, ptvs)) -> GH.to_hs_prepend_list " " $ ptv : ptvs

instance GTC.ToHaskell T.OrTypeDef where
  to_haskell (T.OTD (tn, otv)) =
    "data " ++ GTC.to_haskell (GTC.NoParen, tn) ++ " =\n  " ++ GTC.to_haskell otv

instance GTC.ToHaskell T.OrTypeValues where
  to_haskell = \case
    T.VL otvsl -> GTC.to_haskell otvsl
    T.Ls otvsls -> GTC.to_haskell otvsls

instance GTC.ToHaskell T.OrTypeValuesLine where
  to_haskell = \(T.OTVL (otv, otvs)) ->
    GTC.to_haskell otv ++ GH.to_hs_prepend_list " |\n  " otvs

instance GTC.ToHaskell T.OrTypeValuesLines where
  to_haskell = \(T.OTVLs (otvsl, otvsls)) ->
    GH.to_hs_intercalate " |\n  " $ otvsl : otvsls

instance GTC.ToHaskell T.OrTypeValue where
  to_haskell = \(T.OTV (sid, maybe_with_val)) ->
    GPH.constructor_prefix ++ GTC.to_haskell sid ++ case maybe_with_val of
      P.Nothing -> ""
      P.Just iv -> GTC.to_haskell iv

instance GTC.ToHaskell T.InternalValue where
  to_haskell = \(T.IV (_, st)) -> " " ++ GTC.to_haskell (GTC.Paren, st)

instance GTC.ToHaskell T.TypeNickname where
  to_haskell = \(T.TNN (tn, st)) ->
    "type " ++ GTC.to_haskell (GTC.NoParen, tn) ++ " = " ++
    GTC.to_haskell (GTC.NoParen, st)

-- TypePropDef

instance GTC.ToHaskell T.TypePropDef where
  to_haskell = \case
    T.APD1 apd -> GTC.to_haskell apd
    T.RPD1 rpd -> GTC.to_haskell rpd

instance GTC.ToHaskell T.AtomPropDef where
  to_haskell = \(T.APD (pnl, id, st)) ->
    "class " ++ GTC.to_haskell pnl ++ " where\n  " ++
    GTC.to_haskell id ++ " :: " ++ GTC.to_haskell (GTC.NoParen, st)

instance GTC.ToHaskell T.RenamingPropDef where
  to_haskell = \_ -> ""

instance GTC.ToHaskell T.PropNameLine where
  to_haskell = \(T.PNL pn) -> GTC.to_haskell pn

instance GTC.ToHaskell T.PropName where
  to_haskell = \case
    T.NPStart1 np_start -> GTC.to_haskell np_start
    T.TIPStart tip_start -> GTC.to_haskell tip_start

instance GTC.ToHaskell T.NPStart1 where
  to_haskell (c, np_tip_pairs, maybe_np) =
    [c] ++ GH.nps_args_hs np_tip_pairs ++ GH.to_hs_maybe_np maybe_np ++
    GTC.to_haskell tips
    where
    tips :: [T.TypesInParen]
    tips = P.map P.snd np_tip_pairs

instance GTC.ToHaskell T.TIPStart where
  to_haskell (tip_np_pairs, maybe_tip) =
    GH.args_nps_hs tip_np_pairs ++ GH.single_quotes_hs maybe_tip ++
    GTC.to_haskell tips ++ GTC.to_haskell maybe_tip
    where
    tips :: [T.TypesInParen]
    tips = P.map P.fst tip_np_pairs

instance GTC.ToHaskell T.NamePart where
  to_haskell = \(T.NP str) -> str

-- TypeTheo

instance GTC.ToHaskell T.TypeTheo where
  to_haskell (T.TT (pnws_l, maybe_pnws, proof)) =
    "instance " ++ pnws_l_hs ++
    mpnws_to_hs maybe_pnws ++ " where\n  " ++ GTC.to_haskell proof
    where
    pnws_l_hs :: GTC.Haskell
    pnws_l_hs = case pnws_l of
      [] -> P.error "to_haskell type_theo: no pnws before arrow"
      [pnws] -> GTC.to_haskell pnws
      _ -> GH.to_hs_intercalate ", " pnws_l &> \x -> "(" ++ x ++ ")"

    mpnws_to_hs :: P.Maybe T.PropNameWithSubs -> P.String
    mpnws_to_hs = \case
      P.Nothing -> ""
      P.Just pnws -> " => " ++ GTC.to_haskell pnws

instance GTC.ToHaskell T.PropNameWithSubs where
  to_haskell = \case
    T.NPStart2 np_start -> GTC.to_haskell np_start
    T.SIPStart sip_start -> GTC.to_haskell sip_start

instance GTC.ToHaskell T.NPStart2 where
  to_haskell (c, np_sip_pairs, maybe_np) =
    GH.change_prop_hs_if_needed prop_hs ++ GTC.to_haskell sips
    where
    prop_hs :: GTC.Haskell
    prop_hs = [c] ++ GH.nps_args_hs np_sip_pairs ++ GH.to_hs_maybe_np maybe_np

    sips :: [T.SubsInParen]
    sips = P.map P.snd np_sip_pairs

instance GTC.ToHaskell T.SIPStart where
  to_haskell (sip_np_pairs, maybe_sip) =
    GH.change_prop_hs_if_needed prop_hs ++ GH.single_quotes_hs maybe_sip ++
    GTC.to_haskell sips ++ GTC.to_haskell maybe_sip
    where
    prop_hs :: GTC.Haskell
    prop_hs = GH.args_nps_hs sip_np_pairs

    sips :: [T.SubsInParen]
    sips = P.map P.fst sip_np_pairs

instance GTC.ToHaskell T.SubsInParen where
  to_haskell = \(T.SIP (tvs, tvss)) -> GH.to_hs_prepend_list " " $ tvs : tvss

instance GTC.ToHaskell T.TVarSub where
  to_haskell = \case
    T.TAIOAS1 tasoi -> GTC.to_haskell (GTC.Paren, tasoi)
    T.POPTS1 popts -> GTC.to_haskell popts
    T.FTS1 fts -> "(" ++ GTC.to_haskell fts  ++ ")"

instance GTC.ToHaskell T.ProdOrPowerTypeSub where
  to_haskell = \case
    T.PTS1 pts -> GTC.to_haskell pts
    T.PoTS1 pts -> GTC.to_haskell pts

instance GTC.ToHaskell (GTC.NeedsParenBool, T.TypeAppIdOrTVSub) where
  to_haskell (needs_paren, T.TAIOAS taioas) = case taioas of
    (P.Nothing, T.TIdStart2 (T.TId "Real", []), P.Nothing) -> "Double"
    (P.Nothing, T.TIdStart2 (T.TId "Int", []), P.Nothing) -> "Integer"
    (msouip1, taioasm, msouip2) -> case taioasm of
      T.AHTV2 ahtv ->
        in_paren_if_needed (GTC.to_haskell ahtv) souip_hs
        where
        souip_hs :: GTC.Haskell
        souip_hs = GTC.to_haskell msouip1 ++ GTC.to_haskell msouip2

      T.TIdStart2 (tid, souip_str_pairs) ->
        in_paren_if_needed tid_hs souip_hs
        where
        tid_hs :: GTC.Haskell
        tid_hs =
          GH.maybe_prefix_args_hs GPH.upper_prefix msouip1 ++
          GTC.to_haskell tid ++ GH.args_strs_hs souip_str_pairs ++
          GH.single_quotes_hs msouip2

        souip_hs :: GTC.Haskell
        souip_hs =
          GTC.to_haskell msouip1 ++
          GTC.to_haskell (P.map P.fst souip_str_pairs) ++
          GTC.to_haskell msouip2
      where
      in_paren_if_needed :: GTC.Haskell -> GTC.Haskell -> GTC.Haskell
      in_paren_if_needed = \hs souip_hs ->
        case souip_hs of
          "" -> hs
          _ -> GH.in_paren_if needs_paren $ hs ++ souip_hs

instance GTC.ToHaskell T.SubsOrUndersInParen where
  to_haskell = \(T.SOUIP (sou, sous)) -> GTC.to_haskell $ sou : sous

instance GTC.ToHaskell T.SubOrUnder where
  to_haskell = \case
    T.TVS1 tvs -> " " ++ GTC.to_haskell tvs
    T.Underscore4 -> ""

instance GTC.ToHaskell T.PowerTypeSub where
  to_haskell = \(T.PoTS (pbts, i)) ->
    "(" ++
    (L.replicate (P.fromIntegral i) pbts &> GH.to_hs_intercalate ", ")
    ++
    ")"

instance GTC.ToHaskell T.PowerBaseTypeSub where
  to_haskell = \case
    T.Underscore5 -> P.undefined
    T.TAIOAS2 tasoi -> GTC.to_haskell (GTC.NoParen, tasoi)
    T.IPTS ipts -> GTC.to_haskell ipts

instance GTC.ToHaskell T.InParenTSub where
  to_haskell = \case
    T.PTS2 pts -> "(" ++ GTC.to_haskell pts ++ ")"
    T.FTS2 fts -> "(" ++ GTC.to_haskell fts ++ ")"

instance GTC.ToHaskell T.ProdTypeSub where
  to_haskell = \(T.PTS (fts, fts_l)) ->
    "(" ++ ((fts : fts_l) &> GH.to_hs_intercalate ", ") ++ ")"

instance GTC.ToHaskell T.FieldTypeSub where
  to_haskell = \case
    T.PBTS1 pbts -> GTC.to_haskell pbts
    T.PoTS2 pots -> GTC.to_haskell pots

instance GTC.ToHaskell T.FuncTypeSub where
  to_haskell = \(T.FTS (in_ts, out_ts)) ->
    GTC.to_haskell in_ts ++ " -> " ++ GTC.to_haskell out_ts

instance GTC.ToHaskell T.InOrOutTypeSub where
  to_haskell = \case
    T.Underscore6 -> P.undefined
    T.TAIOAS3 tasoi -> GTC.to_haskell (GTC.NoParen, tasoi)
    T.POPTS2 popts -> GTC.to_haskell popts
    T.FTS3 fts -> GTC.to_haskell fts

instance GTC.ToHaskell T.Proof where
  to_haskell = \case
    T.P1 (iooe, le) -> GTC.to_haskell iooe ++ " " ++ GTC.to_haskell le
    T.P2 (iooe, ttve) -> GTC.to_haskell iooe ++ GTC.to_haskell ttve

instance GTC.ToHaskell T.IdOrOpEq where
  to_haskell (T.IOOE (id, maybe_op_id)) =
    GH.change_id_hs_if_needed1 (GTC.to_haskell id) ++ maybe_op_id_hs ++ " ="
    where
    maybe_op_id_hs :: GTC.Haskell
    maybe_op_id_hs = case maybe_op_id of
      P.Nothing -> ""
      P.Just (op, id) -> GTC.to_haskell op ++ GTC.to_haskell id

instance GTC.ToHaskell T.TTValueExpr where
  to_haskell = \case
    T.LE2 le -> " " ++ GTC.to_haskell le
    T.VEMWE (ve, maybe_we) ->
      "\n" ++
      GH.run_generator
        (GH.twice_deeper (GTC.to_hs_wil (ve, GH.mwe_to_pwe maybe_we)))

-- Program

instance GTC.ToHaskell T.Program where
  to_haskell = \(T.P (pp, pps)) ->
    GTC.to_haskell pp ++ GH.to_hs_prepend_list "\n\n" pps

instance GTC.ToHaskell T.ProgramPart where
  to_haskell = \case
    T.VDD vdd -> GH.run_generator $ GTC.to_hs_wil vdd
    T.TD td -> GTC.to_haskell td
    T.TNN1 tnn -> GTC.to_haskell tnn
    T.TPD tpd -> GTC.to_haskell tpd
    T.TT1 tt -> GTC.to_haskell tt
    T.C1 c -> ""

-- Helper instances

instance GTC.ToHsWithIndentLvl GTC.PossiblyWhereExpr where
  to_hs_wil = \case
    GTC.NoWhereExpr -> P.return ""
    GTC.HasWhereExpr we -> GTC.to_hs_wil we

{-
For fast vim file navigation:
TypesAndHelpers.hs
Preprocess.hs
-}
