{-
This file contains code that changes the AST from the parsing step into a new
more haskell-like AST for the translation step. This is done mainly by
instances of the Preprocess type class which are instances that change all the
AST nodes that need to be changed. A basic expression may be preprocessed into
a post function application under certain circumstances, this is accomplished
by the ToMaybePostFuncApp class.
-}

{-# language
  LambdaCase, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving,
  GeneralizedNewtypeDeriving
#-}

module Preprocessing.Preprocess where

import Prelude (($), (++), (>>=), (<$>), (/=), (>>))
import Prelude qualified as P
import Data.Set qualified as S
import Data.Map qualified as M
import Control.Monad qualified as MO
import Control.Monad.State qualified as MS

import ASTTypes qualified as T
import Helpers ((&>), (>$>), (.>), (++<), (+++<))
import Helpers qualified as H
import ShowInstances qualified as S

import Parsing.AST qualified as PA
import Preprocessing.TypesAndClasses qualified as PTC
import Preprocessing.Collect qualified as PC
import Preprocessing.CheckCompatibility qualified as PCC
import Generation.Helpers qualified as GH
import Generation.PrefixesAndHardcoded qualified as GPH

-- Preprocess instances, general preprocess functions, automatic instances
--   and instances for pairs and triples

preprocess_prog :: T.Program -> T.Program
preprocess_prog = \prog -> MS.evalState (PTC.preprocess prog) (init_state prog)

instance PTC.Preprocess a => PTC.Preprocess [a] where
  preprocess = P.traverse PTC.preprocess

instance PTC.Preprocess a => PTC.Preprocess (P.Maybe a) where
  preprocess = P.traverse PTC.preprocess

preprocess_pair
  :: (PTC.Preprocess a, PTC.Preprocess b) => (a, b) -> PTC.PreprocessState (a, b)
preprocess_pair = \(a, b) -> PTC.preprocess a ++< PTC.preprocess b

preprocess_first :: PTC.Preprocess a => (a, b) -> PTC.PreprocessState (a, b)
preprocess_first = \(a, b) -> PTC.preprocess a >$> \a' -> (a', b)

preprocess_second :: PTC.Preprocess b => (a, b) -> PTC.PreprocessState (a, b)
preprocess_second = \(a, b) -> PTC.preprocess b >$> \b' -> (a, b')

preprocess_triple
  :: (PTC.Preprocess a, PTC.Preprocess b, PTC.Preprocess c) =>
     (a, b, c) -> PTC.PreprocessState (a, b, c)
preprocess_triple = \(a, b, c) ->
  PTC.preprocess a ++< PTC.preprocess b +++< PTC.preprocess c

-- regular Preprocess instances

instance PTC.Preprocess T.Identifier where
  preprocess = \id ->
    GH.check_if_id_is_sid id &> \case
      P.Nothing -> P.return id
      P.Just sid -> PTC.preprocess sid >$> GH.sid_to_id

instance PTC.Preprocess T.SimpleId where
  preprocess = \sid ->
    check_if_sid_in_ncs sid >$> \case
      P.True -> add_constructor_prefix sid
      P.False -> change_if_particular_sid sid

deriving instance PTC.Preprocess T.ParenExpr

instance PTC.Preprocess T.InsideParenExpr where
  preprocess = \case
    T.LOE1 loe -> T.LOE1 <$> PTC.preprocess loe
    T.LFE1 lfe -> T.LFE1 <$> PTC.preprocess lfe

instance PTC.Preprocess T.Tuple where
  preprocess = \(T.T t) -> T.T <$> preprocess_pair t

instance PTC.Preprocess T.LineExprOrUnders where
  preprocess = \(T.LEOUs leous) -> T.LEOUs <$> preprocess_pair leous

instance PTC.Preprocess T.LineExprOrUnder where
  preprocess = \case
    T.LE1 le -> T.LE1 <$> PTC.preprocess le
    T.Underscore1 -> P.return T.Underscore1

instance PTC.Preprocess T.LineExpr where
  preprocess = \case
    T.BOAE1 boae -> T.BOAE1 <$> PTC.preprocess boae
    T.LOE2 loe -> T.LOE2 <$> PTC.preprocess loe
    T.LFE2 lfe -> T.LFE2 <$> PTC.preprocess lfe

instance PTC.Preprocess T.BasicOrAppExpr where
  preprocess = \case
    T.BE3 be ->
      PTC.to_maybe_post_func_app be >>= \case
        P.Nothing -> T.BE3 <$> PTC.preprocess be
        P.Just pfapp -> P.return $ T.PoFA1 pfapp
    T.PrFA1 prfa -> T.PrFA1 <$> PTC.preprocess prfa
    T.PoFA1 pofa -> T.PoFA1 <$> PTC.preprocess pofa

instance PTC.Preprocess T.BasicExpr where
  preprocess = \case
    T.PFAOI1 pfaoi -> T.PFAOI1 <$> PTC.preprocess pfaoi
    T.T1 t -> T.T1 <$> PTC.preprocess t
    T.L1 l -> T.L1 <$> PTC.preprocess l
    other -> P.return other

instance PTC.Preprocess T.BigTuple where
  preprocess = \(T.BT (leou, bts, leous, leous_l)) ->
    preprocess_triple (leou, leous, leous_l) >$>
    \(leou', leous', leous_l') -> T.BT (leou', bts, leous', leous_l')

deriving instance PTC.Preprocess T.List

instance PTC.Preprocess T.BigList where
  preprocess = \(T.BL bl) -> T.BL <$> preprocess_pair bl

instance PTC.Preprocess T.ArgsStr where
  preprocess = preprocess_first

instance PTC.Preprocess T.ParenFuncAppOrId where
  preprocess =
    \pfaoi@(T.PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
    check_if_pfaoi_is_sid pfaoi &> \case
      P.Just sid -> PTC.preprocess sid >$> sid_to_pfaoi
      P.Nothing ->
        preprocess_triple (margs1, args_str_pairs, margs2) >$>
        \(margs1', args_str_pairs', margs2') ->
        T.PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

deriving instance PTC.Preprocess T.Arguments

instance PTC.Preprocess T.PreFuncApp where
  preprocess = \(T.PrFA prfa) -> T.PrFA <$> preprocess_second prfa

instance PTC.Preprocess T.PostFuncApp where
  preprocess = \(T.PoFA (pfarg, pfae)) ->
    PTC.preprocess pfarg >>= \pfarg' ->
    push_post_func_arg pfarg >>
    PTC.preprocess pfae >>= \pfae' ->
    pop_post_func_arg >>
    P.return (T.PoFA (pfarg', pfae'))

instance PTC.Preprocess T.PostFuncArg where
  preprocess = \case
    T.PE2 pe -> T.PE2 <$> PTC.preprocess pe
    T.BE2 be -> T.BE2 <$> PTC.preprocess be
    T.Underscore2 -> P.return T.Underscore2

instance PTC.Preprocess T.PostFuncAppEnd where
  preprocess = \case
    T.DC1 dc -> T.DC1 <$> PTC.preprocess dc
    T.PFsMDC pfs_mdc -> T.PFsMDC <$> preprocess_second pfs_mdc

instance PTC.Preprocess T.DotChange where
  preprocess = \(T.DC dc) -> T.DC <$> preprocess_pair dc

instance PTC.Preprocess T.FieldChange where
  preprocess = \(T.FC fc) -> T.FC <$> preprocess_second fc

instance PTC.Preprocess T.OpExpr where
  preprocess = \case
    T.LOE3 loe -> T.LOE3 <$> PTC.preprocess loe
    T.BOE1 boe -> T.BOE1 <$> PTC.preprocess boe

deriving instance PTC.Preprocess T.OpExprStart

instance PTC.Preprocess (T.Operand, T.Op) where
  preprocess = preprocess_first

instance PTC.Preprocess T.LineOpExpr where
  preprocess = \(T.LOE loe) -> T.LOE <$> preprocess_pair loe

instance PTC.Preprocess T.LineOpExprEnd where
  preprocess = \case
    T.O1 o -> T.O1 <$> PTC.preprocess o
    T.LFE3 lfe -> T.LFE3 <$> PTC.preprocess lfe

instance PTC.Preprocess T.BigOpExpr where
  preprocess = \case
    T.BOEOS1 boeos -> T.BOEOS1 <$> PTC.preprocess boeos
    T.BOEFS1 boefs -> T.BOEFS1 <$> PTC.preprocess boefs

instance PTC.Preprocess T.BigOpExprOpSplit where
  preprocess = \(T.BOEOS boeos) -> T.BOEOS <$> preprocess_triple boeos

instance PTC.Preprocess T.OpSplitLine where
  preprocess = \case
    T.OESMOFCO oesmofco -> T.OESMOFCO <$> preprocess_pair oesmofco
    T.OFCO1 ofco -> T.OFCO1 <$> PTC.preprocess ofco

instance PTC.Preprocess T.OperFCO where
  preprocess = \(T.OFCO oper_fco) -> T.OFCO <$> preprocess_first oper_fco

instance PTC.Preprocess T.OpSplitEnd where
  preprocess = \case
    T.O2 o -> T.O2 <$> PTC.preprocess o
    T.FE1 fe -> T.FE1 <$> PTC.preprocess fe

instance PTC.Preprocess T.BigOpExprFuncSplit where
  preprocess = \(T.BOEFS boefs) -> T.BOEFS <$> preprocess_pair boefs

instance PTC.Preprocess T.BigOrCasesFuncExpr where
  preprocess = \case
    T.BFE1 bfe -> T.BFE1 <$> PTC.preprocess bfe
    T.CFE1 cfe -> T.CFE1 <$> PTC.preprocess cfe

instance PTC.Preprocess T.Operand where
  preprocess = \case
    T.BOAE2 boae -> T.BOAE2 <$> PTC.preprocess boae
    T.PE3 pe -> T.PE3 <$> PTC.preprocess pe
    T.Underscore3 -> P.return T.Underscore3

instance PTC.Preprocess T.FuncExpr where
  preprocess = \case
    T.LFE4 lfe -> T.LFE4 <$> PTC.preprocess lfe
    T.BFE2 bfe -> T.BFE2 <$> PTC.preprocess bfe
    T.CFE2 cfe -> T.CFE2 <$> PTC.preprocess cfe

instance PTC.Preprocess T.LineFuncExpr where
  preprocess = \(T.LFE lfe) -> T.LFE <$> preprocess_second lfe

instance PTC.Preprocess T.BigFuncExpr where
  preprocess = \(T.BFE bfe) -> T.BFE <$> preprocess_second bfe

instance PTC.Preprocess T.LineFuncBody where
  preprocess = \case
    T.BOAE3 boae -> T.BOAE3 <$> PTC.preprocess boae
    T.LOE4 loe -> T.LOE4 <$> PTC.preprocess loe
    T.PLFE1 plfe -> T.PLFE1 <$> PTC.preprocess plfe

instance PTC.Preprocess T.ParenLineFuncExpr where
  preprocess = \(T.PLFE lfe) -> T.PLFE <$> PTC.preprocess lfe

instance PTC.Preprocess T.BigFuncBody where
  preprocess = \case
    T.BOAE4 boae -> T.BOAE4 <$> PTC.preprocess boae
    T.OE1 oe -> T.OE1 <$> PTC.preprocess oe
    T.PLFE2 plfe -> T.PLFE2 <$> PTC.preprocess plfe

instance PTC.Preprocess T.CasesFuncExpr where
  preprocess = \(T.CFE (cparams, cases, mec)) ->
    preprocess_pair (cases, mec) >$> \(cases', mec') ->
    T.CFE (cparams, cases', mec')

instance PTC.Preprocess T.Case where
  preprocess = \(T.Ca om_cb) -> T.Ca <$> preprocess_pair om_cb

instance PTC.Preprocess T.EndCase where
  preprocess = \(T.EC ecp_cb) -> T.EC <$> preprocess_second ecp_cb

instance PTC.Preprocess T.OuterMatching where
  preprocess = \case
    T.SId2 sid ->
      lookup_sid_in_ovm sid >>= \case
        P.Just id -> P.return $ T.M1 $ T.PFM (T.PF sid, T.Id3 id)
        P.Nothing -> T.SId2 <$> PTC.preprocess sid
    T.M1 m -> T.M1 <$> PTC.preprocess m

instance PTC.Preprocess T.Matching where
  preprocess = \case
    T.PFM pfm -> T.PFM <$> preprocess_second pfm
    T.TM1 tm -> T.TM1 <$> PTC.preprocess tm
    T.LM1 lm -> T.LM1 <$> PTC.preprocess lm
    lit -> P.return lit

instance PTC.Preprocess T.InnerMatching where
  preprocess = \case
    T.Star -> P.return T.Star
    T.Id3 id -> T.Id3 <$> PTC.preprocess id
    T.M2 m -> T.M2 <$> PTC.preprocess m

instance PTC.Preprocess T.TupleMatching where
  preprocess = \(T.TM ims) -> T.TM <$> preprocess_pair ims

deriving instance PTC.Preprocess T.ListMatching

instance
  PTC.Preprocess (T.InnerMatching, [T.InnerMatching], P.Maybe T.RestListMatching)
  where
  preprocess = \(im, ims, mrlm) ->
    preprocess_pair (im, ims) >$> \(im', ims') -> (im', ims', mrlm)

instance PTC.Preprocess T.CaseBody where
  preprocess = \case
    T.LFB1 lfb -> T.LFB1 <$> PTC.preprocess lfb
    T.BFB1 bfb -> T.BFB1 <$> preprocess_pair bfb

instance PTC.Preprocess T.ValueDef where
  preprocess = \(T.VD (id, t, mve)) ->
    PTC.preprocess mve >$> \mve' -> T.VD (id, t, mve')

instance PTC.Preprocess T.ValueEquals where
  preprocess = \(T.VE ve) -> T.VE <$> preprocess_pair ve

instance PTC.Preprocess T.ValueExpr where
  preprocess = \case
    T.BOAE5 boae -> T.BOAE5 <$> PTC.preprocess boae
    T.OE2 oe -> T.OE2 <$> PTC.preprocess oe
    T.FE2 fe -> T.FE2 <$> PTC.preprocess fe
    T.BT1 bt -> T.BT1 <$> PTC.preprocess bt
    T.BL1 bl -> T.BL1 <$> PTC.preprocess bl

instance PTC.Preprocess T.GroupedValueDefs where
  preprocess = \(T.GVDs (ids, ts, les, les_l)) ->
    preprocess_pair (les, les_l) >$> \(les', les_l') ->
    T.GVDs (ids, ts, les', les_l')

instance PTC.Preprocess T.LineExprs where
  preprocess = \(T.LEs les) -> T.LEs <$> preprocess_pair les

instance PTC.Preprocess T.WhereExpr where
  preprocess = \(T.WE we) -> T.WE <$> preprocess_pair we

instance PTC.Preprocess T.ValueDefOrDefs where
  preprocess = \case
    T.VD1 vd -> T.VD1 <$> PTC.preprocess vd
    T.GVDs1 gvds -> T.GVDs1 <$> PTC.preprocess gvds

instance PTC.Preprocess T.TypeTheo where
  preprocess (T.TT (pnws_l, mpnws, proof)) = case pnws_l of
    [pnws] ->
      preprocess_pnws pnws >>= \pnws_l' ->
      PTC.preprocess proof >>= \proof' ->
      P.return $ T.TT (pnws_l', mpnws, proof')
    _ ->
      P.error "Should be impossible: many pnws at type theo before preprocessing"

preprocess_pnws :: T.PropNameWithSubs -> PTC.PreprocessState [T.PropNameWithSubs]
preprocess_pnws pnws =
  get_rps >$> P.map rp_compat >$> keep_compat >$> \case
    [] -> [pnws]
    [(PTC.Compatible m, pns)] -> P.map (add_subs_with_map m) pns
    _ -> P.error "proprocess pnws: more than one rps compatible"
  where
  rp_compat :: PTC.RenamingProp -> PTC.CompatAndPNs
  rp_compat = \rp -> (PTC.check_compat(P.fst rp, pnws), P.snd rp)

  keep_compat :: [PTC.CompatAndPNs] -> [PTC.CompatAndPNs]
  keep_compat = P.filter (P.fst .> (/= PTC.NotCompatible))

  add_subs_with_map :: PTC.AHTVMap -> T.PropName -> T.PropNameWithSubs
  add_subs_with_map = \m pn -> MS.evalState (PTC.add_subs pn) m

instance PTC.Preprocess T.Proof where
  preprocess = \case
    T.P1 iooe_le -> T.P1 <$> preprocess_second iooe_le
    T.P2 iooe_ttve -> T.P2 <$> preprocess_second iooe_ttve

instance PTC.Preprocess T.TTValueExpr where
  preprocess = \case
    T.LE2 le -> T.LE2 <$> PTC.preprocess le
    T.VEMWE vemwe -> T.VEMWE <$> preprocess_pair vemwe

instance PTC.Preprocess T.Program where
  preprocess = \(T.P pps) -> T.P <$> preprocess_pair pps

instance PTC.Preprocess T.ProgramPart where
  preprocess = \case
    T.VDD vdd -> T.VDD <$> PTC.preprocess vdd
    T.TT1 tt -> T.TT1 <$> PTC.preprocess tt
    other -> P.return other

-- ToMaybePostFuncApp instances

instance PTC.ToMaybePostFuncApp T.BasicExpr where
  to_maybe_post_func_app = \case
    T.PFAOI1 pfaoi -> PTC.to_maybe_post_func_app pfaoi
    T.SI1 spid -> PTC.to_maybe_post_func_app spid
    _ -> P.return P.Nothing

instance PTC.ToMaybePostFuncApp T.ParenFuncAppOrId where
  to_maybe_post_func_app =
    check_if_pfaoi_is_sid .> \case
      P.Nothing -> P.return P.Nothing
      P.Just sid -> PTC.to_maybe_post_func_app sid

instance PTC.ToMaybePostFuncApp T.SpecialId where
  to_maybe_post_func_app = \spid ->
    PTC.to_maybe_post_func_app $ T.PoF $ T.SI2 spid

instance PTC.ToMaybePostFuncApp T.SimpleId where
  to_maybe_post_func_app = \sid ->
    check_if_sid_in_fids sid >>= \case
      P.True -> PTC.to_maybe_post_func_app $ T.PoF $ T.SId1 sid
      _ -> P.return P.Nothing

instance PTC.ToMaybePostFuncApp T.PostFunc where
  to_maybe_post_func_app = \pf ->
    get_pfarg_if_in_dot_change >$>
    P.fmap (\pfarg -> pfarg_pf_to_pfapp (pfarg, pf))

-- State helpers
--   initial state

init_state :: T.Program -> PTC.StateTuple
init_state = \prog ->
  PC.or_values prog &> \(empty_or_values, full_or_values_map) ->
  ( PTC.NotInDotChange, PC.field_ids prog, empty_or_values
  , PC.renaming_props prog, full_or_values_map
  )

--   individual getters

get_pidc :: PTC.PreprocessState PTC.PossiblyInDC
get_pidc = MS.get >$> \(pidc, _, _, _, _) -> pidc

get_fids :: PTC.PreprocessState PTC.FieldIds
get_fids = MS.get >$> \(_, fids, _, _, _) -> fids

get_ncs :: PTC.PreprocessState PTC.EmptyOrValues
get_ncs = MS.get >$> \(_, _, ncs, _, _) -> ncs

get_rps :: PTC.PreprocessState PTC.RenamingProps
get_rps = MS.get >$> \(_, _, _, rps, _) -> rps

get_ovm :: PTC.PreprocessState PTC.FullOrValuesMap
get_ovm = MS.get >$> \(_, _, _, _, ovm) -> ovm

--   checking membership and lookup

check_if_sid_in_fids :: T.SimpleId -> PTC.PreprocessState P.Bool
check_if_sid_in_fids = \id -> S.member id <$> get_fids

check_if_sid_in_ncs :: T.SimpleId -> PTC.PreprocessState P.Bool
check_if_sid_in_ncs = \sid -> S.member sid <$> get_ncs

lookup_sid_in_ovm :: T.SimpleId -> PTC.PreprocessState (P.Maybe T.Identifier)
lookup_sid_in_ovm = \sid -> M.lookup sid <$> get_ovm

--   post func arg
--     push/pop

push_post_func_arg :: T.PostFuncArg -> PTC.PreprocessState ()
push_post_func_arg = \pfarg ->
  get_pidc >>= \case
    PTC.NotInDotChange -> in_dot_change_with_new_args [pfarg]
    PTC.InDotChange pfargs -> in_dot_change_with_new_args $ pfarg : pfargs

pop_post_func_arg :: PTC.PreprocessState ()
pop_post_func_arg =
  get_pidc >>= \case
    PTC.InDotChange [pfarg] -> change_pidc PTC.NotInDotChange
    PTC.InDotChange (pfarg : pfargs) -> in_dot_change_with_new_args pfargs
    _ -> P.error "should not be possible"

change_pidc :: PTC.PossiblyInDC -> PTC.PreprocessState ()
change_pidc = \pidc ->
  MS.modify (\(_, fids, ncs, rps, ovm) -> (pidc, fids, ncs, rps, ovm))

in_dot_change_with_new_args :: [T.PostFuncArg] -> PTC.PreprocessState ()
in_dot_change_with_new_args = \pfargs -> change_pidc $ PTC.InDotChange pfargs

--     get if in dot change

get_pfarg_if_in_dot_change :: PTC.PreprocessState (P.Maybe T.PostFuncArg)
get_pfarg_if_in_dot_change =
  get_pidc >$> \case
    PTC.NotInDotChange -> P.Nothing
    PTC.InDotChange [] -> P.error "should not be possible"
    PTC.InDotChange (pfarg : pfargs) -> P.Just pfarg

-- other helpers

add_constructor_prefix :: T.SimpleId -> T.SimpleId
add_constructor_prefix = \(T.SId (T.IS str, mdigit)) ->
  T.SId (T.IS $ GPH.constructor_prefix ++ str, mdigit)

change_if_particular_sid :: T.SimpleId -> T.SimpleId
change_if_particular_sid = \case
  T.SId (T.IS str, P.Nothing) -> str &> change_if_particular &> GH.str_to_sid
  sid -> sid

change_if_particular :: P.String -> P.String
change_if_particular = \case
  "true" -> GPH.true
  "false" -> GPH.false
  "no_value" -> GPH.pnothing
  "undefined" -> GPH.pundefined
  "pi" -> GPH.ppi
  str -> str

pfarg_pf_to_pfapp :: (T.PostFuncArg, T.PostFunc) -> T.PostFuncApp
pfarg_pf_to_pfapp = \(pfarg, pf) ->
  T.PoFA (change_pfarg_if_under pfarg, T.PFsMDC ([pf], P.Nothing))

change_pfarg_if_under :: T.PostFuncArg -> T.PostFuncArg
change_pfarg_if_under = \case
  T.Underscore2 ->
    T.BE2 $ T.PFAOI1 $
      sid_to_pfaoi (T.SId (T.IS GPH.under_pfarg_param, P.Nothing))
  other -> other

check_if_pfaoi_is_sid :: T.ParenFuncAppOrId -> P.Maybe T.SimpleId
check_if_pfaoi_is_sid = \case
  T.PFAOI (P.Nothing, id_strt, [], mdigit, P.Nothing) ->
    P.Just $ T.SId (id_strt, mdigit)
  _ -> P.Nothing

sid_to_pfaoi :: T.SimpleId -> T.ParenFuncAppOrId
sid_to_pfaoi = \(T.SId (id_start, mdigit)) ->
  T.PFAOI (P.Nothing, id_start, [], mdigit, P.Nothing)

{-
For fast vim file navigation:
CheckCompatibility.hs
Collect.hs
Preprocess.hs
TypesAndClasses.hs
-}
