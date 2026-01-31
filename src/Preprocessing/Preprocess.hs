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
import Preprocessing.Collect qualified as PC
import Preprocessing.CheckCompatibility qualified as PCC
import Generation.TypesAndHelpers qualified as GTH
import Generation.PrefixesAndHardcoded qualified as GPH

-- types

data PossiblyInDC =
  InDotChange [T.PostFuncArg] | NotInDotChange

type StateTuple =
  ( PossiblyInDC, PC.FieldIds, PC.EmptyOrValues, PC.RenamingProps
  , PC.FullOrValuesMap
  )

type PreprocessState = MS.State StateTuple

-- Preprocess class, general preprocess functions, automatic instances
--   and instances for pairs and triples

class Preprocess a where
  preprocess :: a -> PreprocessState a

preprocess_prog :: T.Program -> T.Program
preprocess_prog = \prog -> MS.evalState (preprocess prog) (init_state prog)

instance Preprocess a => Preprocess [a] where
  preprocess = P.traverse preprocess

instance Preprocess a => Preprocess (P.Maybe a) where
  preprocess = P.traverse preprocess

preprocess_pair
  :: (Preprocess a, Preprocess b) => (a, b) -> PreprocessState (a, b)
preprocess_pair = \(a, b) -> preprocess a ++< preprocess b

preprocess_first :: Preprocess a => (a, b) -> PreprocessState (a, b)
preprocess_first = \(a, b) -> preprocess a >$> \a' -> (a', b)

preprocess_second :: Preprocess b => (a, b) -> PreprocessState (a, b)
preprocess_second = \(a, b) -> preprocess b >$> \b' -> (a, b')

preprocess_triple
  :: (Preprocess a, Preprocess b, Preprocess c) =>
     (a, b, c) -> PreprocessState (a, b, c)
preprocess_triple = \(a, b, c) ->
  preprocess a ++< preprocess b +++< preprocess c

-- regular Preprocess instances

instance Preprocess T.Identifier where
  preprocess = \id ->
    GTH.check_if_id_is_sid id &> \case
      P.Nothing -> P.return id
      P.Just sid -> preprocess sid >$> GTH.sid_to_id

instance Preprocess T.SimpleId where
  preprocess = \sid ->
    check_if_sid_in_ncs sid >$> \case
      P.True -> add_constructor_prefix sid
      P.False -> change_if_particular_sid sid

deriving instance Preprocess T.ParenExpr

instance Preprocess T.InsideParenExpr where
  preprocess = \case
    T.LOE1 loe -> T.LOE1 <$> preprocess loe
    T.LFE1 lfe -> T.LFE1 <$> preprocess lfe

instance Preprocess T.Tuple where
  preprocess = \(T.T t) -> T.T <$> preprocess_pair t

instance Preprocess T.LineExprOrUnders where
  preprocess = \(T.LEOUs leous) -> T.LEOUs <$> preprocess_pair leous

instance Preprocess T.LineExprOrUnder where
  preprocess = \case
    T.LE1 le -> T.LE1 <$> preprocess le
    T.Underscore1 -> P.return T.Underscore1

instance Preprocess T.LineExpr where
  preprocess = \case
    T.BOAE1 boae -> T.BOAE1 <$> preprocess boae
    T.LOE2 loe -> T.LOE2 <$> preprocess loe
    T.LFE2 lfe -> T.LFE2 <$> preprocess lfe

instance Preprocess T.BasicOrAppExpr where
  preprocess = \case
    T.BE3 be ->
      to_maybe_post_func_app be >>= \case
        P.Nothing -> T.BE3 <$> preprocess be
        P.Just pfapp -> P.return $ T.PoFA1 pfapp
    T.PrFA1 prfa -> T.PrFA1 <$> preprocess prfa
    T.PoFA1 pofa -> T.PoFA1 <$> preprocess pofa

instance Preprocess T.BasicExpr where
  preprocess = \case
    T.PFAOI1 pfaoi -> T.PFAOI1 <$> preprocess pfaoi
    T.T1 t -> T.T1 <$> preprocess t
    T.L1 l -> T.L1 <$> preprocess l
    other -> P.return other

instance Preprocess T.BigTuple where
  preprocess = \(T.BT (leou, bts, leous, leous_l)) ->
    preprocess_triple (leou, leous, leous_l) >$>
    \(leou', leous', leous_l') -> T.BT (leou', bts, leous', leous_l')

deriving instance Preprocess T.List

instance Preprocess T.BigList where
  preprocess = \(T.BL bl) -> T.BL <$> preprocess_pair bl

instance Preprocess T.ArgsStr where
  preprocess = preprocess_first

instance Preprocess T.ParenFuncAppOrId where
  preprocess =
    \pfaoi@(T.PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
    GTH.check_if_pfaoi_is_sid pfaoi &> \case
      P.Just sid -> preprocess sid >$> GTH.sid_to_pfaoi
      P.Nothing ->
        preprocess_triple (margs1, args_str_pairs, margs2) >$>
        \(margs1', args_str_pairs', margs2') ->
        T.PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

deriving instance Preprocess T.Arguments

instance Preprocess T.PreFuncApp where
  preprocess = \(T.PrFA prfa) -> T.PrFA <$> preprocess_second prfa

instance Preprocess T.PostFuncApp where
  preprocess = \(T.PoFA (pfarg, pfae)) ->
    preprocess pfarg >>= \pfarg' ->
    push_post_func_arg pfarg >>
    preprocess pfae >>= \pfae' ->
    pop_post_func_arg >>
    P.return (T.PoFA (pfarg', pfae'))

instance Preprocess T.PostFuncArg where
  preprocess = \case
    T.PE2 pe -> T.PE2 <$> preprocess pe
    T.BE2 be -> T.BE2 <$> preprocess be
    T.Underscore2 -> P.return T.Underscore2

instance Preprocess T.PostFuncAppEnd where
  preprocess = \case
    T.DC1 dc -> T.DC1 <$> preprocess dc
    T.PFsMDC pfs_mdc -> T.PFsMDC <$> preprocess_second pfs_mdc

instance Preprocess T.DotChange where
  preprocess = \(T.DC dc) -> T.DC <$> preprocess_pair dc

instance Preprocess T.FieldChange where
  preprocess = \(T.FC fc) -> T.FC <$> preprocess_second fc

instance Preprocess T.OpExpr where
  preprocess = \case
    T.LOE3 loe -> T.LOE3 <$> preprocess loe
    T.BOE1 boe -> T.BOE1 <$> preprocess boe

deriving instance Preprocess T.OpExprStart

instance Preprocess (T.Operand, T.Op) where
  preprocess = preprocess_first

instance Preprocess T.LineOpExpr where
  preprocess = \(T.LOE loe) -> T.LOE <$> preprocess_pair loe

instance Preprocess T.LineOpExprEnd where
  preprocess = \case
    T.O1 o -> T.O1 <$> preprocess o
    T.LFE3 lfe -> T.LFE3 <$> preprocess lfe

instance Preprocess T.BigOpExpr where
  preprocess = \case
    T.BOEOS1 boeos -> T.BOEOS1 <$> preprocess boeos
    T.BOEFS1 boefs -> T.BOEFS1 <$> preprocess boefs

instance Preprocess T.BigOpExprOpSplit where
  preprocess = \(T.BOEOS boeos) -> T.BOEOS <$> preprocess_triple boeos

instance Preprocess T.OpSplitLine where
  preprocess = \case
    T.OESMOFCO oesmofco -> T.OESMOFCO <$> preprocess_pair oesmofco
    T.OFCO1 ofco -> T.OFCO1 <$> preprocess ofco

instance Preprocess T.OperFCO where
  preprocess = \(T.OFCO oper_fco) -> T.OFCO <$> preprocess_first oper_fco

instance Preprocess T.OpSplitEnd where
  preprocess = \case
    T.O2 o -> T.O2 <$> preprocess o
    T.FE1 fe -> T.FE1 <$> preprocess fe

instance Preprocess T.BigOpExprFuncSplit where
  preprocess = \(T.BOEFS boefs) -> T.BOEFS <$> preprocess_pair boefs

instance Preprocess T.BigOrCasesFuncExpr where
  preprocess = \case
    T.BFE1 bfe -> T.BFE1 <$> preprocess bfe
    T.CFE1 cfe -> T.CFE1 <$> preprocess cfe

instance Preprocess T.Operand where
  preprocess = \case
    T.BOAE2 boae -> T.BOAE2 <$> preprocess boae
    T.PE3 pe -> T.PE3 <$> preprocess pe
    T.Underscore3 -> P.return T.Underscore3

instance Preprocess T.FuncExpr where
  preprocess = \case
    T.LFE4 lfe -> T.LFE4 <$> preprocess lfe
    T.BFE2 bfe -> T.BFE2 <$> preprocess bfe
    T.CFE2 cfe -> T.CFE2 <$> preprocess cfe

instance Preprocess T.LineFuncExpr where
  preprocess = \(T.LFE lfe) -> T.LFE <$> preprocess_second lfe

instance Preprocess T.BigFuncExpr where
  preprocess = \(T.BFE bfe) -> T.BFE <$> preprocess_second bfe

instance Preprocess T.LineFuncBody where
  preprocess = \case
    T.BOAE3 boae -> T.BOAE3 <$> preprocess boae
    T.LOE4 loe -> T.LOE4 <$> preprocess loe
    T.LFE5 lfe -> T.LFE5 <$> preprocess lfe

instance Preprocess T.BigFuncBody where
  preprocess = \case
    T.BOAE4 boae -> T.BOAE4 <$> preprocess boae
    T.OE1 oe -> T.OE1 <$> preprocess oe
    T.LFE6 lfe -> T.LFE6 <$> preprocess lfe

instance Preprocess T.CasesFuncExpr where
  preprocess = \(T.CFE (cparams, cases, mec)) ->
    preprocess_pair (cases, mec) >$> \(cases', mec') ->
    T.CFE (cparams, cases', mec')

instance Preprocess T.Case where
  preprocess = \(T.Ca om_cb) -> T.Ca <$> preprocess_pair om_cb

instance Preprocess T.EndCase where
  preprocess = \(T.EC ecp_cb) -> T.EC <$> preprocess_second ecp_cb

instance Preprocess T.OuterMatching where
  preprocess = \case
    T.SId3 sid ->
      lookup_sid_in_ovm sid >>= \case
        P.Just id -> P.return $ T.M1 $ T.PFM (T.PF sid, T.Id2 id)
        P.Nothing -> T.SId3 <$> preprocess sid
    T.M1 m -> T.M1 <$> preprocess m

instance Preprocess T.Matching where
  preprocess = \case
    T.PFM pfm -> T.PFM <$> preprocess_second pfm
    T.TM1 tm -> T.TM1 <$> preprocess tm
    T.LM1 lm -> T.LM1 <$> preprocess lm
    lit -> P.return lit

instance Preprocess T.InnerMatching where
  preprocess = \case
    T.Star -> P.return T.Star
    T.Id2 id -> T.Id2 <$> preprocess id
    T.M2 m -> T.M2 <$> preprocess m

instance Preprocess T.TupleMatching where
  preprocess = \(T.TM ims) -> T.TM <$> preprocess_pair ims

deriving instance Preprocess T.ListMatching

instance
  Preprocess (T.InnerMatching, [T.InnerMatching], P.Maybe T.RestListMatching)
  where
  preprocess = \(im, ims, mrlm) ->
    preprocess_pair (im, ims) >$> \(im', ims') -> (im', ims', mrlm)

instance Preprocess T.CaseBody where
  preprocess = \case
    T.LFB1 lfb -> T.LFB1 <$> preprocess lfb
    T.BFB1 bfb -> T.BFB1 <$> preprocess_pair bfb

instance Preprocess T.ValueDef where
  preprocess = \(T.VD (id, t, ve, mwe)) ->
    preprocess_pair (ve, mwe) >$> \(ve', mwe') -> T.VD (id, t, ve', mwe')

instance Preprocess T.ValueExpr where
  preprocess = \case
    T.BOAE5 boae -> T.BOAE5 <$> preprocess boae
    T.OE2 oe -> T.OE2 <$> preprocess oe
    T.FE2 fe -> T.FE2 <$> preprocess fe
    T.BT1 bt -> T.BT1 <$> preprocess bt
    T.BL1 bl -> T.BL1 <$> preprocess bl

instance Preprocess T.GroupedValueDefs where
  preprocess = \(T.GVDs (id, ids, ts, les, les_l)) ->
    preprocess_pair (les, les_l) >$> \(les', les_l') ->
    T.GVDs (id, ids, ts, les', les_l')

instance Preprocess T.LineExprs where
  preprocess = \(T.LEs les) -> T.LEs <$> preprocess_pair les

instance Preprocess T.WhereExpr where
  preprocess = \(T.WE we) -> T.WE <$> preprocess_pair we

instance Preprocess T.WhereDefExpr where
  preprocess = \case
    T.VD1 vd -> T.VD1 <$> preprocess vd
    T.GVDs1 gvds -> T.GVDs1 <$> preprocess gvds

instance Preprocess T.TypeTheo where
  preprocess (T.TT (pnws_l, mpnws, proof)) = case pnws_l of
    [pnws] ->
      preprocess_pnws pnws >>= \pnws_l' ->
      preprocess proof >>= \proof' ->
      P.return $ T.TT (pnws_l', mpnws, proof')
    _ ->
      P.error "Should be impossible: many pnws at type theo before preprocessing"


type CompatAndPNs = (PCC.Compatibility, [T.PropName])

preprocess_pnws :: T.PropNameWithSubs -> PreprocessState [T.PropNameWithSubs]
preprocess_pnws pnws =
  get_rps >$> P.map rp_compat >$> keep_compat >$> \case
    [] -> [pnws]
    [(PCC.Compatible m, pns)] -> P.map (add_subs_with_map m) pns
    _ -> P.error "proprocess pnws: more than one rps compatible"
  where
  rp_compat :: PC.RenamingProp -> CompatAndPNs
  rp_compat = \rp -> (PCC.check_compat(P.fst rp, pnws), P.snd rp)

  keep_compat :: [CompatAndPNs] -> [CompatAndPNs]
  keep_compat = P.filter (P.fst .> (/= PCC.NotCompatible))

  add_subs_with_map :: PCC.AHTVMap -> T.PropName -> T.PropNameWithSubs
  add_subs_with_map = \m pn -> MS.evalState (PCC.add_subs pn) m

instance Preprocess T.Proof where
  preprocess = \case
    T.P1 iooe_le -> T.P1 <$> preprocess_second iooe_le
    T.P2 iooe_ttve -> T.P2 <$> preprocess_second iooe_ttve

instance Preprocess T.TTValueExpr where
  preprocess = \case
    T.LE2 le -> T.LE2 <$> preprocess le
    T.VEMWE vemwe -> T.VEMWE <$> preprocess_pair vemwe

instance Preprocess T.Program where
  preprocess = \(T.P pps) -> T.P <$> preprocess_pair pps

instance Preprocess T.ProgramPart where
  preprocess = \case
    T.VD2 vd -> T.VD2 <$> preprocess vd
    T.GVDs2 gvds -> T.GVDs2 <$> preprocess gvds
    T.TT1 tt -> T.TT1 <$> preprocess tt
    other -> P.return other

-- ToMaybePostFuncApp class and instances

class ToMaybePostFuncApp a where
  to_maybe_post_func_app :: a -> PreprocessState (P.Maybe T.PostFuncApp)

instance ToMaybePostFuncApp T.BasicExpr where
  to_maybe_post_func_app = \case
    T.PFAOI1 pfaoi -> to_maybe_post_func_app pfaoi
    T.SI1 spid -> to_maybe_post_func_app spid
    _ -> P.return P.Nothing

instance ToMaybePostFuncApp T.ParenFuncAppOrId where
  to_maybe_post_func_app =
    GTH.check_if_pfaoi_is_sid .> \case
      P.Nothing -> P.return P.Nothing
      P.Just sid -> to_maybe_post_func_app sid

instance ToMaybePostFuncApp T.SpecialId where
  to_maybe_post_func_app = \spid -> to_maybe_post_func_app $ T.SI2 spid

instance ToMaybePostFuncApp T.SimpleId where
  to_maybe_post_func_app = \sid ->
    check_if_sid_in_fids sid >>= \case
      P.True -> to_maybe_post_func_app $ T.SId1 sid
      _ -> P.return P.Nothing

instance ToMaybePostFuncApp T.PostFunc where
  to_maybe_post_func_app = \pf ->
    get_pfarg_if_in_dot_change >$>
    P.fmap (\pfarg -> pfarg_pf_to_pfapp (pfarg, pf))

-- State helpers
--   initial state

init_state :: T.Program -> StateTuple
init_state = \prog ->
  PC.or_values prog &> \(empty_or_values, full_or_values_map) ->
  ( NotInDotChange, PC.field_ids prog, empty_or_values, PC.renaming_props prog
  , full_or_values_map
  )

--   individual getters

get_pidc :: PreprocessState PossiblyInDC
get_pidc = MS.get >$> \(pidc, _, _, _, _) -> pidc

get_fids :: PreprocessState PC.FieldIds
get_fids = MS.get >$> \(_, fids, _, _, _) -> fids

get_ncs :: PreprocessState PC.EmptyOrValues
get_ncs = MS.get >$> \(_, _, ncs, _, _) -> ncs

get_rps :: PreprocessState PC.RenamingProps
get_rps = MS.get >$> \(_, _, _, rps, _) -> rps

get_ovm :: PreprocessState PC.FullOrValuesMap
get_ovm = MS.get >$> \(_, _, _, _, ovm) -> ovm

--   checking membership and lookup

check_if_sid_in_fids :: T.SimpleId -> PreprocessState P.Bool
check_if_sid_in_fids = \sid -> S.member sid <$> get_fids

check_if_sid_in_ncs :: T.SimpleId -> PreprocessState P.Bool
check_if_sid_in_ncs = \sid -> S.member sid <$> get_ncs

lookup_sid_in_ovm :: T.SimpleId -> PreprocessState (P.Maybe T.Identifier)
lookup_sid_in_ovm = \sid -> M.lookup sid <$> get_ovm

--   post func arg
--     push/pop

push_post_func_arg :: T.PostFuncArg -> PreprocessState ()
push_post_func_arg = \pfarg ->
  get_pidc >>= \case
    NotInDotChange -> in_dot_change_with_new_args [pfarg]
    InDotChange pfargs -> in_dot_change_with_new_args $ pfarg : pfargs

pop_post_func_arg :: PreprocessState ()
pop_post_func_arg =
  get_pidc >>= \case
    InDotChange [pfarg] -> change_pidc NotInDotChange
    InDotChange (pfarg : pfargs) -> in_dot_change_with_new_args pfargs
    _ -> P.error "should not be possible"

change_pidc :: PossiblyInDC -> PreprocessState ()
change_pidc = \pidc ->
  MS.modify (\(_, fids, ncs, rps, ovm) -> (pidc, fids, ncs, rps, ovm))

in_dot_change_with_new_args :: [T.PostFuncArg] -> PreprocessState ()
in_dot_change_with_new_args = \pfargs -> change_pidc $ InDotChange pfargs

--     get if in dot change

get_pfarg_if_in_dot_change :: PreprocessState (P.Maybe T.PostFuncArg)
get_pfarg_if_in_dot_change =
  get_pidc >$> \case
    NotInDotChange -> P.Nothing
    InDotChange [] -> P.error "should not be possible"
    InDotChange (pfarg : pfargs) -> P.Just pfarg

-- other helpers

add_constructor_prefix :: T.SimpleId -> T.SimpleId
add_constructor_prefix = \(T.SId (T.IS str, mdigit)) ->
  T.SId (T.IS $ GPH.constructor_prefix ++ str, mdigit)

change_if_particular_sid :: T.SimpleId -> T.SimpleId
change_if_particular_sid = \case
  T.SId (T.IS str, P.Nothing) -> str &> change_if_particular &> GTH.str_to_sid
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
      GTH.sid_to_pfaoi (T.SId (T.IS GPH.under_pfarg_param, P.Nothing))
  other -> other

{-
For fast vim file navigation:
Collect.hs
CheckCompatibility.hs
AST.hs
-}
