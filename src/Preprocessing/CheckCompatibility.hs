{-
This file is imported by Preprocessing.Preprocess.  It contains the instances for
the CheckCompatibility and AddSubs classes.  The CheckCompatibility class
checks whether two types are compatible via a map from ad hoc type variables to
substitutions or underscores and if they are it creates the map.  The AddSubs
class converts a type to another by using the map created by
CheckCompatibility.
-}

{-# language
  LambdaCase, MultiParamTypeClasses, FunctionalDependencies,
  TypeSynonymInstances, FlexibleInstances, UndecidableInstances
#-}

module Preprocessing.CheckCompatibility where

import Prelude (($), (<$>), (>>=), (==), (&&))
import Prelude qualified as P
import Data.Map.Strict qualified as M
import Control.Monad.State qualified as MS

import ASTTypes qualified as T
import Helpers ((>$>), (.>))

import Generation.Helpers qualified as GH
import Preprocessing.TypesAndClasses qualified as PTC

-- types

instance P.Eq PTC.Compatibility where
  c1 == c2 = case (c1, c2) of
    (PTC.NotCompatible, PTC.NotCompatible) -> P.True
    _ -> P.False

-- CheckCompatibility class

instance
  PTC.CheckCompatibility a b => PTC.CheckCompatibility (P.Maybe a) (P.Maybe b)
  where
  check_compat = \case
    (P.Just a, P.Just b) -> PTC.check_compat(a, b)
    (P.Nothing, P.Nothing) -> PTC.Compatible M.empty
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility a b => PTC.CheckCompatibility [a] [b] where
  check_compat = error_zip .> P.map PTC.check_compat .> compat_list_union

instance PTC.CheckCompatibility T.PropName T.PropNameWithSubs where
  check_compat = \case
    (T.NPStart1 nps1, T.NPStart2 nps2) -> PTC.check_compat(nps1, nps2)
    (T.TIPStart tips, T.SIPStart sips) -> PTC.check_compat(tips, sips)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility T.NPStart1 T.NPStart2 where
  check_compat = \((c1, np_tip_pairs, mnp1), (c2, np_sip_pairs, mnp2)) ->
    case c1 == c2 && mnp1 == mnp2 of
      P.False -> PTC.NotCompatible
      P.True -> PTC.check_compat(np_tip_pairs, np_sip_pairs)

instance PTC.CheckCompatibility T.TIPStart T.SIPStart where
  check_compat ((tip_np_pairs, mtip), (sip_np_pairs, msip)) =
    compat_union pairs_compat mtip_msip_mcompat
    where
    pairs_compat :: PTC.Compatibility
    pairs_compat = PTC.check_compat(tip_np_pairs, sip_np_pairs)

    mtip_msip_mcompat :: PTC.Compatibility
    mtip_msip_mcompat = PTC.check_compat(mtip, msip)

instance PTC.CheckCompatibility T.TypesInParen T.SubsInParen where
  check_compat = \(T.TIP (st1, sts), T.SIP (tvs1,tvss)) ->
    PTC.check_compat(st1 : sts, tvs1 : tvss)

instance PTC.CheckCompatibility PTC.NP_TIP_Pair PTC.NP_SIP_Pair where
  check_compat = \((np1, tip), (np2, sip)) ->
    case np1 == np2 of
      P.False -> PTC.NotCompatible
      P.True -> PTC.check_compat(tip, sip)

instance PTC.CheckCompatibility PTC.TIP_NP_Pair PTC.SIP_NP_Pair where
  check_compat = \((tip, np1), (sip, np2)) ->
    case np1 == np2 of
      P.False -> PTC.NotCompatible
      P.True -> PTC.check_compat(tip, sip)

instance PTC.CheckCompatibility T.SimpleType T.TVarSub where
  check_compat = \case
    (T.TAIOA1 taioa, T.TAIOAS1 taioas) -> PTC.check_compat(taioa, taioas)
    (T.POPT1 popt, T.POPTS1 popts) -> PTC.check_compat(popt, popts)
    (T.FT1 ft, T.FTS1 fts) -> PTC.check_compat(ft, fts)

instance PTC.CheckCompatibility T.ProdOrPowerType T.ProdOrPowerTypeSub where
  check_compat = \case
    (T.PoT5 pt, T.PoTS1 pts) -> PTC.check_compat(pt, pts)
    (T.PT4 pt, T.PTS1 pts) -> PTC.check_compat(pt, pts)

instance PTC.CheckCompatibility T.TypeAppIdOrTV T.TypeAppIdOrTVSub where
  check_compat = \case
    (T.TAIOA (mtip1, taioam, mtip2), T.TAIOAS (msouip1, taioasm, msouip2)) ->
      compat_list_union
        [ PTC.check_compat(mtip1, msouip1)
        , PTC.check_compat(taioam, taioasm)
        , PTC.check_compat(mtip2, msouip2)
        ]
    (T.PTV1 _, T.PTV2 _) -> PTC.Compatible M.empty
    _ -> P.undefined

instance PTC.CheckCompatibility T.PowerType T.PowerTypeSub where
  check_compat = \(T.PoT (pbt, i), T.PoTS (pbts, j)) ->
    case i == j of
      P.False -> PTC.NotCompatible
      P.True -> PTC.check_compat(pbt, pbts)

instance PTC.CheckCompatibility T.ProdType T.ProdTypeSub where
  check_compat = \(T.PT (ft1, fts), T.PTS (fts1, ftss)) ->
    PTC.check_compat(ft1 : fts, fts1 : ftss)

instance PTC.CheckCompatibility T.FuncType T.FuncTypeSub where
  check_compat = \(T.FT (it, ot), T.FTS (its, ots)) ->
    compat_union (PTC.check_compat(it, its)) (PTC.check_compat(ot, ots))

instance PTC.CheckCompatibility T.TypesInParen T.SubsOrUndersInParen where
  check_compat = \(T.TIP (st1, sts), T.SOUIP (sou1, sous)) ->
    PTC.check_compat(st1 : sts, sou1 : sous)

instance PTC.CheckCompatibility T.TAIOAMiddle T.TAIOASMiddle where
  check_compat = \case
    (T.AHTV1 ahtv, taioasm) ->
      PTC.Compatible $ M.singleton ahtv $ GH.taioasm_to_sou taioasm
    (T.TIdStart1 (tid1, tip_str_pairs), T.TIdStart2 (tid2, souip_str_pairs)) ->
      case tid1 == tid2 of
        P.False -> PTC.NotCompatible
        P.True -> PTC.check_compat(tip_str_pairs, souip_str_pairs)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility T.PowerBaseType T.PowerBaseTypeSub where
  check_compat = \case
    (T.TAIOA2 taioa, T.TAIOAS2 taioas) -> PTC.check_compat(taioa, taioas)
    (T.IPT ipt, T.IPTS ipts) -> PTC.check_compat(ipt, ipts)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility T.FieldType T.FieldTypeSub where
  check_compat = \case
    (T.PBT1 pbt, T.PBTS1 pbts) -> PTC.check_compat(pbt, pbts)
    (T.PoT2 pt, T.PoTS2 pts) -> PTC.check_compat(pt, pts)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility T.InOrOutType T.InOrOutTypeSub where
  check_compat = \case
    (T.TAIOA3 taioa, T.TAIOAS3 taioas) -> PTC.check_compat(taioa, taioas)
    (T.POPT2 popt, T.POPTS2 popts) -> PTC.check_compat(popt, popts)
    (T.FT2 ft, T.FTS3 fts) -> PTC.check_compat(ft, fts)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility T.SimpleType T.SubOrUnder where
  check_compat = \case
    (T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV1 ahtv, P.Nothing)), sou) ->
      PTC.Compatible $ M.singleton ahtv sou
    (st, T.TVS1 tvs) -> PTC.check_compat(st, tvs)
    _ -> PTC.NotCompatible

instance PTC.CheckCompatibility PTC.TIP_STR PTC.SOUIP_STR where
  check_compat = \((tip, str1), (souip, str2)) ->
    case str1 == str2 of
      P.False -> PTC.NotCompatible
      P.True -> PTC.check_compat(tip, souip)

instance PTC.CheckCompatibility T.InParenT T.InParenTSub where
  check_compat = \case
    (T.PT3 pt, T.PTS2 pts) -> PTC.check_compat(pt, pts)
    (T.FT3 ft, T.FTS2 fts) -> PTC.check_compat(ft, fts)
    _ -> PTC.NotCompatible

-- AddSubs Class

add_subs_pair
  :: (PTC.AddSubs a b, PTC.AddSubs c d) => (a, c) -> PTC.WAHTVMap (b, d)
add_subs_pair = \(a, c) ->
  PTC.add_subs a >>= \b -> PTC.add_subs c >>= \d -> P.return (b, d)

add_subs_triple
  :: (PTC.AddSubs a b, PTC.AddSubs c d, PTC.AddSubs e f) =>
     (a, c, e) -> PTC.WAHTVMap (b, d, f)
add_subs_triple = \(a, c, e) ->
  PTC.add_subs a >>= \b -> PTC.add_subs c >>= \d -> PTC.add_subs e >>= \f ->
  P.return (b, d, f)

instance PTC.AddSubs a b => PTC.AddSubs (P.Maybe a) (P.Maybe b) where
  add_subs = P.traverse PTC.add_subs

instance PTC.AddSubs a b => PTC.AddSubs [a] [b] where
  add_subs = P.traverse PTC.add_subs

instance PTC.AddSubs T.PropName T.PropNameWithSubs where
  add_subs = \case
    T.NPStart1 nps1 -> T.NPStart2 <$> PTC.add_subs nps1
    T.TIPStart tips -> T.SIPStart <$> PTC.add_subs tips

instance PTC.AddSubs T.NPStart1 T.NPStart2 where
  add_subs = \(c, np_tip_pairs, mnp) ->
    PTC.add_subs np_tip_pairs >$> \np_sip_pairs -> (c, np_sip_pairs, mnp)

instance PTC.AddSubs T.TIPStart T.SIPStart where
  add_subs = \(tip_np_pairs, mtip) -> add_subs_pair(tip_np_pairs, mtip)

instance PTC.AddSubs PTC.NP_TIP_Pair PTC.NP_SIP_Pair where
  add_subs = \(np, tip) -> PTC.add_subs tip >$> \sip -> (np, sip)

instance PTC.AddSubs PTC.TIP_NP_Pair PTC.SIP_NP_Pair where
  add_subs = \(tip, np) -> PTC.add_subs tip >$> \sip -> (sip, np)

instance PTC.AddSubs T.TypesInParen T.SubsInParen where
  add_subs = \(T.TIP sts) -> T.SIP <$> add_subs_pair sts

instance PTC.AddSubs T.SimpleType T.TVarSub where
  add_subs = \case
    T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV1 ahtv, P.Nothing)) -> PTC.add_subs ahtv
    T.TAIOA1 taioa -> T.TAIOAS1 <$> PTC.add_subs taioa
    T.POPT1 popt -> T.POPTS1 <$> PTC.add_subs popt
    T.FT1 ft -> T.FTS1 <$> PTC.add_subs ft

instance PTC.AddSubs T.ProdOrPowerType T.ProdOrPowerTypeSub where
  add_subs = \case
    T.PT4 pt -> T.PTS1 <$> PTC.add_subs pt
    T.PoT5 pt -> T.PoTS1 <$> PTC.add_subs pt

instance PTC.AddSubs T.AdHocTVar T.TVarSub where
  add_subs = \ahtv ->
    MS.get >$> M.lookup ahtv >$> \case
      P.Just (T.TVS1 tvs) -> tvs
      P.Just T.Underscore4 -> P.error "PTC.add_subs st tvs: ahtv is underscore"
      P.Nothing -> P.error "PTC.add_subs st tvs: didn't find ahtv"

instance PTC.AddSubs T.TypeAppIdOrTV T.TypeAppIdOrTVSub where
  add_subs = \(T.TAIOA taioa) -> T.TAIOAS <$> add_subs_triple taioa

instance PTC.AddSubs T.PowerType T.PowerTypeSub where
  add_subs =
    \(T.PoT (pbt, i)) -> T.PoTS <$> (PTC.add_subs pbt >$> \pbts -> (pbts, i))

instance PTC.AddSubs T.ProdType T.ProdTypeSub where
  add_subs = \(T.PT fts) -> T.PTS <$> add_subs_pair fts

instance PTC.AddSubs T.FuncType T.FuncTypeSub where
  add_subs = \(T.FT it_ot) -> T.FTS <$> add_subs_pair it_ot

instance PTC.AddSubs T.TAIOAMiddle T.TAIOASMiddle where
  add_subs = \case
    T.TIdStart1 (tid, tip_str_pairs) ->
      PTC.add_subs tip_str_pairs >$> \souip_str_pairs ->
      T.TIdStart2 (tid, souip_str_pairs)
    T.AHTV1 ahtv -> P.undefined

instance PTC.AddSubs T.TypesInParen T.SubsOrUndersInParen where
  add_subs = \(T.TIP sts) -> T.SOUIP <$> add_subs_pair sts

instance PTC.AddSubs T.PowerBaseType T.PowerBaseTypeSub where
  add_subs = \case
    T.TAIOA2 taioa -> T.TAIOAS2 <$> PTC.add_subs taioa
    T.IPT ipt -> T.IPTS <$> PTC.add_subs ipt

instance PTC.AddSubs T.FieldType T.FieldTypeSub where
  add_subs = \case
    T.PBT1 pbt -> T.PBTS1 <$> PTC.add_subs pbt
    T.PoT2 pt -> T.PoTS2 <$> PTC.add_subs pt

instance PTC.AddSubs T.InOrOutType T.InOrOutTypeSub where
  add_subs = \case
    T.TAIOA3 taioa -> T.TAIOAS3 <$> PTC.add_subs taioa
    T.POPT2 popt -> T.POPTS2 <$> PTC.add_subs popt
    T.FT2 ft -> T.FTS3 <$> PTC.add_subs ft

instance PTC.AddSubs PTC.TIP_STR PTC.SOUIP_STR where
  add_subs = \(tip, str) -> PTC.add_subs tip >$> \souip -> (souip, str)

instance PTC.AddSubs T.SimpleType T.SubOrUnder where
  add_subs = \case
    T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV1 ahtv, P.Nothing)) -> PTC.add_subs ahtv
    st -> T.TVS1 <$> PTC.add_subs st

instance PTC.AddSubs T.AdHocTVar T.SubOrUnder where
  add_subs = \ahtv ->
    MS.get >$> M.lookup ahtv >$> \case
      P.Just sou -> sou
      P.Nothing -> P.error "PTC.add_subs st sou: didn't find ahtv"

instance PTC.AddSubs T.InParenT T.InParenTSub where
  add_subs = \case
    T.PT3 pt -> T.PTS2 <$> PTC.add_subs pt
    T.FT3 ft -> T.FTS2 <$> PTC.add_subs ft

-- helpers

compat_union :: PTC.Compatibility -> PTC.Compatibility -> PTC.Compatibility
compat_union = \c1 c2 -> case (c1, c2) of
  (PTC.Compatible m1, PTC.Compatible m2) -> PTC.Compatible $ M.union m1 m2
  _ -> PTC.NotCompatible

compat_list_union :: [PTC.Compatibility] -> PTC.Compatibility
compat_list_union = P.foldr compat_union $ PTC.Compatible M.empty

error_zip :: ([a], [b]) -> [(a, b)]
error_zip = \case
  ([], []) -> []
  (a : as, b : bs) -> (a, b) : error_zip(as, bs)
  _ -> P.error "error_zip: lists not the same length"

{-
For fast vim file navigation:
AST.hs
Preprocess.hs
-}
