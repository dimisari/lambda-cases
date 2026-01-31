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

import Generation.TypesAndHelpers qualified as GTH

-- types

type AHTVMap = M.Map T.AdHocTVar T.SubOrUnder

data Compatibility =
  Compatible AHTVMap | NotCompatible

instance P.Eq Compatibility where
  c1 == c2 = case (c1, c2) of
    (NotCompatible, NotCompatible) -> P.True
    _ -> P.False

-- CheckCompatibility class

class CheckCompatibility a b where
  check_compat :: (a, b) -> Compatibility

instance CheckCompatibility a b => CheckCompatibility (P.Maybe a) (P.Maybe b)
  where
  check_compat = \case
    (P.Just a, P.Just b) -> check_compat(a, b)
    (P.Nothing, P.Nothing) -> Compatible M.empty
    _ -> NotCompatible

instance CheckCompatibility a b => CheckCompatibility [a] [b] where
  check_compat = error_zip .> P.map check_compat .> compat_list_union

instance CheckCompatibility T.PropName T.PropNameWithSubs where
  check_compat = \case
    (T.NPStart1 nps1, T.NPStart2 nps2) -> check_compat(nps1, nps2)
    (T.TIPStart tips, T.SIPStart sips) -> check_compat(tips, sips)
    _ -> NotCompatible

instance CheckCompatibility T.NPStart1 T.NPStart2 where
  check_compat = \((c1, np_tip_pairs, mnp1), (c2, np_sip_pairs, mnp2)) ->
    case c1 == c2 && mnp1 == mnp2 of
      P.False -> NotCompatible
      P.True -> check_compat(np_tip_pairs, np_sip_pairs)

instance CheckCompatibility T.TIPStart T.SIPStart where
  check_compat ((tip_np_pairs, mtip), (sip_np_pairs, msip)) =
    compat_union pairs_compat mtip_msip_mcompat
    where
    pairs_compat :: Compatibility
    pairs_compat = check_compat(tip_np_pairs, sip_np_pairs)

    mtip_msip_mcompat :: Compatibility
    mtip_msip_mcompat = check_compat(mtip, msip)

instance CheckCompatibility T.TypesInParen T.SubsInParen where
  check_compat = \(T.TIP (st1, sts), T.SIP (tvs1,tvss)) ->
    check_compat(st1 : sts, tvs1 : tvss)

type NP_TIP_Pair = (T.NamePart, T.TypesInParen)

type NP_SIP_Pair = (T.NamePart, T.SubsInParen)

instance CheckCompatibility NP_TIP_Pair NP_SIP_Pair where
  check_compat = \((np1, tip), (np2, sip)) ->
    case np1 == np2 of
      P.False -> NotCompatible
      P.True -> check_compat(tip, sip)

type TIP_NP_Pair = (T.TypesInParen, T.NamePart)

type SIP_NP_Pair = (T.SubsInParen, T.NamePart)

instance CheckCompatibility TIP_NP_Pair SIP_NP_Pair where
  check_compat = \((tip, np1), (sip, np2)) ->
    case np1 == np2 of
      P.False -> NotCompatible
      P.True -> check_compat(tip, sip)

instance CheckCompatibility T.SimpleType T.TVarSub where
  check_compat = \case
    (T.PTV1 _, T.PTV4 _) -> Compatible M.empty
    (T.TAIOA1 taioa, T.TAIOAS1 taioas) -> check_compat(taioa, taioas)
    (T.PoT1 pt, T.PoTS1 pts) -> check_compat(pt, pts)
    (T.PT1 pt, T.PTS1 pts) -> check_compat(pt, pts)
    (T.FT1 ft, T.FTS1 fts) -> check_compat(ft, fts)

instance CheckCompatibility T.TypeAppIdOrAHTV T.TypeAppIdOrAHTVSub where
  check_compat =
    \(T.TAIOA (mtip1, taioam, mtip2), T.TAIOAS (msouip1, taioasm, msouip2)) ->
    compat_list_union
      [ check_compat(mtip1, msouip1)
      , check_compat(taioam, taioasm)
      , check_compat(mtip2, msouip2)
      ]

instance CheckCompatibility T.PowerType T.PowerTypeSub where
  check_compat = \(T.PoT (pbt, i), T.PoTS (pbts, j)) ->
    case i == j of
      P.False -> NotCompatible
      P.True -> check_compat(pbt, pbts)

instance CheckCompatibility T.ProdType T.ProdTypeSub where
  check_compat = \(T.PT (ft1, fts), T.PTS (fts1, ftss)) ->
    check_compat(ft1 : fts, fts1 : ftss)

instance CheckCompatibility T.FuncType T.FuncTypeSub where
  check_compat = \(T.FT (it, ot), T.FTS (its, ots)) ->
    compat_union (check_compat(it, its)) (check_compat(ot, ots))

instance CheckCompatibility T.TypesInParen T.SubsOrUndersInParen where
  check_compat = \(T.TIP (st1, sts), T.SOUIP (sou1, sous)) ->
    check_compat(st1 : sts, sou1 : sous)

instance CheckCompatibility T.TAIOAMiddle T.TAIOASMiddle where
  check_compat = \case
    (T.AHTV2 ahtv, taioasm) ->
      Compatible $ M.singleton ahtv $ GTH.taioasm_to_sou taioasm
    (T.TIdStart1 (tid1, tip_str_pairs), T.TIdStart2 (tid2, souip_str_pairs)) ->
      case tid1 == tid2 of
        P.False -> NotCompatible
        P.True -> check_compat(tip_str_pairs, souip_str_pairs)
    _ -> NotCompatible

instance CheckCompatibility T.PowerBaseType T.PowerBaseTypeSub where
  check_compat = \case
    (T.PTV2 _, T.PTV5 _) -> Compatible M.empty
    (T.TAIOA2 taioa, T.TAIOAS2 taioas) -> check_compat(taioa, taioas)
    (T.IPT ipt, T.IPTS ipts) -> check_compat(ipt, ipts)
    _ -> NotCompatible

instance CheckCompatibility T.FieldType T.FieldTypeSub where
  check_compat = \case
    (T.PBT1 pbt, T.PBTS1 pbts) -> check_compat(pbt, pbts)
    (T.PoT2 pt, T.PoTS2 pts) -> check_compat(pt, pts)
    _ -> NotCompatible

instance CheckCompatibility T.InOrOutType T.InOrOutTypeSub where
  check_compat = \case
    (T.PTV3 _, T.PTV6 _) -> Compatible M.empty
    (T.TAIOA3 taioa, T.TAIOAS3 taioas) -> check_compat(taioa, taioas)
    (T.PoT4 pt, T.PoTS3 pts) -> check_compat(pt, pts)
    (T.PT2 pt, T.PTS3 pts) -> check_compat(pt, pts)
    (T.FT2 ft, T.FTS3 fts) -> check_compat(ft, fts)
    _ -> NotCompatible

instance CheckCompatibility T.SimpleType T.SubOrUnder where
  check_compat = \case
    (T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV2 ahtv, P.Nothing)), sou) ->
      Compatible $ M.singleton ahtv sou
    (st, T.TVS1 tvs) -> check_compat(st, tvs)
    _ -> NotCompatible

type TIP_STR = (T.TypesInParen, P.String)

type SOUIP_STR = (T.SubsOrUndersInParen, P.String)

instance CheckCompatibility TIP_STR SOUIP_STR where
  check_compat = \((tip, str1), (souip, str2)) ->
    case str1 == str2 of
      P.False -> NotCompatible
      P.True -> check_compat(tip, souip)

instance CheckCompatibility T.InParenT T.InParenTSub where
  check_compat = \case
    (T.PT3 pt, T.PTS2 pts) -> check_compat(pt, pts)
    (T.FT3 ft, T.FTS2 fts) -> check_compat(ft, fts)
    _ -> NotCompatible

-- AddSubs Class

type WAHTVMap a = MS.State AHTVMap a

class AddSubs a b where
  add_subs :: a -> WAHTVMap b

add_subs_pair :: (AddSubs a b, AddSubs c d) => (a, c) -> WAHTVMap (b, d)
add_subs_pair = \(a, c) ->
  add_subs a >>= \b -> add_subs c >>= \d -> P.return (b, d)

add_subs_triple
  :: (AddSubs a b, AddSubs c d, AddSubs e f) => (a, c, e) -> WAHTVMap (b, d, f)
add_subs_triple = \(a, c, e) ->
  add_subs a >>= \b -> add_subs c >>= \d -> add_subs e >>= \f ->
  P.return (b, d, f)

instance AddSubs a b => AddSubs (P.Maybe a) (P.Maybe b) where
  add_subs = P.traverse add_subs

instance AddSubs a b => AddSubs [a] [b] where
  add_subs = P.traverse add_subs

instance AddSubs T.PropName T.PropNameWithSubs where
  add_subs = \case
    T.NPStart1 nps1 -> T.NPStart2 <$> add_subs nps1
    T.TIPStart tips -> T.SIPStart <$> add_subs tips

instance AddSubs T.NPStart1 T.NPStart2 where
  add_subs = \(c, np_tip_pairs, mnp) ->
    add_subs np_tip_pairs >$> \np_sip_pairs -> (c, np_sip_pairs, mnp)

instance AddSubs T.TIPStart T.SIPStart where
  add_subs = \(tip_np_pairs, mtip) -> add_subs_pair(tip_np_pairs, mtip)

instance AddSubs NP_TIP_Pair NP_SIP_Pair where
  add_subs = \(np, tip) -> add_subs tip >$> \sip -> (np, sip)

instance AddSubs TIP_NP_Pair SIP_NP_Pair where
  add_subs = \(tip, np) -> add_subs tip >$> \sip -> (sip, np)

instance AddSubs T.TypesInParen T.SubsInParen where
  add_subs = \(T.TIP sts) -> T.SIP <$> add_subs_pair sts

instance AddSubs T.SimpleType T.TVarSub where
  add_subs = \case
    T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV2 ahtv, P.Nothing)) -> add_subs ahtv
    T.PTV1 ptv -> P.return $ T.PTV4 ptv
    T.TAIOA1 taioa -> T.TAIOAS1 <$> add_subs taioa
    T.PoT1 pt -> T.PoTS1 <$> add_subs pt
    T.PT1 pt -> T.PTS1 <$> add_subs pt
    T.FT1 ft -> T.FTS1 <$> add_subs ft

instance AddSubs T.AdHocTVar T.TVarSub where
  add_subs = \ahtv ->
    MS.get >$> M.lookup ahtv >$> \case
      P.Just (T.TVS1 tvs) -> tvs
      P.Just T.Underscore4 -> P.error "add_subs st tvs: ahtv is underscore"
      P.Nothing -> P.error "add_subs st tvs: didn't find ahtv"

instance AddSubs T.TypeAppIdOrAHTV T.TypeAppIdOrAHTVSub where
  add_subs = \(T.TAIOA taioa) -> T.TAIOAS <$> add_subs_triple taioa

instance AddSubs T.PowerType T.PowerTypeSub where
  add_subs =
    \(T.PoT (pbt, i)) -> T.PoTS <$> (add_subs pbt >$> \pbts -> (pbts, i))

instance AddSubs T.ProdType T.ProdTypeSub where
  add_subs = \(T.PT fts) -> T.PTS <$> add_subs_pair fts

instance AddSubs T.FuncType T.FuncTypeSub where
  add_subs = \(T.FT it_ot) -> T.FTS <$> add_subs_pair it_ot

instance AddSubs T.TAIOAMiddle T.TAIOASMiddle where
  add_subs = \case
    T.TIdStart1 (tid, tip_str_pairs) ->
      add_subs tip_str_pairs >$> \souip_str_pairs ->
      T.TIdStart2 (tid, souip_str_pairs)
    T.AHTV2 ahtv -> P.undefined

instance AddSubs T.TypesInParen T.SubsOrUndersInParen where
  add_subs = \(T.TIP sts) -> T.SOUIP <$> add_subs_pair sts

instance AddSubs T.PowerBaseType T.PowerBaseTypeSub where
  add_subs = \case
    T.PTV2 ptv -> P.return $ T.PTV5 ptv
    T.TAIOA2 taioa -> T.TAIOAS2 <$> add_subs taioa
    T.IPT ipt -> T.IPTS <$> add_subs ipt

instance AddSubs T.FieldType T.FieldTypeSub where
  add_subs = \case
    T.PBT1 pbt -> T.PBTS1 <$> add_subs pbt
    T.PoT2 pt -> T.PoTS2 <$> add_subs pt

instance AddSubs T.InOrOutType T.InOrOutTypeSub where
  add_subs = \case
    T.PTV3 ptv -> P.return $ T.PTV6 ptv
    T.TAIOA3 taioa -> T.TAIOAS3 <$> add_subs taioa
    T.PoT4 pt -> T.PoTS3 <$> add_subs pt
    T.PT2 pt -> T.PTS3 <$> add_subs pt
    T.FT2 ft -> T.FTS3 <$> add_subs ft

instance AddSubs TIP_STR SOUIP_STR where
  add_subs = \(tip, str) -> add_subs tip >$> \souip -> (souip, str)

instance AddSubs T.SimpleType T.SubOrUnder where
  add_subs = \case
    T.TAIOA1 (T.TAIOA (P.Nothing, T.AHTV2 ahtv, P.Nothing)) -> add_subs ahtv
    st -> T.TVS1 <$> add_subs st

instance AddSubs T.AdHocTVar T.SubOrUnder where
  add_subs = \ahtv ->
    MS.get >$> M.lookup ahtv >$> \case
      P.Just sou -> sou
      P.Nothing -> P.error "add_subs st sou: didn't find ahtv"

instance AddSubs T.InParenT T.InParenTSub where
  add_subs = \case
    T.PT3 pt -> T.PTS2 <$> add_subs pt
    T.FT3 ft -> T.FTS2 <$> add_subs ft

-- helpers

compat_union :: Compatibility -> Compatibility -> Compatibility
compat_union = \c1 c2 -> case (c1, c2) of
  (Compatible m1, Compatible m2) -> Compatible $ M.union m1 m2
  _ -> NotCompatible

compat_list_union :: [Compatibility] -> Compatibility
compat_list_union = P.foldr compat_union $ Compatible M.empty

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
