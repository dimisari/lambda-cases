{-# language
  LambdaCase, MultiParamTypeClasses, FunctionalDependencies,
  TypeSynonymInstances, FlexibleInstances, UndecidableInstances
#-}

module Generation.CheckCompatibility where

import qualified Data.Map.Strict as M
import Control.Monad.State

import ASTTypes
import ShowInstances
import Helpers

-- types

type AHTVMap = M.Map AdHocTVar SubOrUnder

data Compatibility =
  Compatible AHTVMap | NotCompatible

instance Eq Compatibility where
  c1 == c2 = case (c1, c2) of
    (NotCompatible, NotCompatible) -> True
    _ -> False

type WAHTVMap a = State AHTVMap a

-- helpers

compat_union :: Compatibility -> Compatibility -> Compatibility
compat_union = \c1 c2 -> case (c1, c2) of
  (Compatible m1, Compatible m2) -> Compatible $ M.union m1 m2
  _ -> NotCompatible

compat_list_union :: [Compatibility] -> Compatibility
compat_list_union = foldr compat_union $ Compatible M.empty

error_zip :: ([a], [b]) -> [(a, b)]
error_zip = \case
  ([], []) -> []
  (a : as, b : bs) -> (a, b) : error_zip(as, bs)
  _ -> error "error_zip: lists not the same length"

-- CheckCompatibility class

class CheckCompatibility a b where
  check_compat :: (a, b) -> Compatibility

instance
  (Show a, Show b, CheckCompatibility a b) => CheckCompatibility (Maybe a) (Maybe b)
  where
  check_compat = \case
    (Just a, Just b) -> check_compat(a, b)
    (Nothing, Nothing) -> Compatible M.empty
    _ -> NotCompatible

instance CheckCompatibility a b => CheckCompatibility [a] [b] where
  check_compat = error_zip .> map check_compat .> compat_list_union

instance CheckCompatibility PropName PropNameWithSubs where
  check_compat = \case
    (NPStart1 nps1, NPStart2 nps2) -> check_compat(nps1, nps2)
    (TIPStart tips, SIPStart sips) -> check_compat(tips, sips)
    _ -> NotCompatible

instance CheckCompatibility NPStart1 NPStart2 where
  check_compat = \((c1, np_tip_pairs, mnp1), (c2, np_sip_pairs, mnp2)) ->
    case c1 == c2 && mnp1 == mnp2 of
      False -> NotCompatible
      True -> check_compat(np_tip_pairs, np_sip_pairs)

instance CheckCompatibility TIPStart SIPStart where
  check_compat ((tip_np_pairs, mtip), (sip_np_pairs, msip)) =
    compat_union pairs_compat mtip_msip_mcompat
    where
    pairs_compat :: Compatibility
    pairs_compat = check_compat(tip_np_pairs, sip_np_pairs)

    mtip_msip_mcompat :: Compatibility
    mtip_msip_mcompat = check_compat(mtip, msip)

instance CheckCompatibility TypesInParen SubsInParen where
  check_compat = \(TIP (st1, sts), SIP (tvs1,tvss)) ->
    check_compat(st1 : sts, tvs1 : tvss)

type NP_TIP_Pair = (NamePart, TypesInParen)
type NP_SIP_Pair = (NamePart, SubsInParen)
instance CheckCompatibility NP_TIP_Pair NP_SIP_Pair where
  check_compat = \((np1, tip), (np2, sip)) ->
    case np1 == np2 of
      False -> NotCompatible
      True -> check_compat(tip, sip)

type TIP_NP_Pair = (TypesInParen, NamePart)
type SIP_NP_Pair = (SubsInParen, NamePart)
instance CheckCompatibility TIP_NP_Pair SIP_NP_Pair where
  check_compat = \((tip, np1), (sip, np2)) ->
    case np1 == np2 of
      False -> NotCompatible
      True -> check_compat(tip, sip)

instance CheckCompatibility SimpleType TVarSub where
  check_compat = \case
    (PTV1 _, PTV4 _) -> Compatible M.empty
    (TAIOA1 taioa, TAIOAS1 taioas) -> check_compat(taioa, taioas)
    (PoT1 pt, PoTS1 pts) -> check_compat(pt, pts)
    (PT1 pt, PTS1 pts) -> check_compat(pt, pts)
    (FT1 ft, FTS1 fts) -> check_compat(ft, fts)

instance CheckCompatibility TypeAppIdOrAHTV TypeAppIdOrAHTVSub where
  check_compat =
    \(TAIOA (mtip1, taioam, mtip2), TAIOAS (msouip1, taioasm, msouip2)) ->
    compat_list_union
      [ check_compat(mtip1, msouip1)
      , check_compat(taioam, taioasm)
      , check_compat(mtip2, msouip2)
      ]

instance CheckCompatibility PowerType PowerTypeSub where
  check_compat = \(PoT (pbt, i), PoTS (pbts, j)) ->
    case i == j of
      False -> NotCompatible
      True -> check_compat(pbt, pbts)

instance CheckCompatibility ProdType ProdTypeSub where
  check_compat = \(PT (ft1, fts), PTS (fts1, ftss)) ->
    check_compat(ft1 : fts, fts1 : ftss)

instance CheckCompatibility FuncType FuncTypeSub where
  check_compat = \(FT (it, ot), FTS (its, ots)) ->
    compat_union (check_compat(it, its)) (check_compat(ot, ots))

instance CheckCompatibility TypesInParen SubsOrUndersInParen where
  check_compat = \(TIP (st1, sts), SOUIP (sou1, sous)) ->
    check_compat(st1 : sts, sou1 : sous)

instance CheckCompatibility TAIOAMiddle TAIOASMiddle where
  check_compat = \case
    (AHTV2 ahtv, taioasm) ->
      Compatible $ M.singleton ahtv taioasm_sou
      where
      taioasm_sou :: SubOrUnder
      taioasm_sou = TVS1 $ TAIOAS1 $ TAIOAS (Nothing, taioasm, Nothing)
    (TIdStart1 (tid1, tip_str_pairs), TIdStart2 (tid2, souip_str_pairs)) ->
      case tid1 == tid2 of
        False -> NotCompatible
        True -> check_compat(tip_str_pairs, souip_str_pairs)
    _ -> NotCompatible

instance CheckCompatibility PowerBaseType PowerBaseTypeSub where
  check_compat = \case
    (PTV2 _, PTV5 _) -> Compatible M.empty
    (TAIOA2 taioa, TAIOAS2 taioas) -> check_compat(taioa, taioas)
    (IPT ipt, IPTS ipts) -> check_compat(ipt, ipts)
    _ -> NotCompatible

instance CheckCompatibility FieldType FieldTypeSub where
  check_compat = \case
    (PBT1 pbt, PBTS1 pbts) -> check_compat(pbt, pbts)
    (PoT2 pt, PoTS2 pts) -> check_compat(pt, pts)
    _ -> NotCompatible

instance CheckCompatibility InOrOutType InOrOutTypeSub where
  check_compat = \case
    (PTV3 _, PTV6 _) -> Compatible M.empty
    (TAIOA3 taioa, TAIOAS3 taioas) -> check_compat(taioa, taioas)
    (PoT4 pt, PoTS3 pts) -> check_compat(pt, pts)
    (PT2 pt, PTS3 pts) -> check_compat(pt, pts)
    (FT2 ft, FTS3 fts) -> check_compat(ft, fts)
    _ -> NotCompatible

instance CheckCompatibility SimpleType SubOrUnder where
  check_compat = \case
    (TAIOA1 (TAIOA (Nothing, AHTV2 ahtv, Nothing)), sou) ->
      Compatible $ M.singleton ahtv sou
    (st, TVS1 tvs) -> check_compat(st, tvs)
    _ -> NotCompatible

type TIP_STR = (TypesInParen, String)
type SOUIP_STR = (SubsOrUndersInParen, String)
instance CheckCompatibility TIP_STR SOUIP_STR where
  check_compat = \((tip, str1), (souip, str2)) ->
    case str1 == str2 of
      False -> NotCompatible
      True -> check_compat(tip, souip)

instance CheckCompatibility InParenT InParenTSub where
  check_compat = \case
    (PT3 pt, PTS2 pts) -> check_compat(pt, pts)
    (FT3 ft, FTS2 fts) -> check_compat(ft, fts)
    _ -> NotCompatible

-- AddSubs Class

class AddSubs a b where
  add_subs :: a -> WAHTVMap b

add_subs_pair :: (AddSubs a b, AddSubs c d) => (a, c) -> WAHTVMap (b, d)
add_subs_pair = \(a, c) ->
  add_subs a >>= \b -> add_subs c >>= \d -> return (b, d)

add_subs_triple
  :: (AddSubs a b, AddSubs c d, AddSubs e f) => (a, c, e) -> WAHTVMap (b, d, f)
add_subs_triple = \(a, c, e) ->
  add_subs a >>= \b -> add_subs c >>= \d -> add_subs e >>= \f ->
  return (b, d, f)

instance AddSubs a b => AddSubs (Maybe a) (Maybe b) where
  add_subs = \case
    Just a -> Just <$> add_subs a
    Nothing -> return Nothing

instance AddSubs a b => AddSubs [a] [b] where
  add_subs = traverse add_subs

instance AddSubs PropName PropNameWithSubs where
  add_subs = \case
    NPStart1 nps1 -> NPStart2 <$> add_subs nps1
    TIPStart tips -> SIPStart <$> add_subs tips

instance AddSubs NPStart1 NPStart2 where
  add_subs = \(c, np_tip_pairs, mnp) ->
    add_subs np_tip_pairs >$> \np_sip_pairs -> (c, np_sip_pairs, mnp)

instance AddSubs TIPStart SIPStart where
  add_subs = \(tip_np_pairs, mtip) -> add_subs_pair(tip_np_pairs, mtip)

instance AddSubs NP_TIP_Pair NP_SIP_Pair where
  add_subs = \(np, tip) -> add_subs tip >$> \sip -> (np, sip)

instance AddSubs TIP_NP_Pair SIP_NP_Pair where
  add_subs = \(tip, np) -> add_subs tip >$> \sip -> (sip, np)

instance AddSubs TypesInParen SubsInParen where
  add_subs = \(TIP sts) -> SIP <$> add_subs_pair sts

instance AddSubs SimpleType TVarSub where
  add_subs = \case
    TAIOA1 (TAIOA (Nothing, AHTV2 ahtv, Nothing)) ->
      get >$> M.lookup ahtv >$> \case
        Just (TVS1 tvs) -> tvs
        Just Underscore4 -> error "add_subs st tvs: ahtv is underscore"
        Nothing -> error "add_subs st tvs: didn't find ahtv"
    PTV1 ptv -> return $ PTV4 ptv
    TAIOA1 taioa -> TAIOAS1 <$> add_subs taioa
    PoT1 pt -> PoTS1 <$> add_subs pt
    PT1 pt -> PTS1 <$> add_subs pt
    FT1 ft -> FTS1 <$> add_subs ft

instance AddSubs TypeAppIdOrAHTV TypeAppIdOrAHTVSub where
  add_subs = \(TAIOA taioa) -> TAIOAS <$> add_subs_triple taioa

instance AddSubs PowerType PowerTypeSub where
  add_subs = \(PoT (pbt, i)) -> PoTS <$> (add_subs pbt >$> \pbts -> (pbts, i))

instance AddSubs ProdType ProdTypeSub where
  add_subs = \(PT fts) -> PTS <$> add_subs_pair fts

instance AddSubs FuncType FuncTypeSub where
  add_subs = \(FT it_ot) -> FTS <$> add_subs_pair it_ot

instance AddSubs TAIOAMiddle TAIOASMiddle where
  add_subs = \case
    TIdStart1 (tid, tip_str_pairs) ->
      add_subs tip_str_pairs >$> \souip_str_pairs ->
      TIdStart2 (tid, souip_str_pairs)
    AHTV2 ahtv -> undefined

instance AddSubs TypesInParen SubsOrUndersInParen where
  add_subs = \(TIP sts) -> SOUIP <$> add_subs_pair sts

instance AddSubs PowerBaseType PowerBaseTypeSub where
  add_subs = \case
    PTV2 ptv -> return $ PTV5 ptv
    TAIOA2 taioa -> TAIOAS2 <$> add_subs taioa
    IPT ipt -> IPTS <$> add_subs ipt

instance AddSubs FieldType FieldTypeSub where
  add_subs = \case
    PBT1 pbt -> PBTS1 <$> add_subs pbt
    PoT2 pt -> PoTS2 <$> add_subs pt

instance AddSubs InOrOutType InOrOutTypeSub where
  add_subs = \case
    PTV3 ptv -> return $ PTV6 ptv
    TAIOA3 taioa -> TAIOAS3 <$> add_subs taioa
    PoT4 pt -> PoTS3 <$> add_subs pt
    PT2 pt -> PTS3 <$> add_subs pt
    FT2 ft -> FTS3 <$> add_subs ft

instance AddSubs TIP_STR SOUIP_STR where
  add_subs = \(tip, str) -> add_subs tip >$> \souip -> (souip, str)

instance AddSubs SimpleType SubOrUnder where
  add_subs = \case
    TAIOA1 (TAIOA (Nothing, AHTV2 ahtv, Nothing)) ->
      get >$> M.lookup ahtv >$> \case
        Just sou -> sou
        Nothing -> error "add_subs st sou: didn't find ahtv"
    st -> TVS1 <$> add_subs st

instance AddSubs InParenT InParenTSub where
  add_subs = \case
    PT3 pt -> PTS2 <$> add_subs pt
    FT3 ft -> FTS2 <$> add_subs ft

{-
For fast vim file navigation:
AST.hs
-}
