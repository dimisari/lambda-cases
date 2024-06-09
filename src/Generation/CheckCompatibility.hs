{-# language
  LambdaCase, MultiParamTypeClasses, FunctionalDependencies,
  TypeSynonymInstances, FlexibleInstances, UndecidableInstances
#-}

module Generation.CheckCompatibility where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad.State

import ASTTypes
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

--helpers
compat_union :: Compatibility -> Compatibility -> Compatibility
compat_union = \c1 c2 -> case (c1, c2) of
  (Compatible m1, Compatible m2) ->
    case S.union (M.keysSet m1) (M.keysSet m2) == S.empty of
      True -> Compatible $ M.union m1 m2
      False -> NotCompatible
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

instance CheckCompatibility a b => CheckCompatibility (Maybe a) (Maybe b) where
  check_compat = \case
    (Just a, Just b) -> check_compat(a, b)
    _ -> NotCompatible

instance CheckCompatibility a b => CheckCompatibility [a] [b] where
  check_compat = error_zip .> map check_compat .> compat_list_union

instance CheckCompatibility PropName PropNameWithSubs where
  check_compat = \case
    (NPStart1 nps1, NPStart2 nps2) -> check_compat(nps1, nps2)
    (AHVIPStart ahvips, SIPStart sips) -> check_compat(ahvips, sips)
    _ -> NotCompatible

instance CheckCompatibility NPStart1 NPStart2 where
  check_compat = \((c1, np_tip_pairs, mnp1), (c2, np_sip_pairs, mnp2)) ->
    case c1 == c2 && mnp1 == mnp2 of
      False -> NotCompatible
      True -> check_compat(np_tip_pairs, np_sip_pairs)

instance CheckCompatibility AHVIPStart SIPStart where
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
    (PoT3 pt, PoTS2 pts) -> check_compat(pt, pts)
    _ -> NotCompatible

instance CheckCompatibility InOrOutType InOrOutTypeSub where
  check_compat = \case
    (PTV3 _, PTV6 _) -> Compatible M.empty
    (TAIOA3 taioa, TAIOAS3 taioas) -> check_compat(taioa, taioas)
    (PoT2 pt, PoTS3 pts) -> check_compat(pt, pts)
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

instance AddSubs PropName PropNameWithSubs where
  add_subs = undefined

