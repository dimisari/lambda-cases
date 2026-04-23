{-
This file contains code that traverses the AST and collects stuff to be
used in the preprocessing phase:
- field ids
- ranaming propositions
- or values (empty and full)
and code that collects stuff from particular AST types in the translation
phase:
- parametric type variables
-}

{-# language LambdaCase, StandaloneDeriving, FlexibleInstances #-}

module Preprocessing.Collect where

import Prelude (($), (>>))
import Prelude qualified as P
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Control.Monad.State qualified as MS

import ASTTypes qualified as T
import Helpers ((&>))
import Helpers qualified as H

import Generation.Helpers qualified as GH
import Preprocessing.TypesAndClasses qualified as PTC

-- CollectFieldIds final function

field_ids :: T.Program -> PTC.FieldIds
field_ids = \prog -> MS.execState (PTC.collect_fids prog) S.empty

-- CollectRenamingProps final function

renaming_props :: T.Program -> PTC.RenamingProps
renaming_props = \prog -> MS.execState (PTC.collect_rps prog) []

-- CollectOrValues initial map and final function

init_or_val_map :: PTC.FullOrValuesMap
init_or_val_map =
  M.fromList $
    P.map (\(s1, s2) -> (GH.str_to_sid s1, GH.str_to_id s2)) predefined
  where
  predefined :: [(P.String, P.String)]
  predefined =
    [("a_value", "the_value"), ("error", "err"), ("result", "res")]

or_values :: T.Program -> PTC.OrValues
or_values =
  \prog -> MS.execState (PTC.collect_ovms prog) (S.empty, init_or_val_map)

-- CollectParamTVars final function

param_t_vars :: T.Type -> [T.ParamTVar]
param_t_vars =
  \(T.Ty (_, st)) -> MS.execState (PTC.collect_ptvs st) S.empty &> S.toList

-- CollectFieldIds instances

instance PTC.CollectFieldIds T.Program where
  collect_fids = \(T.P (pp, pps)) -> P.mapM_ PTC.collect_fids $ pp : pps

instance PTC.CollectFieldIds T.ProgramPart where
  collect_fids = \case
    T.TD td -> PTC.collect_fids td
    _ -> H.do_nothing

instance PTC.CollectFieldIds T.TypeDef where
  collect_fids = \case
    T.TTD1 ttd -> PTC.collect_fids ttd
    _ -> H.do_nothing

instance PTC.CollectFieldIds T.TupleTypeDef where
  collect_fids = \(T.TTD (_, _, idt)) -> PTC.collect_fids idt

instance PTC.CollectFieldIds T.FieldNames where
  collect_fids = \(T.PCSIs sids) -> PTC.collect_fids sids

instance PTC.CollectFieldIds T.SimpleIds where
  collect_fids = \(T.SIds (sid, sids)) -> P.mapM_ PTC.collect_fids $ sid : sids

instance PTC.CollectFieldIds T.SimpleId where
  collect_fids = \sid -> MS.modify (S.insert sid)

-- CollectParamTVars instances

instance PTC.CollectParamTVars T.SimpleType where
  collect_ptvs = \case
    T.TAIOA1 taioa -> PTC.collect_ptvs taioa
    T.POPT1 popt -> PTC.collect_ptvs popt
    T.FT1 ft -> PTC.collect_ptvs ft

instance PTC.CollectParamTVars T.ProdOrPowerType where
  collect_ptvs = \case
    T.PT4 pt -> PTC.collect_ptvs pt
    T.PoT5 pt -> PTC.collect_ptvs pt

instance PTC.CollectParamTVars T.TypeAppIdOrTV where
  collect_ptvs = \case
    (T.TAIOA (mtip1, taioam, mtip2)) ->
      PTC.collect_ptvs mtip1 >> PTC.collect_ptvs taioam >> PTC.collect_ptvs mtip2
    T.PTV1 ptv -> MS.modify (S.insert ptv)

instance PTC.CollectParamTVars T.PowerType where
  collect_ptvs = \(T.PoT (pbt, _)) -> PTC.collect_ptvs pbt

instance PTC.CollectParamTVars T.ProdType where
  collect_ptvs = \(T.PT (ft, fts)) -> P.mapM_ PTC.collect_ptvs $ ft : fts

instance PTC.CollectParamTVars T.FuncType where
  collect_ptvs = \(T.FT (ioot1, ioot2)) ->
    PTC.collect_ptvs ioot1 >> PTC.collect_ptvs ioot2

instance PTC.CollectParamTVars (P.Maybe T.TypesInParen) where
  collect_ptvs = \case
    P.Nothing -> H.do_nothing
    P.Just tip -> PTC.collect_ptvs tip

instance PTC.CollectParamTVars T.TypesInParen where
  collect_ptvs = \(T.TIP (st, sts)) -> P.mapM_ PTC.collect_ptvs $ st : sts

instance PTC.CollectParamTVars T.TAIOAMiddle where
  collect_ptvs = \case
    T.TIdStart1 (_, tip_str_pairs) -> P.mapM_ PTC.collect_ptvs $ tip_str_pairs
    T.AHTV1 _ -> H.do_nothing

instance PTC.CollectParamTVars T.PowerBaseType where
  collect_ptvs = \case
    T.TAIOA2 taioa -> PTC.collect_ptvs taioa
    T.IPT ipt -> PTC.collect_ptvs ipt

instance PTC.CollectParamTVars T.FieldType where
  collect_ptvs = \case
    T.PBT1 pbt -> PTC.collect_ptvs pbt
    T.PoT2 pt -> PTC.collect_ptvs pt

instance PTC.CollectParamTVars T.InOrOutType where
  collect_ptvs = \case
    T.TAIOA3 taioa -> PTC.collect_ptvs taioa
    T.POPT2 popt -> PTC.collect_ptvs popt
    T.FT2 ft -> PTC.collect_ptvs ft

instance PTC.CollectParamTVars (T.TypesInParen, P.String) where
  collect_ptvs = \(tip, _) -> PTC.collect_ptvs tip

instance PTC.CollectParamTVars T.InParenT where
  collect_ptvs = \case
    T.PT3 pt -> PTC.collect_ptvs pt
    T.PoT3 pt -> PTC.collect_ptvs pt
    T.FT3 ft -> PTC.collect_ptvs ft

-- CollectRenamingProps instances

instance PTC.CollectRenamingProps T.Program where
  collect_rps = \(T.P (pp, pps)) -> P.mapM_ PTC.collect_rps $ pp : pps

instance PTC.CollectRenamingProps T.ProgramPart where
  collect_rps = \case
    T.TPD tpd -> PTC.collect_rps tpd
    _ -> H.do_nothing

instance PTC.CollectRenamingProps T.TypePropDef where
  collect_rps = \case
    T.RPD1 rpd -> PTC.collect_rps rpd
    _ -> H.do_nothing

instance PTC.CollectRenamingProps T.RenamingPropDef where
  collect_rps = \(T.RPD (T.PNL pn_key, pn1, pns)) ->
    MS.modify $ (:) (pn_key, pn1 : pns)

-- CollectOrValues instances

instance PTC.CollectOrValues T.Program where
  collect_ovms = \(T.P (pp, pps)) -> P.mapM_ PTC.collect_ovms $ pp : pps

instance PTC.CollectOrValues T.ProgramPart where
  collect_ovms = \case
    T.TD td -> PTC.collect_ovms td
    _ -> H.do_nothing

instance PTC.CollectOrValues T.TypeDef where
  collect_ovms = \case
    T.OTD1 otd -> PTC.collect_ovms otd
    _ -> H.do_nothing

instance PTC.CollectOrValues T.OrTypeDef where
  collect_ovms = \(T.OTD (_, otvs)) -> PTC.collect_ovms otvs

instance PTC.CollectOrValues T.OrTypeValues where
  collect_ovms = \case
    T.VL otvsl ->  PTC.collect_ovms otvsl
    T.Ls otvsls -> PTC.collect_ovms otvsls

instance PTC.CollectOrValues T.OrTypeValuesLine where
  collect_ovms = \(T.OTVL (otv, otvs)) -> P.mapM_ PTC.collect_ovms $ otv : otvs

instance PTC.CollectOrValues T.OrTypeValuesLines where
  collect_ovms = \(T.OTVLs (otvl, otvls)) ->
    P.mapM_ PTC.collect_ovms $ otvl : otvls

instance PTC.CollectOrValues T.OrTypeValue where
  collect_ovms = \(T.OTV (sid, maybe_id_st)) -> case maybe_id_st of
    P.Just (T.IV (id, _)) ->
      MS.modify $ \(nc, fovm) -> (nc, M.insert sid id fovm)
    P.Nothing -> MS.modify $ \(nc, fovm) -> (S.insert sid nc, fovm)

{-
For fast vim file navigation:
CheckCompatibility.hs
Collect.hs
Preprocess.hs
TypesAndClasses.hs
-}
