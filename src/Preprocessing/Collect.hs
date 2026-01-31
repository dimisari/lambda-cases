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

import Generation.TypesAndHelpers qualified as GTH

-- CollectFieldIds types, class and final function

type FieldId = T.SimpleId

type FieldIds = S.Set FieldId

type FieldIdsState = MS.State FieldIds ()

class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

field_ids :: T.Program -> FieldIds
field_ids = \prog -> MS.execState (collect_fids prog) S.empty

-- CollectRenamingProps types, class and final function

type RenamingProp = (T.PropName, [T.PropName])

type RenamingProps = [RenamingProp]

type RenamingPropsState = MS.State RenamingProps ()

class CollectRenamingProps a where
  collect_rps :: a -> RenamingPropsState

renaming_props :: T.Program -> RenamingProps
renaming_props = \prog -> MS.execState (collect_rps prog) []

-- CollectOrValues types, class, initial map and final function

type OrValue = T.SimpleId

type EmptyOrValues = S.Set OrValue

type FullOrValuesMap = M.Map OrValue T.Identifier

type OrValues = (EmptyOrValues, FullOrValuesMap)

type OrValuesState = MS.State OrValues ()

class CollectOrValues a where
  collect_ovms :: a -> OrValuesState

init_or_val_map :: FullOrValuesMap
init_or_val_map =
  M.fromList $
    P.map (\(s1, s2) -> (GTH.str_to_sid s1, GTH.str_to_id s2)) predefined
  where
  predefined :: [(P.String, P.String)]
  predefined =
    [("a_value", "the_value"), ("error", "err"), ("result", "res")]

or_values :: T.Program -> OrValues
or_values = \prog -> MS.execState (collect_ovms prog) (S.empty, init_or_val_map)

-- CollectParamTVars types, class and final function

type ParamTVars = S.Set T.ParamTVar

type ParamTVarsState = MS.State ParamTVars ()

class CollectParamTVars a where
  collect_ptvs :: a -> ParamTVarsState

param_t_vars :: T.Type -> [T.ParamTVar]
param_t_vars =
  \(T.Ty (_, st)) -> MS.execState (collect_ptvs st) S.empty &> S.toList

{-
Below are all the instances
-}

-- CollectFieldIds instances

instance CollectFieldIds T.Program where
  collect_fids = \(T.P (pp, pps)) -> P.mapM_ collect_fids $ pp : pps

instance CollectFieldIds T.ProgramPart where
  collect_fids = \case
    T.TD td -> collect_fids td
    _ -> H.do_nothing

instance CollectFieldIds T.TypeDef where
  collect_fids = \case
    T.TTD1 ttd -> collect_fids ttd
    _ -> H.do_nothing

instance CollectFieldIds T.TupleTypeDef where
  collect_fids = \(T.TTD (_, _, idt)) -> collect_fids idt

instance CollectFieldIds T.FieldNames where
  collect_fids = \(T.PCSIs (si, sis)) -> P.mapM_ collect_fids $ si : sis

instance CollectFieldIds T.SimpleId where
  collect_fids = \sid -> MS.modify (S.insert sid)

-- CollectParamTVars instances

instance CollectParamTVars T.SimpleType where
  collect_ptvs = \case
    T.PTV1 ptv -> MS.modify (S.insert ptv)
    T.TAIOA1 taioa -> collect_ptvs taioa
    T.PoT1 pt -> collect_ptvs pt
    T.PT1 pt -> collect_ptvs pt
    T.FT1 ft -> collect_ptvs ft

instance CollectParamTVars T.TypeAppIdOrAHTV where
  collect_ptvs = \(T.TAIOA (mtip1, taioam, mtip2)) ->
    collect_ptvs mtip1 >> collect_ptvs taioam >> collect_ptvs mtip2

instance CollectParamTVars T.PowerType where
  collect_ptvs = \(T.PoT (pbt, _)) -> collect_ptvs pbt

instance CollectParamTVars T.ProdType where
  collect_ptvs = \(T.PT (ft, fts)) -> P.mapM_ collect_ptvs $ ft : fts

instance CollectParamTVars T.FuncType where
  collect_ptvs = \(T.FT (ioot1, ioot2)) ->
    collect_ptvs ioot1 >> collect_ptvs ioot2

instance CollectParamTVars (P.Maybe T.TypesInParen) where
  collect_ptvs = \case
    P.Nothing -> H.do_nothing
    P.Just tip -> collect_ptvs tip

instance CollectParamTVars T.TypesInParen where
  collect_ptvs = \(T.TIP (st, sts)) -> P.mapM_ collect_ptvs $ st : sts

instance CollectParamTVars T.TAIOAMiddle where
  collect_ptvs = \case
    T.TIdStart1 (_, tip_str_pairs) -> P.mapM_ collect_ptvs $ tip_str_pairs
    T.AHTV2 _ -> H.do_nothing

instance CollectParamTVars T.PowerBaseType where
  collect_ptvs = \case
    T.PTV2 ptv -> MS.modify (S.insert ptv)
    T.TAIOA2 taioa -> collect_ptvs taioa
    T.IPT ipt -> collect_ptvs ipt

instance CollectParamTVars T.FieldType where
  collect_ptvs = \case
    T.PBT1 pbt -> collect_ptvs pbt
    T.PoT2 pt -> collect_ptvs pt

instance CollectParamTVars T.InOrOutType where
  collect_ptvs = \case
    T.PTV3 ptv -> MS.modify (S.insert ptv)
    T.TAIOA3 taioa -> collect_ptvs taioa
    T.PoT4 pt -> collect_ptvs pt
    T.PT2 pt -> collect_ptvs pt
    T.FT2 ft -> collect_ptvs ft

instance CollectParamTVars (T.TypesInParen, P.String) where
  collect_ptvs = \(tip, _) -> collect_ptvs tip

instance CollectParamTVars T.InParenT where
  collect_ptvs = \case
    T.PT3 pt -> collect_ptvs pt
    T.PoT3 pt -> collect_ptvs pt
    T.FT3 ft -> collect_ptvs ft

-- CollectRenamingProps instances

instance CollectRenamingProps T.Program where
  collect_rps = \(T.P (pp, pps)) -> P.mapM_ collect_rps $ pp : pps

instance CollectRenamingProps T.ProgramPart where
  collect_rps = \case
    T.TPD tpd -> collect_rps tpd
    _ -> H.do_nothing

instance CollectRenamingProps T.TypePropDef where
  collect_rps = \case
    T.RPD1 rpd -> collect_rps rpd
    _ -> H.do_nothing

instance CollectRenamingProps T.RenamingPropDef where
  collect_rps = \(T.RPD (T.PNL pn_key, pn1, pns)) ->
    MS.modify $ (:) (pn_key, pn1 : pns)

-- CollectOrValues instances

instance CollectOrValues T.Program where
  collect_ovms = \(T.P (pp, pps)) -> P.mapM_ collect_ovms $ pp : pps

instance CollectOrValues T.ProgramPart where
  collect_ovms = \case
    T.TD td -> collect_ovms td
    _ -> H.do_nothing

instance CollectOrValues T.TypeDef where
  collect_ovms = \case
    T.OTD1 otd -> collect_ovms otd
    _ -> H.do_nothing

instance CollectOrValues T.OrTypeDef where
  collect_ovms = \(T.OTD (_, pv, pvs)) -> P.mapM_ collect_ovms $ pv : pvs

instance CollectOrValues T.PossibleValue where
  collect_ovms = \(T.PV (ov, maybe_id_st)) -> case maybe_id_st of
    P.Just (id, _) -> MS.modify $ \(nc, fovm) -> (nc, M.insert ov id fovm)
    P.Nothing -> MS.modify $ \(nc, fovm) -> (S.insert ov nc, fovm)

{-
For fast vim file navigation:
Preprocess.hs
AST.hs
-}
