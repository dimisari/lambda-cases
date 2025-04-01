{-# LANGUAGE LambdaCase, StandaloneDeriving, FlexibleInstances #-}

module Generation.Collect where

import qualified Data.Map.Strict as M
import Data.Set hiding (map)
import Control.Monad.State

import ASTTypes
import Helpers

import Generation.TypesAndHelpers

-- types

type FieldId = SimpleId

type NakedCase = SimpleId

type RenamingProp = (PropName, [PropName])


type FieldIds = Set FieldId

type NakedCases = Set NakedCase

type ParamTVars = Set ParamTVar

type RenamingProps = [RenamingProp]

type OrValuesMap = M.Map SimpleId Identifier


type FieldIdsState = State FieldIds ()

type NakedCasesState = State NakedCases ()

type ParamTVarsState = State ParamTVars ()

type RenamingPropsState = State RenamingProps ()

type OrValuesMapState = State OrValuesMap ()

-- classes

class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

class CollectNakedCases a where
  collect_ncs :: a -> NakedCasesState

class CollectParamTVars a where
  collect_ptvs :: a -> ParamTVarsState

class CollectRenamingProps a where
  collect_rps :: a -> RenamingPropsState

class CollectOrValuesMap a where
  collect_ovms :: a -> OrValuesMapState


-- final functions

field_ids :: Program -> FieldIds
field_ids = \prog -> execState (collect_fids prog) empty

naked_cases :: Program -> NakedCases
naked_cases = \prog -> execState (collect_ncs prog) empty

renaming_props :: Program -> RenamingProps
renaming_props = \prog -> execState (collect_rps prog) []

or_values_map :: Program -> OrValuesMap
or_values_map = \prog -> execState (collect_ovms prog) init_or_val_map

param_t_vars :: Type -> [ParamTVar]
param_t_vars = \(Ty (_, st)) -> execState (collect_ptvs st) empty &> toList


-- CollectFieldIds

instance CollectFieldIds Program where
  collect_fids = \(P (pp, pps)) -> mapM_ collect_fids $ pp : pps

instance CollectFieldIds ProgramPart where
  collect_fids = \case
    TD td -> collect_fids td
    _ -> do_nothing

instance CollectFieldIds TypeDef where
  collect_fids = \case
    TTD1 ttd -> collect_fids ttd
    _ -> do_nothing

instance CollectFieldIds TupleTypeDef where
  collect_fids = \(TTD (_, _, idt)) -> collect_fids idt

instance CollectFieldIds FieldNames where
  collect_fids = \(PCSIs (si, sis)) -> mapM_ collect_fids $ si : sis

instance CollectFieldIds SimpleId where
  collect_fids = \sid -> modify (insert sid)


-- CollectNakedCases

instance CollectNakedCases Program where
  collect_ncs = \(P (pp, pps)) -> mapM_ collect_ncs $ pp : pps

instance CollectNakedCases ProgramPart where
  collect_ncs = \case
    TD td -> collect_ncs td
    _ -> do_nothing

instance CollectNakedCases TypeDef where
  collect_ncs = \case
    OTD1 otd -> collect_ncs otd
    _ -> do_nothing

instance CollectNakedCases OrTypeDef where
  collect_ncs = \(OTD (_, pv, pvs)) -> mapM_ collect_ncs $ pv : pvs

instance CollectNakedCases PossibleValue where
  collect_ncs = \(PV pv) -> case pv of
    (sid, Nothing) -> modify (insert sid)
    _ -> do_nothing


-- CollectParamTVars

instance CollectParamTVars SimpleType where
  collect_ptvs = \case
    PTV1 ptv -> modify (insert ptv)
    TAIOA1 taioa -> collect_ptvs taioa
    PoT1 pt -> collect_ptvs pt
    PT1 pt -> collect_ptvs pt
    FT1 ft -> collect_ptvs ft

instance CollectParamTVars TypeAppIdOrAHTV where
  collect_ptvs = \(TAIOA (mtip1, taioam, mtip2)) ->
    collect_ptvs mtip1 >> collect_ptvs taioam >> collect_ptvs mtip2

instance CollectParamTVars PowerType where
  collect_ptvs = \(PoT (pbt, _)) -> collect_ptvs pbt

instance CollectParamTVars ProdType where
  collect_ptvs = \(PT (ft, fts)) -> mapM_ collect_ptvs $ ft : fts

instance CollectParamTVars FuncType where
  collect_ptvs = \(FT (ioot1, ioot2)) ->
    collect_ptvs ioot1 >> collect_ptvs ioot2

instance CollectParamTVars (Maybe TypesInParen) where
  collect_ptvs = \case
    Nothing -> do_nothing
    Just tip -> collect_ptvs tip

instance CollectParamTVars TypesInParen where
  collect_ptvs = \(TIP (st, sts)) -> mapM_ collect_ptvs $ st : sts

instance CollectParamTVars TAIOAMiddle where
  collect_ptvs = \case
    TIdStart1 (_, tip_str_pairs) -> mapM_ collect_ptvs $ tip_str_pairs
    AHTV2 _ -> do_nothing

instance CollectParamTVars PowerBaseType where
  collect_ptvs = \case
    PTV2 ptv -> modify (insert ptv)
    TAIOA2 taioa -> collect_ptvs taioa
    IPT ipt -> collect_ptvs ipt

instance CollectParamTVars FieldType where
  collect_ptvs = \case
    PBT1 pbt -> collect_ptvs pbt
    PoT2 pt -> collect_ptvs pt

instance CollectParamTVars InOrOutType where
  collect_ptvs = \case
    PTV3 ptv -> modify (insert ptv)
    TAIOA3 taioa -> collect_ptvs taioa
    PoT4 pt -> collect_ptvs pt
    PT2 pt -> collect_ptvs pt
    FT2 ft -> collect_ptvs ft

instance CollectParamTVars (TypesInParen, String) where
  collect_ptvs = \(tip, _) -> collect_ptvs tip

instance CollectParamTVars InParenT where
  collect_ptvs = \case
    PT3 pt -> collect_ptvs pt
    PoT3 pt -> collect_ptvs pt
    FT3 ft -> collect_ptvs ft


-- CollectRenamingProps

instance CollectRenamingProps Program where
  collect_rps = \(P (pp, pps)) -> mapM_ collect_rps $ pp : pps

instance CollectRenamingProps ProgramPart where
  collect_rps = \case
    TPD tpd -> collect_rps tpd
    _ -> do_nothing

instance CollectRenamingProps TypePropDef where
  collect_rps = \case
    RPD1 rpd -> collect_rps rpd
    _ -> do_nothing

instance CollectRenamingProps RenamingPropDef where
  collect_rps = \(RPD (PNL pn_key, pn1, pns)) ->
    modify $ (:) (pn_key, pn1 : pns)

-- CollectOrValuesMap

init_or_val_map :: OrValuesMap
init_or_val_map =
  M.fromList $ map (\(s1, s2) -> (str_to_sid s1, str_to_id s2)) predefined

predefined :: [(String, String)]
predefined =
  [("a_value", "the_value"), ("error", "err"), ("result", "res")]

instance CollectOrValuesMap Program where
  collect_ovms = \(P (pp, pps)) -> mapM_ collect_ovms $ pp : pps

instance CollectOrValuesMap ProgramPart where
  collect_ovms = \case
    TD td -> collect_ovms td
    _ -> do_nothing

instance CollectOrValuesMap TypeDef where
  collect_ovms = \case
    OTD1 otd -> collect_ovms otd
    _ -> do_nothing

instance CollectOrValuesMap OrTypeDef where
  collect_ovms = \(OTD (_, pv, pvs)) -> mapM_ collect_ovms $ pv : pvs

instance CollectOrValuesMap PossibleValue where
  collect_ovms = \(PV (sid, maybe_id_st)) -> case maybe_id_st of
    Just (id, _) -> modify $ M.insert sid id
    _ -> do_nothing


-- Preprocess.hs
-- AST.hs
-- lcc.hs
