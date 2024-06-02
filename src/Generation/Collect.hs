{-# LANGUAGE LambdaCase, StandaloneDeriving, FlexibleInstances #-}

module Generation.Collect where

import Data.Set
import Control.Monad.State

import ASTTypes
import Helpers

-- types
type FieldId = SimpleId

type NakedCase = SimpleId

type FieldIds = Set FieldId

type NakedCases = Set NakedCase

type ParamTVars = Set ParamTVar

type FieldIdsState = State FieldIds ()

type NakedCasesState = State NakedCases ()

type ParamTVarsState = State ParamTVars ()

-- classes
class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

class CollectNakedCases a where
  collect_ncs :: a -> NakedCasesState

class CollectParamTVars a where
  collect_ptvs :: a -> ParamTVarsState

-- Eq and Ord for Set
deriving instance Eq IdStart
deriving instance Eq SimpleId
deriving instance Eq ParamTVar

deriving instance Ord IdStart
deriving instance Ord SimpleId
deriving instance Ord ParamTVar

--
field_ids :: Program -> FieldIds
field_ids = \prog -> execState (collect_fids prog) empty

naked_cases :: Program -> NakedCases
naked_cases = \prog -> execState (collect_ncs prog) empty

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
  collect_fids = \(TTD (_, idt, _)) -> collect_fids idt

instance CollectFieldIds IdTuple where
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
  collect_ncs = \(OTD (_, sid, mst, sid_mst_pairs)) ->
    mapM_ collect_ncs $ (sid, mst) : sid_mst_pairs

instance CollectNakedCases (SimpleId, Maybe SimpleType) where
  collect_ncs = \case
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
    PoT3 pt -> collect_ptvs pt

instance CollectParamTVars InOrOutType where
  collect_ptvs = \case
    PTV3 ptv -> modify (insert ptv)
    TAIOA3 taioa -> collect_ptvs taioa
    PoT2 pt -> collect_ptvs pt
    PT2 pt -> collect_ptvs pt
    FT2 ft -> collect_ptvs ft

instance CollectParamTVars (TypesInParen, String) where
  collect_ptvs = \(tip, _) -> collect_ptvs tip

instance CollectParamTVars InParenT where
  collect_ptvs = \case
    PT3 pt -> collect_ptvs pt
    FT3 ft -> collect_ptvs ft

-- Preprocess.hs
-- AST.hs
-- Test.hs
