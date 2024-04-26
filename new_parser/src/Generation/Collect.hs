{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Generation.Collect where

import Data.Set
import Control.Monad.State

import ASTTypes
import Helpers

-- types
type FieldId = SimpleId

type FieldIds = Set FieldId

type NakedCase = SimpleId

type NakedCases = Set NakedCase

type FieldIdsState = State FieldIds ()

type NakedCasesState = State NakedCases ()

-- classes
class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

class CollectNakedCases a where
  collect_ncs :: a -> NakedCasesState

-- Eq and Ord for Set
deriving instance Eq IdStart
deriving instance Eq SimpleId

deriving instance Ord IdStart
deriving instance Ord SimpleId

--
field_ids :: Program -> FieldIds
field_ids = \prog -> execState (collect_fids prog) empty

naked_cases :: Program -> NakedCases
naked_cases = \prog -> execState (collect_ncs prog) empty

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

-- Test.hs
