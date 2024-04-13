{-# LANGUAGE StandaloneDeriving #-}

module Generation.FieldIds where

import Data.Set

import ASTTypes
import Helpers

type FieldId = SimpleId

deriving instance Eq IdStart
deriving instance Eq SimpleId 

deriving instance Ord IdStart
deriving instance Ord SimpleId 

type FieldIds = Set FieldId

field_ids_set :: Program -> Set FieldId
field_ids_set = \(P (pp, pps)) -> (pp : pps) &> concatMap field_ids &> fromList

class GetFieldIds a where
  field_ids :: a -> [FieldId]

instance GetFieldIds ProgramPart where 
  field_ids = \case
    TD td -> field_ids td
    _ -> []

instance GetFieldIds TypeDef where 
  field_ids = \case
    TTD1 ttd -> field_ids ttd
    _ -> []

instance GetFieldIds TupleTypeDef where 
  field_ids = \(TTD (_, idt, _)) -> field_ids idt

instance GetFieldIds IdTuple where 
  field_ids = \(PCSIs (si, sis)) -> si : sis

