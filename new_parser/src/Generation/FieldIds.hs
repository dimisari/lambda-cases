module Generation.FieldIds where

import ASTTypes

class FieldIds a where
  field_ids :: a -> [SimpleId]

instance FieldIds Program where 
  field_ids = \(P (pp, pps)) -> concatMap field_ids $ pp : pps

instance FieldIds ProgramPart where 
  field_ids = \case
    TD td -> field_ids td
    _ -> []

instance FieldIds TypeDef where 
  field_ids = \case
    TTD1 ttd -> field_ids ttd
    _ -> []

instance FieldIds TupleTypeDef where 
  field_ids = \(TTD (_, idt, _)) -> field_ids idt

instance FieldIds IdTuple where 
  field_ids = \(PCSIs (si, sis)) -> si : sis

