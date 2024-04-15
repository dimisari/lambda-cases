{-# LANGUAGE StandaloneDeriving #-}

module Generation.FieldIds where

import Data.Set
import Control.Monad.State

import ASTTypes
import Helpers
import Parsing.Test

-- types and class
type FieldId = SimpleId

type FieldIds = Set FieldId

type FieldIdsState = State FieldIds ()

class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

-- Eq and Ord for Set
deriving instance Eq IdStart
deriving instance Eq SimpleId

deriving instance Ord IdStart
deriving instance Ord SimpleId

-- helper
do_nothing :: FieldIdsState
do_nothing = return ()

--

get_field_ids :: Program -> Set FieldId
get_field_ids = \prog -> execState (collect_fids prog) empty

-- instances

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
  collect_fids = \si -> modify (insert si)

-- testing

in_file :: FilePath
in_file =
  "/home/gnostis/Desktop/lambda-cases/new_parser/inputs/programs/" ++
  "gcd.lc"

test_parse :: String -> Program
test_parse = parse .> \case
  Left err -> error $ show err
  Right res -> res

test :: IO ()
test = readFile in_file >>= test_parse .> get_field_ids .> print
