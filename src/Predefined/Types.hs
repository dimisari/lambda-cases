module Predefined.Types where

import Prelude qualified as P
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Control.Monad.State qualified as MS

type ProgramWith' = P.IO
type EmptyVal = ()
type Program = ProgramWith' EmptyVal
type ListOf's = []
type State'With' a b = MS.State a b
type State' a = State'With' a EmptyVal
type Possibly' = P.Maybe
type Result'OrError' a b = P.Either b a
type Z = P.Integer
type R = P.Double
type SMapTo' = HM.HashMap P.String
type ArrayOf's = IM.IntMap
type Strings = ListOf's P.String

