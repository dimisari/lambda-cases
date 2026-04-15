module Preprocessing.TypesAndClasses where

import Prelude qualified as P
import Data.Map.Strict qualified as M
import Control.Monad.State qualified as MS
import Data.Set qualified as S

import ASTTypes qualified as T

-- Collect types

type FieldId = T.SimpleId

type FieldIds = S.Set FieldId

type FieldIdsState = MS.State FieldIds ()

type RenamingProp = (T.PropName, [T.PropName])

type RenamingProps = [RenamingProp]

type RenamingPropsState = MS.State RenamingProps ()

type OrValue = T.SimpleId

type EmptyOrValues = S.Set OrValue

type FullOrValuesMap = M.Map OrValue T.Identifier

type OrValues = (EmptyOrValues, FullOrValuesMap)

type OrValuesState = MS.State OrValues ()

type ParamTVars = S.Set T.ParamTVar

type ParamTVarsState = MS.State ParamTVars ()

-- Collect classes

class CollectFieldIds a where
  collect_fids :: a -> FieldIdsState

class CollectRenamingProps a where
  collect_rps :: a -> RenamingPropsState

class CollectOrValues a where
  collect_ovms :: a -> OrValuesState

class CollectParamTVars a where
  collect_ptvs :: a -> ParamTVarsState

-- CheckCompatibility types

type AHTVMap = M.Map T.AdHocTVar T.SubOrUnder

type NP_TIP_Pair = (T.NamePart, T.TypesInParen)

type NP_SIP_Pair = (T.NamePart, T.SubsInParen)

type TIP_NP_Pair = (T.TypesInParen, T.NamePart)

type SIP_NP_Pair = (T.SubsInParen, T.NamePart)

type TIP_STR = (T.TypesInParen, P.String)

type SOUIP_STR = (T.SubsOrUndersInParen, P.String)

type WAHTVMap a = MS.State AHTVMap a

data Compatibility =
  Compatible AHTVMap | NotCompatible

-- CheckCompatibility classes

class CheckCompatibility a b where
  check_compat :: (a, b) -> Compatibility

class AddSubs a b where
  add_subs :: a -> WAHTVMap b

-- Preprocess types

data PossiblyInDC =
  InDotChange [T.PostFuncArg] | NotInDotChange

type StateTuple =
  ( PossiblyInDC, FieldIds, EmptyOrValues, RenamingProps
  , FullOrValuesMap
  )

type PreprocessState = MS.State StateTuple

type CompatAndPNs = (Compatibility, [T.PropName])

-- Preprocess classes

class Preprocess a where
  preprocess :: a -> PreprocessState a

class ToMaybePostFuncApp a where
  to_maybe_post_func_app :: a -> PreprocessState (P.Maybe T.PostFuncApp)

