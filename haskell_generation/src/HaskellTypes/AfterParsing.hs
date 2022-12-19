{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.Types
  ( ValueType(..) )
import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )
import HaskellTypes.Generation
  ( Stateful )

import Helpers
  ( Haskell )

data ApplicationTree = 
  Application ApplicationTree ApplicationTree | BaseValueLeaf BaseValue

to_application_tree = ( \(OAA (bv_first, ad_first) bv_ads bv_last) ->
  case bv_ads of
    [] ->
      combine_with_direction
        (BaseValueLeaf bv_first) ad_first (BaseValueLeaf bv_last)
    (bv_head, ad_head):bv_ads_tail -> 
      combine_with_direction
        (BaseValueLeaf bv_first)
        ad_first
        (to_application_tree (OAA (bv_head, ad_head) bv_ads_tail bv_last))
  ) :: OneArgApplications -> ApplicationTree 

combine_with_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at1 at2
  RightApplication -> Application at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

application_tree_g = (
  undefined
  ) :: ValueType -> ApplicationTree -> Stateful Haskell
