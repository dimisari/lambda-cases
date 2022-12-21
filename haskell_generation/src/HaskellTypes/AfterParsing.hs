{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )

data ApplicationTree = 
  Application ApplicationTree ApplicationTree | BaseValueLeaf BaseValue

to_application_tree = ( \(OAA (bv_first, ad_first) bv_ads bv_last) ->
  case bv_ads of
    [] ->
      combine_with_direction
        (BaseValueLeaf bv_first) ad_first (BaseValueLeaf bv_last)
    (bv_head, ad_head):[] -> 
      combine_with_direction
        (combine_with_direction
          (BaseValueLeaf bv_first) ad_first (BaseValueLeaf bv_head) )
        ad_head
        (BaseValueLeaf bv_last)
    (bv_head, ad_head):bv_ad_2:bv_ads_tail -> 
      combine_with_direction
        (combine_with_direction
          (BaseValueLeaf bv_first) ad_first (BaseValueLeaf bv_head))
        ad_head
        (to_application_tree $ OAA bv_ad_2 bv_ads_tail bv_last)
  ) :: OneArgApplications -> ApplicationTree 

combine_with_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at1 at2
  RightApplication -> Application at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree
