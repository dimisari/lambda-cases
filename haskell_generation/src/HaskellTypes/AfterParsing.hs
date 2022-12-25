{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )

data ApplicationTree = 
  Application ApplicationTree ApplicationTree | BaseValueLeaf BaseValue
  deriving Show

to_application_tree = ( \(OAA bv_ad bv_ads bv_last) ->
  to_application_tree_help bv_last (reverse $ bv_ad : bv_ads )
  ) :: OneArgApplications -> ApplicationTree 

to_application_tree_help = ( \prev_bv -> \case
  [] -> BaseValueLeaf prev_bv
  ( bv, ad ):bv_ads -> 
    combine_with_reverse_direction
      (BaseValueLeaf prev_bv) ad (to_application_tree_help bv bv_ads)
  ) :: BaseValue -> [ ( BaseValue, ApplicationDirection ) ] -> ApplicationTree 

combine_with_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at1 at2
  RightApplication -> Application at2 at1
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree

combine_with_reverse_direction = ( \at1 ad at2 -> case ad of 
  LeftApplication -> Application at2 at1
  RightApplication -> Application at1 at2
  ) :: ApplicationTree -> ApplicationDirection -> ApplicationTree ->
       ApplicationTree
