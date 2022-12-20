{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.Types
  ( ValueType(..) )
import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )
import HaskellTypes.Generation
  ( Stateful )

import CodeGenerators.Values
  ( base_value_type_inference_g )

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

application_tree_g = ( \vt at -> 
  application_tree_type_inference_g at >>= \( inferred_vt, hs ) ->
  case vt == inferred_vt of 
    True -> return hs
    False -> undefined
  ) :: ValueType -> ApplicationTree -> Stateful Haskell

application_tree_type_inference_g = (
  \(Application at1 at2) -> case ( at1, at2 ) of
    ( BaseValueLeaf bv1, BaseValueLeaf bv2 ) -> bv_bv_type_inference_g bv1 bv2
    ( app@(Application _ _), BaseValueLeaf bv ) -> app_bv_type_inference_g app bv
    ( BaseValueLeaf bv, app@(Application _ _ ) ) -> bv_app_type_inference_g bv app
    _ -> app_app_type_inference_g at1 at2
  ) :: ApplicationTree -> Stateful ( ValueType, Haskell )

bv_bv_type_inference_g = ( \bv1 bv2 -> 
  base_value_type_inference_g bv1 >>= \( vt1, hs1 ) ->
  base_value_type_inference_g bv2 >>= \( vt2, hs2 ) ->
  return ( undefined, "(" ++ hs1 ++ " " ++ hs2 ++ ")" ) 
  ) :: BaseValue -> BaseValue -> Stateful ( ValueType, Haskell )

app_bv_type_inference_g = ( \at bv ->
  application_tree_type_inference_g at >>= \( at_vt, at_hs ) ->
  base_value_type_inference_g bv >>= \( bv_vt, bv_hs ) ->
  return ( undefined, "(" ++ at_hs ++ " " ++ bv_hs ++ ")" ) 
  ) :: ApplicationTree -> BaseValue -> Stateful ( ValueType, Haskell )

bv_app_type_inference_g = ( \bv at ->
  base_value_type_inference_g bv >>= \( bv_vt, bv_hs ) ->
  application_tree_type_inference_g at >>= \( at_vt, at_hs ) ->
  return ( undefined, "(" ++ bv_hs ++ " " ++ at_hs ++ ")" ) 
  ) :: BaseValue -> ApplicationTree -> Stateful ( ValueType, Haskell )

app_app_type_inference_g = ( \at1 at2 ->
  application_tree_type_inference_g at1 >>= \( vt1, hs1 ) ->
  application_tree_type_inference_g at2 >>= \( vt2, hs2 ) ->
  return ( undefined, "(" ++ hs1 ++ " " ++ hs2 ++ ")" ) 
  ) :: ApplicationTree -> ApplicationTree -> Stateful ( ValueType, Haskell )
