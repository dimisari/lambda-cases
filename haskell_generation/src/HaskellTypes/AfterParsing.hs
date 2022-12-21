{-# language LambdaCase #-}

module HaskellTypes.AfterParsing where

import HaskellTypes.Types
  ( ValueType(..) )
import HaskellTypes.Values
  ( BaseValue(..), ApplicationDirection(..), OneArgApplications(..) )
import HaskellTypes.Generation
  ( Stateful )

import CodeGenerators.Values
  ( base_value_g, base_value_type_inference_g )

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

application_tree_g = ( \vt@(AbsTypesAndResType abs_ts res_t) -> \case 
  Application at1 at2 -> 
    application_tree_type_inference_g at2 >>=
      \( vt2, hs2 ) ->
    application_tree_g (AbsTypesAndResType (vt_to_bt vt2 : abs_ts) res_t) at1 >>=
      \hs1 ->
    return $ "(" ++ hs1 ++ " " ++ hs2 ++ ")"
  BaseValueLeaf bv -> base_value_g vt bv
  ) :: ValueType -> ApplicationTree -> Stateful Haskell

vt_to_bt = undefined
bt_to_vt = undefined

application_tree_type_inference_g = ( \case 
  Application at1 at2 -> 
    application_tree_type_inference_g at1 >>=
      \( vt1@(AbsTypesAndResType abs_ts res_t), hs1 ) -> case abs_ts of 
    [] -> undefined
    abs_t:rest -> 
      application_tree_g (bt_to_vt abs_t) at2 >>= \hs2 ->
      return ( (AbsTypesAndResType rest res_t), "(" ++ hs1 ++ " " ++ hs2 ++ ")" ) 
  BaseValueLeaf bv -> base_value_type_inference_g bv
  ) :: ApplicationTree -> Stateful ( ValueType, Haskell )

valid_types = ( \vt1 vt2 res_vt -> undefined
  ) :: ValueType -> ValueType -> ValueType -> Stateful Bool

infer_valid_return_type = ( \vt1 vt2 -> undefined
  ) :: ValueType -> ValueType -> Stateful ValueType
