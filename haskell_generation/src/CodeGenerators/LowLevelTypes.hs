module CodeGenerators.LowLevelTypes where

import Helpers (Haskell, (.>))

import HaskellTypes.LowLevelTypes 

-- All: TypeName, LeftTypeInputs, RightTypeInputs, TypeApplication

-- TypeName: type_name_p
 
type_name_g = show
  :: TypeName -> Haskell

-- LeftTypeInputs:

left_type_inputs_g = ( \(LeftTypeInputs type_names) ->
  concatMap (show .> (" " ++)) type_names
  ) :: LeftTypeInputs -> Haskell

-- RightTypeInputs:

right_type_inputs_g = ( \(RightTypeInputs type_names) ->
  concatMap (show .> (" " ++)) type_names
  ) :: RightTypeInputs -> Haskell

-- TypeApplication

type_application_g = ( \(NameAndTypeInputs t_name left_t_inputs right_t_inputs) ->
  type_name_g t_name ++
    left_type_inputs_g left_t_inputs ++ right_type_inputs_g right_t_inputs
  ) :: TypeApplication -> Haskell
