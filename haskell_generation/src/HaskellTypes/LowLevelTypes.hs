module HaskellTypes.LowLevelTypes where

-- All: Types, Show instances

-- Types: TypeName

newtype TypeName =
  TN String deriving (Eq, Ord)

newtype LeftTypeInputs = 
  LeftTypeInputs [ TypeName ]

newtype RightTypeInputs = 
  RightTypeInputs [ TypeName ]

data TypeExpression =
  NameAndTypeInputs TypeName LeftTypeInputs RightTypeInputs

-- Show instances: TypeName

instance Show TypeName where
  show = \(TN name) -> name
