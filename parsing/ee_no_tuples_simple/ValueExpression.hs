module ValueExpression where

import Prelude (Eq, Show, String, undefined)
import Text.Parsec ()
import Text.Parsec.String (Parser)
import AtomicExpression (AtomicExpression)
import TypeExpression (TypeExpression)

-- ValueExpression

data ValueExpression =
  Abstraction String ValueExpression |
  Case [ ( ValueExpression, ValueExpression ) ] |
  InteremediatesOutput [ NameTypeAndValue ]
  deriving (Eq, Show)

value_expression_p = undefined
  :: Parser ValueExpression

-- NameTypeAndValue

data NameTypeAndValue = NTAV
  { get_name :: String, get_type :: TypeExpression, get_value :: ValueExpression }
  deriving (Eq, Show)

--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ String ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValue ]
