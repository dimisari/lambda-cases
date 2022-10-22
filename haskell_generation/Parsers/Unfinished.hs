--NameTypeAndValueLists

data NameTypeAndValueLists = NTAVL
  { get_names :: [ NameExpression ], get_types :: [ TypeExpression ]
  , get_values :: [ ValueExpression ] }
  deriving (Eq, Show)

-- Probably going to need 

to_NTAV_list = undefined
  :: NameTypeAndValueLists -> [ NameTypeAndValueExpression ]
