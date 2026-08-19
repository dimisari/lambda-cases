module SyntaxTreeGen.TypesAndClasses where

import Prelude qualified as P

type Root = P.String
type Dot = P.String

class ToDot a where
  to_dot :: a -> (Root, Dot)

class AddNewRootCode a where
  add_new_root_code :: P.String -> a -> Dot
