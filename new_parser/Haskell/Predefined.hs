{-# language
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
#-}
module Haskell.Predefined where

import Prelude hiding (IO)
import qualified Prelude as P

type FromIO = P.IO
type IO = FromIO ()
type ListOf's = []

print_line = putStrLn
get_line = getLine
split_words = words
apply'to_all = map
throw_err = error
true = True
drop'from :: (Int, [a]) -> [a]
drop'from = uncurry drop

end_io :: IO
end_io = return ()

from_string :: Read a => String -> a
from_string = read

wrap :: Monad m => a -> m a
wrap = return

wrap_in_io :: a -> P.IO a
wrap_in_io = return

class IsFirst a b | b -> a where
  first :: b -> a

instance IsFirst a [a] where
  first = head

class IsSecond a b | b -> a where
  second :: b -> a

instance IsSecond a [a] where
  second = head . tail 

class IsFirst' a b where
  first' :: b -> a

class IsSecond' a b where
  second' :: b -> a
