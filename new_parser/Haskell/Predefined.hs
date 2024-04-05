{-# language
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
#-}
module Haskell.Predefined where

import Prelude hiding (IO)
import qualified Prelude as P

type FromIO = P.IO
type IO = FromIO ()
type ListOf's = []

a'div' :: Integral a => (a, a) -> a
a'div' = uncurry div

a'mod' :: Integral a => (a, a) -> a
a'mod' = uncurry mod

print' :: Show a => a -> IO
print' = print

print_line' = putStrLn
get_line = getLine
split'to_words = words
apply'to_all = map
throw_err' = error
true = True

drop'from' :: (Int, [a]) -> [a]
drop'from' = uncurry drop

end_io :: IO
end_io = return ()

from_string' :: Read a => String -> a
from_string' = read

wrap :: Monad m => a -> m a
wrap = return

a'from_io :: a -> P.IO a
a'from_io = return

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
