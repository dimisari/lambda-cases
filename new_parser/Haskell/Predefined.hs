{-# language
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies
#-}
module Haskell.Predefined where

import Prelude hiding (IO)
import qualified Prelude as P

type FromIO = P.IO
type IO = FromIO ()
type ListOf's = []

a0'div' :: Integral a => (a, a) -> a
a0'div' = uncurry div

a0'mod' :: Integral a => (a, a) -> a
a0'mod' = uncurry mod

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

a0'from_io :: a -> P.IO a
a0'from_io = return

class IsFirst a b | b -> a where
  first :: b -> a

instance IsFirst a [a] where
  first = head

class IsSecond a b | b -> a where
  second :: b -> a

instance IsSecond a [a] where
  second = head . tail 

-- IsFirst'

class IsFirst' a b where
  b1first :: b -> a

instance IsFirst' a (a, b) where
  b1first = fst

instance IsFirst' a (a, b, c) where
  b1first = \(a, _, _) -> a

instance IsFirst' a (a, b, c, d) where
  b1first = \(a, _, _, _) -> a

instance IsFirst' a (a, b, c, d, e) where
  b1first = \(a, _, _, _, _) -> a

-- IsSecond'

class IsSecond' a b where
  b1second :: b -> a

instance IsSecond' b (a, b) where
  b1second = snd

instance IsSecond' b (a, b, c) where
  b1second = \(_, b, _) -> b

instance IsSecond' b (a, b, c, d) where
  b1second = \(_, b, _, _) -> b

instance IsSecond' b (a, b, c, d, e) where
  b1second = \(_, b, _, _, _) -> b

-- IsSecond'

class IsThird' a b where
  b1third :: b -> a

instance IsThird' c (a, b, c) where
  b1third = \(_, _, c) -> c

instance IsThird' c (a, b, c, d) where
  b1third = \(_, _, c, _) -> c

instance IsThird' c (a, b, c, d, e) where
  b1third = \(_, _, c, _, _) -> c

