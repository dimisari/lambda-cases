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
false = False

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
  a0'first :: b -> a

instance IsFirst a [a] where
  a0'first = head

class IsSecond a b | b -> a where
  a0'second :: b -> a

instance IsSecond a [a] where
  a0'second = head . tail 

-- IsFirst'

class IsFirst' a b | b -> a where
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

class IsSecond' a b | b -> a where
  b1second :: b -> a

instance IsSecond' b (a, b) where
  b1second = snd

instance IsSecond' b (a, b, c) where
  b1second = \(_, b, _) -> b

instance IsSecond' b (a, b, c, d) where
  b1second = \(_, b, _, _) -> b

instance IsSecond' b (a, b, c, d, e) where
  b1second = \(_, b, _, _, _) -> b

-- IsThird'

class IsThird' a b | b -> a where
  b1third :: b -> a

instance IsThird' c (a, b, c) where
  b1third = \(_, _, c) -> c

instance IsThird' c (a, b, c, d) where
  b1third = \(_, _, c, _) -> c

instance IsThird' c (a, b, c, d, e) where
  b1third = \(_, _, c, _, _) -> c

-- ChangeFirst'

class ChangeFirstTo' a b | b -> a where
  c1first :: a -> b -> b

instance ChangeFirstTo' a (a, b) where
  c1first = \a (_, b) -> (a, b)

instance ChangeFirstTo' a (a, b, c) where
  c1first = \a (_, b, c) -> (a, b, c)

instance ChangeFirstTo' a (a, b, c, d) where
  c1first = \a (_, b, c, d) -> (a, b, c, d)

instance ChangeFirstTo' a (a, b, c, d, e) where
  c1first = \a (_, b, c, d, e) -> (a, b, c, d, e)

-- ChangeSecond'

class ChangeSecondTo' a b | b -> a where
  c1second :: a -> b -> b

instance ChangeSecondTo' b (a, b) where
  c1second = \b (a, _) -> (a, b)

instance ChangeSecondTo' b (a, b, c) where
  c1second = \b (a, _, c) -> (a, b, c)

instance ChangeSecondTo' b (a, b, c, d) where
  c1second = \b (a, _, c, d) -> (a, b, c, d)

instance ChangeSecondTo' b (a, b, c, d, e) where
  c1second = \b (a, _, c, d, e) -> (a, b, c, d, e)

-- ChangeThird'

class ChangeThirdTo' a b | b -> a where
  c1third :: a -> b -> b

instance ChangeThirdTo' c (a, b, c) where
  c1third = \c (a, b, _) -> (a, b, c)

instance ChangeThirdTo' c (a, b, c, d) where
  c1third = \c (a, b, _, d) -> (a, b, c, d)

instance ChangeThirdTo' c (a, b, c, d, e) where
  c1third = \c (a, b, _, d, e) -> (a, b, c, d, e)

