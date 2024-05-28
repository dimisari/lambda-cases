{-# language
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,
  UndecidableInstances
#-}
module Haskell.Predefined where

import Prelude hiding (IO)
import qualified Prelude as P
import Control.Monad.State

-- types
type A1FromIO = P.IO
type IO = A1FromIO ()
type ListOf1s = []
type A1FState1Man a b = State b a
type State1Man a = A1FState1Man () a

-- values
print_line1a = putStrLn
get_line = getLine
split1to_words = words
apply1to_all_in1a = uncurry map
throw_err1a = error

get_state :: State a a
get_state = get

set_state1a :: s -> State s ()
set_state1a = put

modify_state_with1a :: (s -> s) -> State s ()
modify_state_with1a = modify

result_of1on_init_state1a :: (State s a, s) -> a
result_of1on_init_state1a = uncurry evalState

final_state_of1on_init_state1a :: (State s a, s) -> s
final_state_of1on_init_state1a = uncurry execState

run1on_init_state1a :: (State s a, s) -> (a, s)
run1on_init_state1a = uncurry runState

a1div1a :: Integral a => (a, a) -> a
a1div1a = uncurry div

a1mod1a :: Integral a => (a, a) -> a
a1mod1a = uncurry mod

print1a :: Show a => a -> IO
print1a = print

a1length :: [a] -> Int
a1length = length

a1is_in1a :: Eq a => (a, [a]) -> Bool
a1is_in1a = uncurry elem

ignore1from1a :: (Int, [a]) -> [a]
ignore1from1a = uncurry drop

take1from1a :: (Int, [a]) -> [a]
take1from1a = uncurry take

do_nothing :: Applicative f => f ()
do_nothing = pure ()

from_string1a :: Read a => String -> a
from_string1a = read

wrap :: Monad m => a -> m a
wrap = return

a1from_io :: a -> P.IO a
a1from_io = return

not1a :: Bool -> Bool
not1a = not

for_all_in2a :: Monad m => ([a], a -> m b) -> m ()
for_all_in2a = uncurry $ flip mapM_

-- IsFirst'
class IsFirst' a b | b -> a where
  p1st :: b -> a

instance IsFirst' a (a, b) where
  p1st = fst

instance IsFirst' a (a, b, c) where
  p1st = \(a, _, _) -> a

instance IsFirst' a (a, b, c, d) where
  p1st = \(a, _, _, _) -> a

instance IsFirst' a (a, b, c, d, e) where
  p1st = \(a, _, _, _, _) -> a

-- IsSecond'
class IsSecond' a b | b -> a where
  p2nd :: b -> a

instance IsSecond' b (a, b) where
  p2nd = snd

instance IsSecond' b (a, b, c) where
  p2nd = \(_, b, _) -> b

instance IsSecond' b (a, b, c, d) where
  p2nd = \(_, b, _, _) -> b

instance IsSecond' b (a, b, c, d, e) where
  p2nd = \(_, b, _, _, _) -> b

-- IsThird'
class IsThird' a b | b -> a where
  p3rd :: b -> a

instance IsThird' c (a, b, c) where
  p3rd = \(_, _, c) -> c

instance IsThird' c (a, b, c, d) where
  p3rd = \(_, _, c, _) -> c

instance IsThird' c (a, b, c, d, e) where
  p3rd = \(_, _, c, _, _) -> c

-- ChangeFirst'
class ChangeFirstTo' a b | b -> a where
  c1st :: a -> b -> b

instance ChangeFirstTo' a (a, b) where
  c1st = \a (_, b) -> (a, b)

instance ChangeFirstTo' a (a, b, c) where
  c1st = \a (_, b, c) -> (a, b, c)

instance ChangeFirstTo' a (a, b, c, d) where
  c1st = \a (_, b, c, d) -> (a, b, c, d)

instance ChangeFirstTo' a (a, b, c, d, e) where
  c1st = \a (_, b, c, d, e) -> (a, b, c, d, e)

-- ChangeSecond'
class ChangeSecondTo' a b | b -> a where
  c2nd :: a -> b -> b

instance ChangeSecondTo' b (a, b) where
  c2nd = \b (a, _) -> (a, b)

instance ChangeSecondTo' b (a, b, c) where
  c2nd = \b (a, _, c) -> (a, b, c)

instance ChangeSecondTo' b (a, b, c, d) where
  c2nd = \b (a, _, c, d) -> (a, b, c, d)

instance ChangeSecondTo' b (a, b, c, d, e) where
  c2nd = \b (a, _, c, d, e) -> (a, b, c, d, e)

-- ChangeThird'
class ChangeThirdTo' a b | b -> a where
  c3rd :: a -> b -> b

instance ChangeThirdTo' c (a, b, c) where
  c3rd = \c (a, b, _) -> (a, b, c)

instance ChangeThirdTo' c (a, b, c, d) where
  c3rd = \c (a, b, _, d) -> (a, b, c, d)

instance ChangeThirdTo' c (a, b, c, d, e) where
  c3rd = \c (a, b, _, d, e) -> (a, b, c, d, e)

-- FromTuple classes
class FromTuple2 a b c | c -> a b where
  ft2 :: (a, b) -> c

class FromTuple3 a b c d | d -> a b c where
  ft3 :: (a, b, c) -> d

class FromTuple4 a b c d e | e -> a b c d where
  ft4 :: (a, b, c, d) -> e

class FromTuple5 a b c d e f | f -> a b c d e where
  ft5 :: (a, b, c, d, e) -> f

-- FromTuple classes for regular tuples
instance FromTuple2 a b (a, b) where
  ft2 = id

instance FromTuple3 a b c (a, b, c) where
  ft3 = id

instance FromTuple4 a b c d (a, b, c, d) where
  ft4 = id

instance FromTuple5 a b c d e (a, b, c, d, e) where
  ft5 = id

-- MyShow class
instance {-# OVERLAPS #-} Show String where
  show = id

--
class A1Has_A_Wrapper t where
  wrap1a :: a -> t a

instance Applicative f => A1Has_A_Wrapper f where
  wrap1a = pure

--
class A1Has_Internal_App t where
  apply1inside1a :: (a -> b, t a) -> t b

instance Applicative f => A1Has_Internal_App f where
  apply1inside1a = uncurry fmap

