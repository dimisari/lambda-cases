{-
This file contains:
- Type classes for implementing .1st, .2nd, ... and .change when translating to
  haskell. This is done by a class that defines a function that gives you the
  n-th element for any tuple that has size >= n (and <= 5 for now). Similarly,
  there is class that changes the n-th element. (For now n <= 3)
- Type classes for automatically calling constructors in the translation of
  tuples. This is done by a class that defines a function from a product type
  of size n to any equivalent tuple type (and it is implemented automatically
  during the translation of the tuple type by calling its constructor). This
  allows to simply call that function when translating tuples without needing
  to know to which of potentially many equivalent tuple types it belongs to
  (or of it's simply a tuple of the equivalent product type).
-}

{-# language
  FunctionalDependencies, UndecidableInstances, IncoherentInstances
#-}

module Predefined.Classes where

import Prelude qualified as P

import Prelude ((++))
import Predefined.Operators ((.>))
import Predefined.Types qualified as PT
import Predefined.UTF8IO qualified as PU

-- IsFirst'

class IsFirst' a b | b -> a where
  p1st :: b -> a

instance IsFirst' a (a, b) where
  p1st = P.fst

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
  p2nd = P.snd

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

-- FromTuple instances for regular tuples

instance FromTuple2 a b (a, b) where
  ft2 = P.id

instance FromTuple3 a b c (a, b, c) where
  ft3 = P.id

instance FromTuple4 a b c d (a, b, c, d) where
  ft4 = P.id

instance FromTuple5 a b c d e (a, b, c, d, e) where
  ft5 = P.id

-- Isolate the "pure" function into a new type class and call it "wrap"

class A'Has_A_Wrapper t where
  wrap' :: a -> t a

instance P.Applicative f => A'Has_A_Wrapper f where
  wrap' = P.pure

-- Renaming functor

class A'Has_Internal_App t where
  apply'inside' :: (a -> b, t a) -> t b

instance P.Functor f => A'Has_Internal_App f where
  apply'inside' = P.uncurry P.fmap

-- Print class

class Print a where
  print' :: a -> PT.Program

instance {- OVERLAPS -} P.Show P.String where
  show = P.id

instance P.Show a => Print a where
  print' = P.show .> (++ "\n") .> PU.print_string'

