{-# LANGUAGE
  MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
  UndecidableInstances, FunctionalDependencies, GADTs, IncoherentInstances
#-}

module Haskell.OpsInHaskell where

infixl 9 &>
infixr 8 <&
infixl 7 .>, <.
infixr 6 !^
infixl 5 !*, !/
infixl 4 !+, !-
infix 3 !==, !!=, !>, !<, !>=, !<=
infixr 2 !&
infixr 1 !|
infixr 0 !>>=, !>>

(&>) :: a -> (a -> b) -> b
x &> f = f x

(<&) :: (a -> b) -> a -> b
f <& x = f x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)

class GeneralExponentiation a b c where
  (!^) :: a -> b -> c

class GeneralMultiplication a b c where
  (!*) :: a -> b -> c
class GeneralDivision a b c where
  (!/) :: a -> b -> c

class P0'And'Add_To a b c where
  (!+) :: a -> b -> c
class GeneralSubtraction a b c where
  (!-) :: a -> b -> c

class GeneralEquality a b where
  (!==) :: a -> b -> Bool
class GeneralInequality a b where
  (!!=) :: a -> b -> Bool
class GeneralGreaterThan a b where
  (!>) :: a -> b -> Bool
class GeneralLessThan a b where
  (!<) :: a -> b -> Bool
class GeneralGreaterEqual a b where
  (!>=) :: a -> b -> Bool
class GeneralLessEqual a b where
  (!<=) :: a -> b -> Bool

class HasAnd a where
  (!&) :: a -> a -> a

class HasOr a where
  (!|) :: a -> a -> a

class HasUse u where
  (!>>=) :: u a -> (a -> u b) -> u b

class HasThen t where
  (!>>) :: t a -> t b -> t b

-- GeneralMultiplication
instance (a ~ Int) => GeneralMultiplication Int Int a where
  (!*) = (*)

-- P0'And'Add_To
instance P0'And'Add_To String [Int] String where
  str !+ l = str ++ show l

instance (a ~ String) => P0'And'Add_To String Int a where
  str !+ i = str ++ show i

instance (a ~ String) => P0'And'Add_To String (Int, Int, Int) a where
  str !+ i = str ++ show i

instance P0'And'Add_To a [a] [a] where
  (!+) = (:)

instance (a ~ String) => P0'And'Add_To String String a where
  (!+) = (++)

instance (a ~ Int) => P0'And'Add_To Int Int a where
  (!+) = (+)

instance (a ~ Int) => P0'And'Add_To a Int Int where
  (!+) = (+)

-- GeneralSubtraction
instance Num a => GeneralSubtraction a a a where
  (!-) = (-)

-- GeneralEquality
instance Eq a => GeneralEquality a a where
  (!==) = (==)

-- GeneralGreaterThan
instance Ord a => GeneralGreaterThan a a where
  (!>) = (>)

-- GeneralLessThan
instance Ord a => GeneralLessThan a a where
  (!<) = (<)

-- HasThen
instance Applicative f => HasThen f where
  (!>>) = (*>)

-- HasThen
instance Monad m => HasUse m where
  (!>>=) = (>>=)
