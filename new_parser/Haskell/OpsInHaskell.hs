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
instance (a ~ b, Num a) => GeneralMultiplication a a b where
  (!*) = (*)

instance (a ~ b, Num a) => GeneralMultiplication a b a  where
  (!*) = (*)

instance (a ~ b, Num b) => GeneralMultiplication a b b where
  (!*) = (*)

-- P0'And'Add_To
instance (Show a, b ~ String) => P0'And'Add_To String a b where
  str !+ i = str ++ show i

instance P0'And'Add_To a [a] [a] where
  (!+) = (:)

instance P0'And'Add_To [a] a [a] where
  l !+ a = l ++ [a]

instance a ~ String => P0'And'Add_To String String a where
  (!+) = (++)

instance a ~ String => P0'And'Add_To a String String where
  (!+) = (++)

instance a ~ b => P0'And'Add_To a [b] [b] where
  (!+) = (:)

instance (a ~ b, Num a) => P0'And'Add_To a a b where
  (!+) = (+)

instance (a ~ b, Num a) => P0'And'Add_To a b a  where
  (!+) = (+)

instance (a ~ b, Num b) => P0'And'Add_To a b b where
  (!+) = (+)

instance (a ~ Int, b ~ Int) => P0'And'Add_To a b Int where
  (!+) = (+)

-- GeneralSubtraction
instance (a ~ b, Num a) => GeneralSubtraction a a b where
  (!-) = (-)

instance (a ~ b, Num a) => GeneralSubtraction a b a  where
  (!-) = (-)

instance (a ~ b, Num b) => GeneralSubtraction a b b where
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
