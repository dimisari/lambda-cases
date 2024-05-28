{-# LANGUAGE
  MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
  UndecidableInstances, FunctionalDependencies, GADTs, IncoherentInstances
#-}

module Haskell.OpsInHaskell where

import Haskell.Predefined

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

class A1And1Add_To a b c where
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

class A1Has_Use u where
  (!>>=) :: u a -> (a -> u b) -> u b

class A1Has_Then t where
  (!>>) :: t a -> t b -> t b

-- GeneralMultiplication
instance (a ~ b, Num a) => GeneralMultiplication a a b where
  (!*) = (*)

instance (a ~ b, Num a) => GeneralMultiplication a b a  where
  (!*) = (*)

instance (a ~ b, Num b) => GeneralMultiplication a b b where
  (!*) = (*)

-- A1And1Add_To
instance Show a => A1And1Add_To a String String where
  x !+ str = show x ++ str

instance (Show a, b ~ String) => A1And1Add_To String a b where
  str !+ x = str ++ show x

instance (Show a, b ~ String) => A1And1Add_To a String b where
  x !+ str = show x ++ str

instance b ~ [a] => A1And1Add_To [a] [a] b where
  (!+) = (++)

instance b ~ [a] => A1And1Add_To a [a] b where
  (!+) = (:)

instance b ~ [a] => A1And1Add_To [a] a b where
  l !+ a = l ++ [a]

instance a ~ b => A1And1Add_To a [b] [b] where
  (!+) = (:)

instance (a ~ b, Num a) => A1And1Add_To a a b where
  (!+) = (+)

instance (a ~ b, Num a) => A1And1Add_To a b a  where
  (!+) = (+)

instance (a ~ b, Num b) => A1And1Add_To a b b where
  (!+) = (+)

instance (a ~ Int, b ~ Int) => A1And1Add_To a b Int where
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

instance (Int ~ a) => GeneralLessThan Int a where
  (!<) = (<)

-- A1Has_Then
instance Applicative f => A1Has_Then f where
  (!>>) = (*>)

-- A1Has_Use
instance Monad m => A1Has_Use m where
  (!>>=) = (>>=)

-- HasOr
instance HasOr Bool where
  (!|) = (||)

