{-
This file defines equivalents of every lcases operator for haskell:
- precedence and associativity
- definitions or type classes according to the operator
- particular instances for operators defined by a type class
-}

{-# language
  MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
  UndecidableInstances, FunctionalDependencies, GADTs, IncoherentInstances
#-}

module PredefImports.OpsInHaskell where

import Prelude ((.), (^), (**), (*), ($), (/), (++), (+), (-), (==), (/=), (>))
import Prelude ((<), (>=), (<=), (*>), (>>=), (&&), (||))
import Prelude qualified as P

-- precedence and associativity

infixl 9 &>
infixr 8 <&
infixl 7 .>, <.
infixr 6 !^
infixl 5 !*, !/
infixl 4 !+, !-
infix 3 !==, !!=, !>, !<, !>=, !<=
infixr 2 !&
infixr 1 !|
infixl 0 !>>=, !>>

-- function application/composition

(&>) :: a -> (a -> b) -> b
x &> f = f x

(<&) :: (a -> b) -> a -> b
f <& x = f x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = P.flip (.)

(<.) :: (b -> c) -> (a -> b) -> a -> c
(<.) = (.)

-- arithmetic operators

class A'To_The'Is' a b c | a b -> c where
  (!^) :: a -> b -> c

class A'And'Multiply_To' a b c | a b -> c where
  (!*) :: a -> b -> c

class A'Divided_By'Is' a b c | a b -> c  where
  (!/) :: a -> b -> c

class A'And'Add_To' a b c | a b -> c where
  (!+) :: a -> b -> c

class A'Minus'Is' a b c | a b -> c where
  (!-) :: a -> b -> c

-- comparison operators

class A'And'Can_Be_Equal a b where
  (!==) :: a -> b -> P.Bool

class A'And'Can_Be_Unequal a b where
  (!!=) :: a -> b -> P.Bool

class A'Can_Be_Greater_Than' a b where
  (!>) :: a -> b -> P.Bool

class A'Can_Be_Less_Than' a b where
  (!<) :: a -> b -> P.Bool

class A'Can_Be_Gr_Or_Eq_To' a b where
  (!>=) :: a -> b -> P.Bool

class A'Can_Be_Le_Or_Eq_To' a b where
  (!<=) :: a -> b -> P.Bool

-- boolean operators

class A'Has_And a where
  (!&) :: a -> a -> a

class A'Has_Or a where
  (!|) :: a -> a -> a

-- environment operators

class A'Has_Use u where
  (!>>=) :: u a -> (a -> u b) -> u b

class A'Has_Then t where
  (!>>) :: t a -> t b -> t b

-- Instances
--   A'To_The'Is'

instance P.Floating a => A'To_The'Is' a a a where
  (!^) = (**)

instance A'To_The'Is' P.Integer P.Integer P.Integer where
  (!^) = (^)

instance A'To_The'Is' P.Integer P.Double P.Double where
  i !^ x = P.fromIntegral i ** x

instance A'To_The'Is' P.Double P.Integer P.Double where
  x !^ i = x ** P.fromIntegral i

--   A'And'Multiply_To'

instance P.Num a => A'And'Multiply_To' a a a where
  (!*) = (*)

instance A'And'Multiply_To' P.Integer P.Double P.Double where
  i !* x  = P.fromIntegral i * x

instance A'And'Multiply_To' P.Double P.Integer P.Double where
  x !* i = x * P.fromIntegral i

instance A'And'Multiply_To' P.Integer P.Char P.String where
  i !* c = P.replicate (P.fromIntegral i) c

instance A'And'Multiply_To' P.Integer P.String P.String where
  i !* s = P.concat $ P.replicate (P.fromIntegral i) s

--   A'Divided_By'Is'

instance A'Divided_By'Is' P.Integer P.Integer P.Double where
  x !/ y = P.fromIntegral x / P.fromIntegral y

instance A'Divided_By'Is' P.Integer P.Double P.Double where
  i !/ x  = P.fromIntegral i / x

instance A'Divided_By'Is' P.Double P.Integer P.Double where
  x !/ i = x / P.fromIntegral i

--   A'And'Add_To'

instance P.Show a => A'And'Add_To' a P.String P.String where
  x !+ str = P.show x ++ str

instance P.Show a => A'And'Add_To' P.String a P.String where
  str !+ x = str ++ P.show x

instance A'And'Add_To' [a] [a] [a] where
  (!+) = (++)

instance A'And'Add_To' [P.Char] [P.Char] [P.Char] where
  (!+) = (++)

instance b ~ [a] => A'And'Add_To' a [a] b where
  (!+) = (:)

instance b ~ [a] => A'And'Add_To' [a] a b where
  l !+ a = l ++ [a]

instance P.Num a => A'And'Add_To' a a a where
  (!+) = (+)

instance A'And'Add_To' P.Integer P.Double P.Double where
  i !+ x  = P.fromIntegral i + x

instance A'And'Add_To' P.Double P.Integer P.Double where
  x !+ i = x + P.fromIntegral i

instance (a ~ P.String) => A'And'Add_To' P.Char P.Char a where
  c' !+ c2 = c' : [c2]

instance A'And'Add_To' P.Char P.String P.String where
  (!+) = (:)

--   A'Minus'Is'

instance P.Num a => A'Minus'Is' a a a where
  (!-) = (-)

instance A'Minus'Is' P.String P.Char P.String where
  s !- c = P.filter (/= c) s

instance P.Eq a => A'Minus'Is' [a] [a] [a] where
  l1 !- l2 = case P.length l2 > P.length l1 of
    P.True -> l1
    P.False ->
      let
      (l11, l12) = P.splitAt (P.length l2) l1
      in
      case l11 == l2 of
        P.True -> l12 !- l2
        P.False -> P.head l1 : (P.tail l1 !- l2)

--   A'And'Can_Be_Equal

instance P.Eq a => A'And'Can_Be_Equal a a where
  (!==) = (==)

--   A'And'Can_Be_Unequal

instance P.Eq a => A'And'Can_Be_Unequal a a where
  (!!=) = (/=)

--   A'Can_Be_Greater_Than'

instance P.Ord a => A'Can_Be_Greater_Than' a a where
  (!>) = (>)

--   A'Can_Be_Less_Than'

instance P.Ord a => A'Can_Be_Less_Than' a a where
  (!<) = (<)

--   A'Can_Be_Gr_Or_Eq_To'

instance P.Ord a => A'Can_Be_Gr_Or_Eq_To' a a where
  (!>=) = (>=)

--   A'Can_Be_Le_Or_Eq_To'

instance P.Ord a => A'Can_Be_Le_Or_Eq_To' a a where
  (!<=) = (<=)

--   A'Has_Then

instance P.Applicative f => A'Has_Then f where
  (!>>) = (*>)

--   A'Has_Use

instance P.Monad m => A'Has_Use m where
  (!>>=) = (>>=)

--   A'Has_And

instance A'Has_And P.Bool where
  (!&) = (&&)

--   A'Has_Or

instance A'Has_Or P.Bool where
  (!|) = (||)

