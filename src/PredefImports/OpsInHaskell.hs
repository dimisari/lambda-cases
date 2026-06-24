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

class A'To_The'Is' a b c where
  (!^) :: a -> b -> c

class A'And'Multiply_To' a b c where
  (!*) :: a -> b -> c

class A'Divided_By'Is' a b c  where
  (!/) :: a -> b -> c

class A'And'Add_To' a b c where
  (!+) :: a -> b -> c

class A'Minus'Is' a b c where
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

instance a ~ P.Double => A'To_The'Is' P.Double P.Integer a where
  x !^ i = x ** P.fromIntegral i

instance a ~ P.Integer => A'To_The'Is' P.Integer P.Integer a where
  (!^) = (^)

instance a ~ P.Double => A'To_The'Is' P.Integer P.Double a where
  i !^ x = P.fromIntegral i ** x

instance a ~ P.Double => A'To_The'Is' P.Double P.Double a where
  (!^) = (**)

--   A'And'Multiply_To'

instance a ~ P.Double => A'And'Multiply_To' P.Integer P.Double a where
  i !* x  = P.fromIntegral i * x

instance a ~ P.Double => A'And'Multiply_To' P.Double P.Integer a where
  x !* i = x * P.fromIntegral i

instance a ~ P.String => A'And'Multiply_To' P.Integer P.Char a where
  i !* c = P.replicate (P.fromIntegral i) c

instance a ~ P.String => A'And'Multiply_To' P.Integer P.String a where
  i !* s = P.concat $ P.replicate (P.fromIntegral i) s

instance a ~ P.Integer => A'And'Multiply_To' P.Integer P.Integer a where
  (!*) = (*)

--   A'Divided_By'Is'

instance a ~ P.Double => A'Divided_By'Is' P.Integer P.Integer a where
  x !/ y = P.fromIntegral x / P.fromIntegral y

instance a ~ P.Double => A'Divided_By'Is' P.Integer P.Double a where
  i !/ x  = P.fromIntegral i / x

instance a ~ P.Double => A'Divided_By'Is' P.Double P.Integer a where
  x !/ i = x / P.fromIntegral i

--   A'And'Add_To'

instance P.Show a => A'And'Add_To' a P.String P.String where
  x !+ str = P.show x ++ str

instance (P.Show a, b ~ P.String) => A'And'Add_To' P.String a b where
  str !+ x = str ++ P.show x

instance A'And'Add_To' [a] [a] [a] where
  (!+) = (++)

instance b ~ [a] => A'And'Add_To' a [a] b where
  (!+) = (:)

instance A'And'Add_To' a [a] [a] where
  (!+) = (:)

instance b ~ [a] => A'And'Add_To' [a] a b where
  l !+ a = l ++ [a]

instance a ~ P.Double => A'And'Add_To' P.Integer P.Double a where
  i !+ x  = P.fromIntegral i + x

instance a ~ P.Double => A'And'Add_To' P.Double P.Integer a where
  x !+ i = x + P.fromIntegral i

instance (a ~ P.String) => A'And'Add_To' P.Char P.Char a where
  c' !+ c2 = c' : [c2]

instance a ~ P.Integer => A'And'Add_To' a P.Integer P.Integer where
  (!+) = (+)

instance a ~ P.Integer => A'And'Add_To' P.Integer P.Integer a where
  (!+) = (+)

instance a ~ P.Integer => A'And'Add_To' P.Integer a P.Integer where
  (!+) = (+)

instance (a ~ P.Double, b ~ P.Double) => A'And'Add_To' a b P.Double where
  (!+) = (+)

--   A'Minus'Is'

instance a ~ P.String => A'Minus'Is' P.String P.Char a where
  s !- c = P.filter (/= c) s

remove_list_from_list :: P.Eq a => [a] -> [a] -> [a]
remove_list_from_list = \l1 l2 ->
  case P.length l2 > P.length l1 of
    P.True -> l1
    P.False ->
      let
      (l11, l12) = P.splitAt (P.length l2) l1
      in
      case l11 == l2 of
        P.True -> remove_list_from_list l12 l2
        P.False -> P.head l1 : (remove_list_from_list l1 l2)

instance a ~ P.String => A'Minus'Is' P.String P.String a where
  (!-) = remove_list_from_list

instance (a ~ b, P.Num b) => A'Minus'Is' a b b where
  (!-) = (-)

instance (P.Num a, a ~ b) => A'Minus'Is' a a b where
  (!-) = (-)

instance (P.Num a, a ~ b) => A'Minus'Is' a b a where
  (!-) = (-)

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

