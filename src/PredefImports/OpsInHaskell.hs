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
infixr 0 !>>=, !>>

-- function application/composition

(&>) :: a -> (a -> b) -> b
x &> f = f x

(<&) :: (a -> b) -> a -> b
f <& x = f x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

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
  (!==) :: a -> b -> Bool

class A'And'Can_Be_Unequal a b where
  (!!=) :: a -> b -> Bool

class A'Can_Be_Greater_Than' a b where
  (!>) :: a -> b -> Bool

class A'Can_Be_Less_Than' a b where
  (!<) :: a -> b -> Bool

class A'Can_Be_Gr_Or_Eq_To' a b where
  (!>=) :: a -> b -> Bool

class A'Can_Be_Le_Or_Eq_To' a b where
  (!<=) :: a -> b -> Bool

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

instance Floating a => A'To_The'Is' a a a where
  (!^) = (**)

instance A'To_The'Is' Integer Integer Integer where
  (!^) = (^)

instance A'To_The'Is' Integer Double Double where
  i !^ x = fromIntegral i ** x

instance A'To_The'Is' Double Integer Double where
  x !^ i = x ** fromIntegral i

--   A'And'Multiply_To'

instance Num a => A'And'Multiply_To' a a a where
  (!*) = (*)

instance A'And'Multiply_To' Integer Double Double where
  i !* x  = fromIntegral i * x

instance A'And'Multiply_To' Double Integer Double where
  x !* i = x * fromIntegral i

instance A'And'Multiply_To' Integer Char String where
  i !* c = replicate (fromIntegral i) c

instance A'And'Multiply_To' Integer String String where
  i !* s = concat $ replicate (fromIntegral i) s

--   A'Divided_By'Is'

instance A'Divided_By'Is' Integer Integer Double where
  x !/ y = fromIntegral x / fromIntegral y

instance A'Divided_By'Is' Integer Double Double where
  i !/ x  = fromIntegral i / x

instance A'Divided_By'Is' Double Integer Double where
  x !/ i = x / fromIntegral i

--   A'And'Add_To'

instance Show a => A'And'Add_To' a String String where
  x !+ str = show x ++ str

instance Show a => A'And'Add_To' String a String where
  str !+ x = str ++ show x

instance A'And'Add_To' [a] [a] [a] where
  (!+) = (++)

instance A'And'Add_To' [Char] [Char] [Char] where
  (!+) = (++)

instance b ~ [a] => A'And'Add_To' a [a] b where
  (!+) = (:)

instance b ~ [a] => A'And'Add_To' [a] a b where
  l !+ a = l ++ [a]

instance Num a => A'And'Add_To' a a a where
  (!+) = (+)

instance A'And'Add_To' Integer Double Double where
  i !+ x  = fromIntegral i + x

instance A'And'Add_To' Double Integer Double where
  x !+ i = x + fromIntegral i

instance (a ~ String) => A'And'Add_To' Char Char a where
  c' !+ c2 = c' : [c2]

instance A'And'Add_To' Char String String where
  (!+) = (:)

--   A'Minus'Is'

instance Num a => A'Minus'Is' a a a where
  (!-) = (-)

instance A'Minus'Is' String Char String where
  s !- c = filter (/= c) s

instance Eq a => A'Minus'Is' [a] [a] [a] where
  l1 !- l2 = case length l2 > length l1 of
    True -> l1
    False ->
      let
      (l11, l12) = splitAt (length l2) l1
      in
      case l11 == l2 of
        True -> l12 !- l2
        False -> head l1 : (tail l1 !- l2)

--   A'And'Can_Be_Equal

instance Eq a => A'And'Can_Be_Equal a a where
  (!==) = (==)

--   A'And'Can_Be_Unequal

instance Eq a => A'And'Can_Be_Unequal a a where
  (!!=) = (/=)

--   A'Can_Be_Greater_Than'

instance Ord a => A'Can_Be_Greater_Than' a a where
  (!>) = (>)

--   A'Can_Be_Less_Than'

instance Ord a => A'Can_Be_Less_Than' a a where
  (!<) = (<)

--   A'Can_Be_Gr_Or_Eq_To'

instance Ord a => A'Can_Be_Gr_Or_Eq_To' a a where
  (!>=) = (>=)

--   A'Can_Be_Le_Or_Eq_To'

instance Ord a => A'Can_Be_Le_Or_Eq_To' a a where
  (!<=) = (<=)

--   A'Has_Then

instance Applicative f => A'Has_Then f where
  (!>>) = (*>)

--   A'Has_Use

instance Monad m => A'Has_Use m where
  (!>>=) = (>>=)

--   A'Has_And

instance A'Has_And Bool where
  (!&) = (&&)

--   A'Has_Or

instance A'Has_Or Bool where
  (!|) = (||)

