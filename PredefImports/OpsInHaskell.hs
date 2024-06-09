{-# LANGUAGE
  MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances,
  UndecidableInstances, FunctionalDependencies, GADTs, IncoherentInstances
#-}

module PredefImports.OpsInHaskell where

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

class A1ToThe1Is1 a b c | a b -> c where
  (!^) :: a -> b -> c

class A1And1Multiply_To1 a b c | a b -> c where
  (!*) :: a -> b -> c
class A1Divided_By1Is1 a b c | a b -> c  where
  (!/) :: a -> b -> c

class A1And1Add_To1 a b c | a b -> c where
  (!+) :: a -> b -> c
class A1Minus1Is1 a b c | a b -> c where
  (!-) :: a -> b -> c

class A1And1Can_Be_Equal a b where
  (!==) :: a -> b -> Bool
class A1And1Can_Be_Unequal a b where
  (!!=) :: a -> b -> Bool
class A1Can_Be_Greater_Than1 a b where
  (!>) :: a -> b -> Bool
class A1Can_Be_Less_Than1 a b where
  (!<) :: a -> b -> Bool
class A1Can_Be_Gr_Or_Eq_To1 a b where
  (!>=) :: a -> b -> Bool
class A1Can_Be_Le_Or_Eq_To1 a b where
  (!<=) :: a -> b -> Bool

class A1Has_And a where
  (!&) :: a -> a -> a

class A1Has_Or a where
  (!|) :: a -> a -> a

class A1Has_Use u where
  (!>>=) :: u a -> (a -> u b) -> u b

class A1Has_Then t where
  (!>>) :: t a -> t b -> t b

-- A1ToThe1Is1
instance Floating a => A1ToThe1Is1 a a a where
  (!^) = (**)

instance A1ToThe1Is1 Int Int Int where
  (!^) = (^)

instance A1ToThe1Is1 Int Float Float where
  i !^ x = fromIntegral i ** x

instance A1ToThe1Is1 Float Int Float where
  x !^ i = x ** fromIntegral i

-- A1And1Multiply_To1
instance Num a => A1And1Multiply_To1 a a a where
  (!*) = (*)

instance A1And1Multiply_To1 Int Float Float where
  i !* x  = fromIntegral i * x

instance A1And1Multiply_To1 Float Int Float where
  x !* i = x * fromIntegral i

instance A1And1Multiply_To1 Int Char String where
  i !* c = replicate i c

instance A1And1Multiply_To1 Int String String where
  i !* s = concat $ replicate i s

-- A1Divided_By1Is1
instance A1Divided_By1Is1 Int Int Float where
  x !/ y = fromIntegral x / fromIntegral y

instance A1Divided_By1Is1 Int Float Float where
  i !/ x  = fromIntegral i / x

instance A1Divided_By1Is1 Float Int Float where
  x !/ i = x / fromIntegral i

-- A1And1Add_To1
instance Show a => A1And1Add_To1 a String String where
  x !+ str = show x ++ str

instance Show a => A1And1Add_To1 String a String where
  str !+ x = str ++ show x

instance A1And1Add_To1 [a] [a] [a] where
  (!+) = (++)

instance b ~ [a] => A1And1Add_To1 a [a] b where
  (!+) = (:)

instance b ~ [a] => A1And1Add_To1 [a] a b where
  l !+ a = l ++ [a]

instance Num a => A1And1Add_To1 a a a where
  (!+) = (+)

instance A1And1Add_To1 Int Float Float where
  i !+ x  = fromIntegral i + x

instance A1And1Add_To1 Float Int Float where
  x !+ i = x + fromIntegral i

instance (a ~ String) => A1And1Add_To1 Char Char a where
  c1 !+ c2 = c1 : [c2]

instance A1And1Add_To1 Char String String where
  (!+) = (:)

-- A1Minus1Is1
instance Num a => A1Minus1Is1 a a a where
  (!-) = (-)

instance A1Minus1Is1 String Char String where
  s !- c = filter (/= c) s

instance Eq a => A1Minus1Is1 [a] [a] [a] where
  l1 !- l2 = case length l2 > length l1 of
    True -> l1
    False ->
      let
      (l11, l12) = splitAt (length l2) l1
      in
      case l11 == l2 of
        True -> l12 !- l2
        False -> head l1 : (tail l1 !- l2)

-- A1And1Can_Be_Equal
instance Eq a => A1And1Can_Be_Equal a a where
  (!==) = (==)

-- A1And1Can_Be_Unequal
instance Eq a => A1And1Can_Be_Unequal a a where
  (!!=) = (/=)

-- A1Can_Be_Greater_Than1
instance Ord a => A1Can_Be_Greater_Than1 a a where
  (!>) = (>)

-- A1Can_Be_Less_Than1
instance Ord a => A1Can_Be_Less_Than1 a a where
  (!<) = (<)

-- A1Can_Be_Gr_Or_Eq_To1
instance Ord a => A1Can_Be_Gr_Or_Eq_To1 a a where
  (!>=) = (>=)

-- A1Can_Be_Le_Or_Eq_To1
instance Ord a => A1Can_Be_Le_Or_Eq_To1 a a where
  (!<=) = (<=)

-- A1Has_Then
instance Applicative f => A1Has_Then f where
  (!>>) = (*>)

-- A1Has_Use
instance Monad m => A1Has_Use m where
  (!>>=) = (>>=)

-- A1Has_And
instance A1Has_And Bool where
  (!&) = (&&)

-- A1Has_Or
instance A1Has_Or Bool where
  (!|) = (||)

