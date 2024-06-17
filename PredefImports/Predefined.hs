{-# language
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,
  UndecidableInstances
#-}
module PredefImports.Predefined where

import Prelude hiding (IO)
import qualified Prelude as P
import Control.Monad.State

-- types
type A'FromIO = P.IO
type EmptyVal = ()
type IO = A'FromIO EmptyVal
type ListOf's = []
type A'FState'Man a b = State b a
type State'Man a = A'FState'Man EmptyVal a
type Possibly' = Maybe
type Result'OrError' a b = Either b a
type Z = Integer
type R = Double

-- values
print_line' = putStrLn
get_line = getLine
split'to_words = words
apply'to_all_in' = uncurry map
throw_err' = error
id' = id
sqrt_of' = sqrt
sin' = sin
cos' = cos
tan' = tan
asin' = asin
acos' = acos
atan' = atan
a'is_odd = odd
a'is_even = even
truncate' = truncate
round' = round
floor' = floor
ceiling' = ceiling
exp' = exp
ln' = log
log_of'base' = uncurry (flip logBase)
filter'with' = uncurry (flip filter)
zip'with' = uncurry zip
unzip' = unzip
get_char = getChar
get_input = getContents
read_file' = readFile
write'in_file' = uncurry (flip writeFile)
print_string' = putStr
empty_val = ()

apply'to_zipped'with' = \(f, l1, l2) -> zipWith (curry f) l1 l2

max_of'and' :: Ord a => (a, a) -> a
max_of'and' = uncurry max

min_of'and' :: Ord a => (a, a) -> a
min_of'and' = uncurry min

abs_val_of' :: Num a => a -> a
abs_val_of' = abs

gcd_of'and' :: Integral a => (a, a) -> a
gcd_of'and' = uncurry gcd

lcm_of'and' :: Integral a => (a, a) -> a
lcm_of'and' = uncurry lcm

get_state :: State a a
get_state = get

set_state' :: s -> State s EmptyVal
set_state' = put

modify_state_with' :: (s -> s) -> State s EmptyVal
modify_state_with' = modify

result_of'on_init_state' :: (State s a, s) -> a
result_of'on_init_state' = uncurry evalState

final_state_of'on_init_state' :: (State s a, s) -> s
final_state_of'on_init_state' = uncurry execState

run'on_init_state' :: (State s a, s) -> (a, s)
run'on_init_state' = uncurry runState

a'div' :: Integral a => (a, a) -> a
a'div' = uncurry div

a'mod' :: Integral a => (a, a) -> a
a'mod' = uncurry mod

print' :: Show a => a -> IO
print' = print

a'length :: [a] -> Integer
a'length = fromIntegral . length

a'is_in' :: Eq a => (a, [a]) -> Bool
a'is_in' = uncurry elem

ignore'from' :: (Integer, [a]) -> [a]
ignore'from' = uncurry drop . \(x, y) -> (fromIntegral x, y)

take'from' :: (Integer, [a]) -> [a]
take'from' = uncurry take . \(x, y) -> (fromIntegral x, y)

do_nothing :: Applicative f => f EmptyVal
do_nothing = pure empty_val

from_string' :: Read a => String -> a
from_string' = read

a'from_io :: a -> P.IO a
a'from_io = return

not' :: Bool -> Bool
not' = not

for_all_in'' :: Monad m => ([a], a -> m b) -> m EmptyVal
for_all_in'' = uncurry $ flip mapM_

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

--
class A'Has_A_Wrapper t where
  wrap' :: a -> t a

instance Applicative f => A'Has_A_Wrapper f where
  wrap' = pure

--
class A'Has_Internal_App t where
  apply'inside' :: (a -> b, t a) -> t b

instance Applicative f => A'Has_Internal_App f where
  apply'inside' = uncurry fmap

-- Show for String
instance {-# OVERLAPS #-} Show String where
  show = id
