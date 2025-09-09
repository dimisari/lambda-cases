{-
This file contains:
- Types and values that can be used in every lcases program.  Most of it is
  simply renaming standard haskell stuff.
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
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies,
  UndecidableInstances
#-}

module PredefImports.Predefined where

import qualified Prelude as P
import qualified Control.Monad.State as MS
import qualified Data.List.Split as LS
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM

-- types

type ProgramWith' = P.IO
type EmptyVal = ()
type Program = ProgramWith' EmptyVal
type ListOf's = []
type A'FState'Man a b = MS.State b a
type State'Man a = A'FState'Man EmptyVal a
type Possibly' = P.Maybe
type Result'OrError' a b = P.Either b a
type Z = P.Integer
type R = P.Double
type SMapTo' = HM.HashMap P.String
type ArrayOf's = IM.IntMap

-- values

print_line' = P.putStrLn
get_line = P.getLine
split'to_words = P.words
split'to_lines = P.lines
apply'to_all_in' = P.uncurry P.map
throw_err' = P.error
id' = P.id
sqrt_of' = P.sqrt
sin' = P.sin
cos' = P.cos
tan' = P.tan
asin' = P.asin
acos' = P.acos
atan' = P.atan
a'is_odd = P.odd
a'is_even = P.even
truncate' = P.truncate
round' = P.round
floor' = P.floor
ceiling' = P.ceiling
exp' = P.exp
ln' = P.log
log_of'base' = P.uncurry (P.flip P.logBase)
filter'with' = P.uncurry (P.flip P.filter)
zip'with' = P.uncurry P.zip
unzip' = P.unzip
get_char = P.getChar
get_input = P.getContents
read_file' = P.readFile
write'to_file' = P.uncurry (P.flip P.writeFile)
print_string' = P.putStr
empty_val = ()
apply'to_all_in_zipped'' = \(f, l1, l2) -> P.zipWith (P.curry f) l1 l2

max_of'and' :: P.Ord a => (a, a) -> a
max_of'and' = P.uncurry P.max

min_of'and' :: P.Ord a => (a, a) -> a
min_of'and' = P.uncurry P.min

abs_val_of' :: P.Num a => a -> a
abs_val_of' = P.abs

gcd_of'and' :: P.Integral a => (a, a) -> a
gcd_of'and' = P.uncurry P.gcd

lcm_of'and' :: P.Integral a => (a, a) -> a
lcm_of'and' = P.uncurry P.lcm

get_state :: MS.State a a
get_state = MS.get

set_state' :: s -> MS.State s EmptyVal
set_state' = MS.put

modify_state_with' :: (s -> s) -> MS.State s EmptyVal
modify_state_with' = MS.modify

result_of'on_init_state' :: (MS.State s a, s) -> a
result_of'on_init_state' = P.uncurry MS.evalState

final_state_of'on_init_state' :: (MS.State s a, s) -> s
final_state_of'on_init_state' = P.uncurry MS.execState

run'on_init_state' :: (MS.State s a, s) -> (a, s)
run'on_init_state' = P.uncurry MS.runState

a'div' :: P.Integral a => (a, a) -> a
a'div' = P.uncurry P.div

a'mod' :: P.Integral a => (a, a) -> a
a'mod' = P.uncurry P.mod

print' :: P.Show a => a -> Program
print' = P.print

a'length :: [a] -> P.Integer
a'length = P.fromIntegral P.. P.length

a'is_in' :: P.Eq a => (a, [a]) -> P.Bool
a'is_in' = P.uncurry P.elem

ignore'from' :: (P.Integer, [a]) -> [a]
ignore'from' = P.uncurry P.drop P.. \(x, y) -> (P.fromIntegral x, y)

take'from' :: (P.Integer, [a]) -> [a]
take'from' = P.uncurry P.take P.. \(x, y) -> (P.fromIntegral x, y)

split'at_index' :: ([a], P.Integer) -> ([a], [a])
split'at_index' =
  P.uncurry (P.flip P.splitAt) P.. \(l, i) -> (l, P.fromIntegral i)

split'at_str' :: (P.String, P.String) -> [P.String]
split'at_str' = P.uncurry P.$ P.flip LS.splitOn

concat_lists_in' :: [[a]] -> [a]
concat_lists_in' = P.concat

do_nothing :: P.Applicative f => f EmptyVal
do_nothing = P.pure empty_val

from_string' :: P.Read a => P.String -> a
from_string' = P.read

program_with' :: a -> P.IO a
program_with' = P.return

not' :: P.Bool -> P.Bool
not' = P.not

for_all_in'' :: P.Monad m => ([a], a -> m b) -> m EmptyVal
for_all_in'' = P.uncurry P.$ P.flip P.mapM_

-- Hash map

empty_smap :: SMapTo' v
empty_smap = HM.empty

insert'to_smap' :: ((P.String, v), SMapTo' v) -> SMapTo' v
insert'to_smap' = \((s,v), m) -> HM.insert s v m

look_for'in_smap' :: (P.String, SMapTo' v) -> P.Maybe v
look_for'in_smap' = \(s,m) -> HM.lookup s m

smap_from_list' :: [(P.String, v)] -> SMapTo' v
smap_from_list' = HM.fromList

-- int map

empty_array :: ArrayOf's v
empty_array = IM.empty

insert'to_array' :: ((P.Integer, v), ArrayOf's v) -> ArrayOf's v
insert'to_array' = \((i, v), m) -> IM.insert (P.fromInteger i) v m

index'of_array' :: (P.Integer, ArrayOf's v) -> P.Maybe v
index'of_array' = \(i, m) -> IM.lookup (P.fromInteger i) m

array_from_list' :: ListOf's (P.Integer, v) -> ArrayOf's v
array_from_list' = IM.fromList P.. P.map (\(i, v) -> (P.fromInteger i, v))

array_size :: ArrayOf's v -> P.Integer
array_size = P.toInteger P.. IM.size

--

from'to' :: (P.Integer, P.Integer) -> [P.Integer]
from'to' = \(i1, i2) -> case i1 P.< i2 of
  P.True -> [i1..i2]
  _ -> P.reverse [i2..i1]

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

-- override P.Show for P.String

instance {-# OVERLAPS #-} P.Show P.String where
  show = P.id

