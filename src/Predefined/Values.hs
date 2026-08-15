{-
This file contains:
- Values that can be used in every lcases program
-}

{-# language LambdaCase #-}

module Predefined.Values where

import Prelude ((.), (<), (>>), (>>=), (++), ($), (+), (-), (!!), (/=), (==))
import Prelude qualified as P
import Control.Monad.State qualified as MS
import Data.List qualified as DL
import Data.List.Split qualified as LS
import Data.HashMap.Strict qualified as HM
import Data.IntMap.Strict qualified as IM
import Data.Char qualified as CH
import System.Exit qualified as E
import System.Console.ANSI qualified as ANSI
import System.Process qualified as SP
import System.Environment qualified as SE
import System.Directory qualified as SD
import System.FilePath qualified as SF
import Text.Read qualified as TR
import Control.Concurrent.Async qualified as CCA

import Predefined.Operators ((.>), (&>))
import Predefined.Types qualified as PT
import Predefined.Classes qualified as PC
import Predefined.UTF8IO qualified as PU

split'to_words :: P.String -> [P.String]
split'to_words = P.words

split'to_lines :: P.String -> [P.String]
split'to_lines = P.lines

string_from_lines' :: [P.String] -> P.String
string_from_lines' = P.unlines

apply'to_all_in' :: (a -> b, [a]) -> [b]
apply'to_all_in' = P.uncurry P.map

error' :: P.String -> a
error' = P.error

id' :: a -> a
id' = P.id

sqrt_of' :: P.Floating a => a -> a
sqrt_of' = P.sqrt

sin' :: P.Floating a => a -> a
sin' = P.sin

cos' :: P.Floating a => a -> a
cos' = P.cos

tan' :: P.Floating a => a -> a
tan' = P.tan

asin' :: P.Floating a => a -> a
asin' = P.asin

acos' :: P.Floating a => a -> a
acos' = P.acos

atan' :: P.Floating a => a -> a
atan' = P.atan

a'is_odd :: P.Integral a => a -> P.Bool
a'is_odd = P.odd

a'is_even :: P.Integral a => a -> P.Bool
a'is_even = P.even

a'is_lower :: P.Char -> P.Bool
a'is_lower = CH.isLower

truncate' :: (P.RealFrac a, P.Integral b) => a -> b
truncate' = P.truncate

round' :: (P.RealFrac a, P.Integral b) => a -> b
round' = P.round

floor' :: (P.RealFrac a, P.Integral b) => a -> b
floor' = P.floor

ceiling' :: (P.RealFrac a, P.Integral b) => a -> b
ceiling' = P.ceiling

exp' :: P.Floating a => a -> a
exp' = P.exp

ln' :: P.Floating a => a -> a
ln' = P.log

log_of'base' :: P.Floating a => (a, a) -> a
log_of'base' = P.uncurry (P.flip P.logBase)

filter'with' :: ([a], a -> P.Bool) -> [a]
filter'with' = P.uncurry (P.flip P.filter)

zip'with' :: ([a], [b]) -> [(a, b)]
zip'with' = P.uncurry P.zip

unzip' :: [(a, b)] -> ([a], [b])
unzip' = P.unzip

get_char :: PT.ProgramWith' P.Char
get_char = P.getChar

get_input :: PT.ProgramWith' P.String
get_input = P.getContents

print'and_get_line :: P.String -> PT.ProgramWith' P.String
print'and_get_line = PC.print' .> (P.>> PU.get_line)

print_strings'in_lines :: PT.Strings -> PT.Program
print_strings'in_lines = string_from_lines' .> PC.print'

does_file'exist :: P.String -> PT.ProgramWith' P.Bool
does_file'exist = SD.doesFileExist

does_directory'exist :: P.String -> PT.ProgramWith' P.Bool
does_directory'exist = SD.doesDirectoryExist

list_directory' :: P.String -> PT.ProgramWith' (PT.ListOf's P.String)
list_directory' = SD.listDirectory

get_directory_of' :: P.String -> P.String
get_directory_of' = P.reverse .> P.dropWhile (/= '/') .> P.tail .> P.reverse

create_directory' :: P.String -> PT.Program
create_directory' = SD.createDirectory

get_file_name_of' :: P.String -> P.String
get_file_name_of' = P.reverse .> P.takeWhile (/= '/') .> P.reverse

get_file_extension_of' :: P.String -> P.String
get_file_extension_of' = SF.takeExtension

remove_file_extension_of' :: P.String -> P.String
remove_file_extension_of' = SF.dropExtension

split_file'at_extension :: P.String -> (P.String, P.String)
split_file'at_extension = SF.splitExtension

empty_val :: ()
empty_val = ()

apply'to_all_in_zipped'' :: ((a, b) -> c, [a], [b]) -> [c]
apply'to_all_in_zipped'' = \(f, l1, l2) -> P.zipWith (P.curry f) l1 l2

success :: PT.Program
success = E.exitSuccess

run' :: P.String -> PT.Program
run' = SP.callCommand

run_commands' :: PT.ListOf's P.String -> PT.Program
run_commands' = P.mapM_ SP.callCommand

run'and_get_output :: P.String -> PT.ProgramWith' P.String
run'and_get_output = \c -> SP.readCreateProcess (SP.shell c) ""

run'and_get_outputs :: PT.Strings -> PT.ProgramWith' PT.Strings
run'and_get_outputs = P.mapM run'and_get_output

run_commands'concurrently :: PT.ListOf's P.String -> PT.Program
run_commands'concurrently = CCA.mapConcurrently_ SP.callCommand

ask_to_run' :: P.String -> PT.Program
ask_to_run' = \s ->
  PC.print'("Should I run \"" ++ s ++ "\"? (y + Enter for yes)") >>
  PU.get_line >>= \case
    "y" -> SP.callCommand s
    _ -> do_nothing

clear_screen4 :: PT.Program
clear_screen4 =
  ANSI.getCursorPosition >>= \case
    P.Just (l, _) -> ANSI.scrollPageUp l >> ANSI.setCursorPosition 0 4
    P.Nothing -> error' "Could not get cursor position"

clear_screen :: PT.Program
clear_screen =
  ANSI.getCursorPosition >>= \case
    P.Just (l, _) -> ANSI.scrollPageUp l >> ANSI.setCursorPosition 0 0
    P.Nothing -> error' "Could not get cursor position"

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

set_state' :: s -> MS.State s PT.EmptyVal
set_state' = MS.put

modify_state_with' :: (s -> s) -> MS.State s PT.EmptyVal
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

length_of' :: [a] -> P.Integer
length_of' = P.fromIntegral . P.length

a'is_in' :: P.Eq a => (a, [a]) -> P.Bool
a'is_in' = P.uncurry P.elem

elem'of' :: (P.Integer, [a]) -> PT.Possibly' a
elem'of' = \(i, l) ->
  case i < 1 of
    P.True -> P.Nothing
    P.False ->
      case (i, l) of
        (1, a : _) -> P.Just a
        (_, []) -> P.Nothing
        (_, a : as) -> elem'of' (i - 1, as)

remove_last_of' :: [a] -> PT.Possibly' [a]
remove_last_of' = \case
  [] -> P.Nothing
  [a] -> P.Just []
  a : as -> P.fmap (a :) (remove_last_of' as)

last_of' :: [a] -> PT.Possibly' a
last_of' = \case
  [] -> P.Nothing
  [a] -> P.Just a
  a : as -> last_of' as

insert'at'in' :: (a, P.Integer, [a]) -> PT.Possibly' [a]
insert'at'in' = \(a, i, as) ->
  case i of
    1 -> P.Just (a : as)
    _ -> case as of
      [] -> P.Nothing
      h : t -> P.fmap (h :) (insert'at'in' (a, i - 1, t))

ignore'from' :: (P.Integer, [a]) -> [a]
ignore'from' = P.uncurry P.drop . \(x, y) -> (P.fromIntegral x, y)

take'from' :: (P.Integer, [a]) -> [a]
take'from' = P.uncurry P.take . \(x, y) -> (P.fromIntegral x, y)

take_from'while' :: ([a], a -> P.Bool) -> [a]
take_from'while' = P.uncurry $ P.flip P.takeWhile

split'at_index' :: ([a], P.Integer) -> ([a], [a])
split'at_index' =
  P.uncurry (P.flip P.splitAt) . \(l, i) -> (l, P.fromIntegral i)

split'at_string' :: (P.String, P.String) -> [P.String]
split'at_string' = P.uncurry $ P.flip LS.splitOn

split'at_first' :: P.Eq a => ([a], a) -> ([a], [a])
split'at_first' = \(l, e) -> P.break (== e) l &> \(l1, l2) -> (l1, P.drop 1 l2)

concat_lists' :: [[a]] -> [a]
concat_lists' = P.concat

do_nothing :: P.Applicative f => f PT.EmptyVal
do_nothing = P.pure empty_val

from_string' :: P.Read a => P.String -> PT.Possibly' a
from_string' = TR.readMaybe

program_with' :: a -> PT.ProgramWith' a
program_with' = P.return

not' :: P.Bool -> P.Bool
not' = P.not

for_all_in'' :: P.Monad m => ([a], a -> m b) -> m PT.EmptyVal
for_all_in'' = P.uncurry $ P.flip P.mapM_

to_all_in'' :: P.Monad m => ([a], a -> m b) -> m [b]
to_all_in'' = P.uncurry $ P.flip P.mapM

get_arguments :: PT.ProgramWith' [P.String]
get_arguments = SE.getArgs

sort'after_applying' :: P.Ord b => ([a], a -> b) -> [a]
sort'after_applying' = P.uncurry $ P.flip DL.sortOn

delimit'with' :: ([[a]], [a]) -> [a]
delimit'with' = P.uncurry $ P.flip DL.intercalate

-- Hash map

empty_smap :: PT.SMapTo' v
empty_smap = HM.empty

insert'to_smap' :: ((P.String, v), PT.SMapTo' v) -> PT.SMapTo' v
insert'to_smap' = \((s,v), m) -> HM.insert s v m

look_for'in_smap' :: (P.String, PT.SMapTo' v) -> P.Maybe v
look_for'in_smap' = \(s,m) -> HM.lookup s m

smap_from_list' :: [(P.String, v)] -> PT.SMapTo' v
smap_from_list' = HM.fromList

-- int map

empty_array :: PT.ArrayOf's v
empty_array = IM.empty

insert'to_array' :: ((P.Integer, v), PT.ArrayOf's v) -> PT.ArrayOf's v
insert'to_array' = \((i, v), m) -> IM.insert (P.fromInteger i) v m

index'of_array' :: (P.Integer, PT.ArrayOf's v) -> P.Maybe v
index'of_array' = \(i, m) -> IM.lookup (P.fromInteger i) m

array_from_list' :: PT.ListOf's (P.Integer, v) -> PT.ArrayOf's v
array_from_list' = IM.fromList . P.map (\(i, v) -> (P.fromInteger i, v))

array_size :: PT.ArrayOf's v -> P.Integer
array_size = P.toInteger . IM.size

-- mine

from'to' :: (P.Integer, P.Integer) -> [P.Integer]
from'to' = \(i1, i2) -> case i1 < i2 of
  P.True -> [i1..i2]
  _ -> P.reverse [i2..i1]

add_indexes_to'starting_from' :: ([a], P.Integer) -> [(P.Integer, a)]
add_indexes_to'starting_from' = \(l, i) -> P.zip [i..] l

add_one_indexes_to' :: [a] -> [(P.Integer, a)]
add_one_indexes_to' = \l -> add_indexes_to'starting_from' (l, 1)

add_zero_indexes_to' :: [a] -> [(P.Integer, a)]
add_zero_indexes_to' = \l -> add_indexes_to'starting_from' (l, 0)

