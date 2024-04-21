module Helpers where

import System.Directory

import Data.List.Split

type ProgramFileName = FileName
type ProgramStr = String
type FileName = String
type FileString = String
type TestExample = String

--paths
[progs_dir, test_exs_dir, in_dir] =
  ["programs/", "test_examples/", "../inputs/"]
  :: [FilePath]

list_progs :: IO [FilePath]
list_progs = listDirectory (in_dir ++ progs_dir)

-- func app/comp
(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(&>) :: a -> (a -> b) -> b
(&>) = flip ($)

-- Functor
mapf :: Functor f => f a -> (a -> b) -> f b
mapf = flip fmap

($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap

(<++) :: Functor f => f [a] -> [a] -> f [a]
fas <++ as = (++ as) <$> fas

(++>) :: Functor f => [a] -> f [a] -> f [a]
as ++> fas = (as ++) <$> fas

-- Applicative
(>:<) :: Applicative f => f a -> f [a] -> f [a]
a >:< as = (:) <$> a <*> as

nothing :: Applicative f => f ()
nothing = pure ()

-- Monad
(+++) :: Monad m => m a -> m b -> m (a, b)
pa +++ pb = pa >>= \a -> pb >>= \b -> return (a, b)

(++<) :: Monad m => m (a, b) -> m c -> m (a, b, c)
pab ++< pc = pab >>= \(a, b) -> pc >>= \c -> return (a, b, c)

(>++<) :: Monad m => m [a] -> m [a] -> m [a]
mas1 >++< mas2 = mas1 >>= \as1 -> mas2 >>= \as2 -> return $ as1 ++ as2

--
ind_lvl_to_spaces :: Int -> String
ind_lvl_to_spaces = \i -> replicate (2 * i) ' '

--
make_extension_hs :: FileName -> FileName
make_extension_hs = takeWhile (/= '.') .> (++ ".hs")

--
read_prog :: ProgramFileName -> IO ProgramStr
read_prog = \pfn -> readFile $ in_dir ++ progs_dir ++ pfn

read_exs_file :: FileName -> IO FileString
read_exs_file = \file_name -> readFile $ in_dir ++ test_exs_dir ++ file_name

read_examples :: FileName -> IO [FileString]
read_examples = \file_name -> read_exs_file file_name $> file_str_to_examples

file_str_to_examples :: FileString -> [ TestExample ]
file_str_to_examples = endBy "#\n\n"

