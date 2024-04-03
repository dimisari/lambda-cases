module Helpers where

import System.Directory

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

-- Applicative
(>:<) :: Applicative f => f a -> f [a] -> f [a]
a >:< as = (:) <$> a <*> as

-- Monad
(+++) :: Monad m => m a -> m b -> m (a, b)
pa +++ pb = pa >>= \a -> pb >>= \b -> return (a, b)

(++<) :: Monad m => m (a, b) -> m c -> m (a, b, c)
pab ++< pc = pab >>= \(a, b) -> pc >>= \c -> return (a, b, c)

(>++<) :: Monad m => m [a] -> m [a] -> m [a]
mas1 >++< mas2 = mas1 >>= \as1 -> mas2 >>= \as2 -> return $ as1 ++ as2
