{-
This file defines some helpers:
- Function application and composition operators
- Functor and Applicative operators
- Other
-}
{-# language LambdaCase #-}

module Helpers where

import Prelude ((.), ($), (<$>), (++), (*), (/=))
import Prelude qualified as P

import System.FilePath qualified as SFP
import Generation.TypesAndClasses qualified as GTC

-- types

type Lcases = P.String
type FilePath = P.String

-- func app/comp

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = P.flip (.)

(&>) :: a -> (a -> b) -> b
(&>) = P.flip ($)

-- Functor

mapf :: P.Functor f => f a -> (a -> b) -> f b
mapf = P.flip P.fmap

(>$>) :: P.Functor f => f a -> (a -> b) -> f b
(>$>) = P.flip P.fmap

(<++) :: P.Functor f => f [a] -> [a] -> f [a]
fas <++ as = (++ as) <$> fas

(++>) :: P.Functor f => [a] -> f [a] -> f [a]
as ++> fas = (as ++) <$> fas

-- Applicative

nothing :: P.Applicative f => f ()
nothing = P.pure ()

(++<) :: P.Applicative f => f a -> f b -> f (a, b)
pa ++< pb = P.liftA2 (,) pa pb

(+++<) :: P.Applicative f => f (a, b) -> f c -> f (a, b, c)
pab +++< pc = P.liftA2 (\(a, b) -> (,,) a b) pab pc

(++++<) :: P.Applicative f => f (a, b, c) -> f d -> f (a, b, c, d)
pabc ++++< pd = P.liftA2 (\(a, b, c) -> (,,,) a b c) pabc pd

(>:<) :: P.Applicative f => f a -> f [a] -> f [a]
a >:< as = P.liftA2 (:) a as

(>++<) :: P.Applicative f => f [a] -> f [a] -> f [a]
fas1 >++< fas2 = P.liftA2 (++) fas1 fas2

-- other

ind_lvl_to_spaces :: P.Int -> P.String
ind_lvl_to_spaces = \i -> P.replicate (2 * i) ' '

add_dotlc_if_needed :: FilePath -> FilePath
add_dotlc_if_needed = \pfn ->
  SFP.takeExtension pfn &> \case
    "" -> pfn ++ ".lc"
    _ -> pfn

make_extension :: P.String -> FilePath -> FilePath
make_extension = \ext -> SFP.dropExtension .> (++ ("." ++ ext))

do_nothing :: P.Monad m => m ()
do_nothing = P.return ()

-- module code

import_lc_file :: FilePath -> P.String -> FilePath
import_lc_file = \p imf -> SFP.takeDirectory p ++ "/" ++ imf

import_hs_file :: FilePath -> P.String -> FilePath
import_hs_file = \p imp -> SFP.takeDirectory p ++ "/" ++ imp ++ ".hs"

module_line_hs :: P.String -> GTC.Haskell
module_line_hs = \s -> "module " ++ s ++ " where\n"
