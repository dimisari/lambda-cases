{-
This file defines some helpers:
- Function application and composition operators
- Functor and Applicative operators
- Other
-}

module Helpers where

import Prelude ((.), ($), (<$>), (++), (*), (/=))
import Prelude qualified as P

-- types

type Lcases = P.String
type FileName = P.String

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

make_extension_hs :: FileName -> FileName
make_extension_hs =
  P.reverse .> P.span (/= '.') .> \(reversed_extension, reversed_name) ->
  let
  extension :: P.String
  extension = P.reverse reversed_extension
  in
  case P.elem extension ["lc", "txt"] of
    P.True -> reversed_name &> P.tail &> P.reverse ++ ".hs"
    P.False ->
      P.error $
        "make_extension_hs: did not receive .lc file\n" ++
        "make_extension_hs: instead I got " ++ extension

do_nothing :: P.Monad m => m ()
do_nothing = P.return ()

-- ASTTypes.hs
-- grules.hs
