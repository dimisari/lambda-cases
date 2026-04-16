module Generation.TypesAndClasses where

import Prelude qualified as P
import Control.Monad.State.Lazy qualified as MS

import ASTTypes qualified as T

type Haskell = P.String

type WithParamNum = MS.State P.Int

type WithIndentLvl = MS.State P.Int

type DotChangeArgHs = Haskell

type MargsPair = (P.Maybe T.Arguments, P.Maybe T.Arguments)

newtype WholeParams = Whole T.Parameters

data NeedsParenBool = Paren | NoParen

data NeedsAnnotBool = Annot | NoAnnot

data PossiblyWhereExpr = HasWhereExpr T.WhereExpr | NoWhereExpr

class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

class HasArgs a where
  args_length :: a -> P.Int

