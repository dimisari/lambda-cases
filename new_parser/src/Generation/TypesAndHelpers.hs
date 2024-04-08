module Generation.TypesAndHelpers where

import Control.Monad.State.Lazy

import Data.List

import ASTTypes
import Helpers

-- types
type Haskell = String

type HsPair = (Haskell, Haskell)

type WithParamNum = State Int

type WithIndentLvl = State Int

newtype NeedsParen a = NeedsParen a

newtype NeedsTypeAnnotation a = NeedsTypeAnnotation a

newtype CaseOf = CaseOf CasesParams

-- classes
class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

-- helper instances
instance ToHaskell a => ToHaskell [a] where 
  to_haskell = concatMap to_haskell

instance ToHaskell a => ToHaskell (Maybe a) where 
  to_haskell = \case
    Nothing -> ""
    Just a -> to_haskell a

instance ToHsWithParamNum a => ToHsWithParamNum (Maybe a) where
  to_hs_wpn = \case
    Nothing -> return ""
    Just a -> to_hs_wpn a 

instance ToHsWithIndentLvl a => ToHsWithIndentLvl (Maybe a) where
  to_hs_wil = \case
    Nothing -> return ""
    Just a -> to_hs_wil a 

-- list helpers
to_hs_prepend_list :: ToHaskell a => String -> [a] -> Haskell
to_hs_prepend_list = \sep -> concatMap ((sep ++) . to_haskell)

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_wpn

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = traverse to_hs_wil

-- params helpers
get_next_param :: WithParamNum Haskell
get_next_param = get $> (\i -> "x" ++ show i ++ "'") <* modify (+1)

params_hs_gen :: WithParamNum Haskell
params_hs_gen =
  get $> \case
    0 -> ""
    1 -> "\\x0' -> "
    i ->
      "\\(" ++ ([0..i-1] &> map num_to_param &> intercalate ", ") ++ ") -> "
      where
      num_to_param :: Int -> String
      num_to_param = \j -> "x" ++ show j ++ "'"

add_params_to :: WithParamNum Haskell -> WithParamNum Haskell
add_params_to = \hs_gen ->
  hs_gen >>= \hs ->
  params_hs_gen >>= \case
    "" -> return hs
    params_hs -> return $ "(" ++ params_hs ++ hs ++ ")"

add_params_to2 :: WithParamNum [Haskell] -> WithParamNum [Haskell]
add_params_to2 = \hs_list_gen ->
  hs_list_gen >>= \hs_list ->
  params_hs_gen >>= \case
    "" -> return hs_list
    params_hs -> return $ params_hs : hs_list

-- Indentation helpers
change_indent_lvl :: Int -> WithIndentLvl ()
change_indent_lvl = \i -> modify (+i)

deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper = \hs_gen -> change_indent_lvl 1 *> hs_gen <* change_indent_lvl (-1)

deeper2 :: WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper2 = \hs_gen -> change_indent_lvl 2 *> hs_gen <* change_indent_lvl (-2)

indent :: WithIndentLvl Haskell
indent = get $> (\i -> replicate (2 * i) ' ')

indent_all_and_concat :: [Haskell] -> WithParamNum Haskell
indent_all_and_concat = \hs_list ->
  indent >>= \indent_hs ->
  return $ map (indent_hs ++) hs_list &> intercalate "\n"

-- other
run_generator :: State Int a -> a
run_generator = \hs_gen -> evalState hs_gen 0

to_hs_maybe_np :: Maybe NamePart -> Haskell
to_hs_maybe_np = \case
  Nothing -> ""
  Just (NP str) -> "'" ++ str

-- GroupedValueDefs helpers
-- ../ASTTypes.hs
-- AST.hs
-- Test.hs
