{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Generation.TypesAndHelpers where

import Control.Monad.State.Lazy

import Data.List
import qualified Data.Set as S

import ASTTypes
import Helpers

import Generation.Collect

-- Haskell types
type Haskell = String

type HsPair = (Haskell, Haskell)

type DotChangeArgHs = Haskell

-- Helper types
data NeedsParenBool = Paren | NoParen

data NeedsAnnotationBool = Annotation | NoAnnotation

data PossiblyWhereExpr = HasWhereExpr WhereExpr | NoWhereExpr

newtype WholeParams = Whole Parameters

-- State types
type WithParamNum = State Int

type WithIndentLvl = State Int

-- classes
class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

class HasQuotesLength a where
  quotes_length :: a -> Int

-- automatic instances
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
to_hs_prepend_list = \prepend_hs -> concatMap ((prepend_hs ++) . to_haskell)

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_wpn

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = traverse to_hs_wil

-- Params helpers
get_next_param :: WithParamNum Haskell
get_next_param = get >$> to_param <* modify (+1)

params_hs_gen :: WithParamNum Haskell
params_hs_gen = get >$> to_params_hs

to_params_hs :: Int -> Haskell
to_params_hs = \case
  0 -> ""
  1 -> "\\x0' -> "
  i -> case i > 1 of
    True -> "\\" ++ to_params_in_paren_hs i ++ " -> "
    False -> error "should be impossible: negative num of params"

to_params_in_paren_hs :: Int -> Haskell
to_params_in_paren_hs = \i ->
  "(" ++ ([0..i-1] &> map to_param &> intercalate ", ") ++ ")"

to_param :: Int -> Haskell
to_param = \j -> "x" ++ show j ++ "'"

add_params_to :: WithParamNum Haskell -> WithParamNum Haskell
add_params_to = \hs_gen ->
  hs_gen >>= \hs ->
  params_hs_gen >$> \case
    "" -> hs
    params_hs -> "(" ++ params_hs ++ hs ++ ")"

add_params_to_list :: WithParamNum [Haskell] -> WithParamNum [Haskell]
add_params_to_list = \hs_list_gen ->
  hs_list_gen >>= \hs_list ->
  params_hs_gen >$> \case
    "" -> hs_list
    params_hs -> params_hs : hs_list

case_of_inner_hs_gen :: WithParamNum Haskell
case_of_inner_hs_gen =
  get >$> \case
    1 -> "x0'"
    i -> case i > 1 of
      True -> to_params_in_paren_hs i
      False -> error "should be impossible: zero or negative cases params"

-- Indentation helpers
change_il :: Int -> WithIndentLvl ()
change_il = \i -> modify (+i)

deeper_with_num :: Int -> WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper_with_num = \i hs_gen -> change_il i *> hs_gen <* change_il (-i)

deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper = deeper_with_num 1

twice_deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
twice_deeper = deeper_with_num 2

indent :: WithIndentLvl Haskell
indent = get >$> ind_lvl_to_spaces

indent_all_and_concat :: [Haskell] -> WithIndentLvl Haskell
indent_all_and_concat = \hs_list ->
  indent >$> \indent_hs -> map (indent_hs ++) hs_list &> intercalate "\n"

-- Single quotes
single_quotes :: HasQuotesLength a => a -> String
single_quotes = \a -> replicate (quotes_length a) '\''

quotes_strs_hs :: HasQuotesLength a => [(a, String)] -> Haskell
quotes_strs_hs = concatMap (\(a, str) -> single_quotes a ++ str)

quotes_nps_hs :: HasQuotesLength a => [(a, NamePart)] -> Haskell
quotes_nps_hs =
  concatMap (\(a, NP str) -> single_quotes a ++ str) .> (prop_prefix ++)

nps_quotes_hs :: HasQuotesLength a => [(NamePart, a)] -> Haskell
nps_quotes_hs = concatMap (\(NP str, a) -> str ++ single_quotes a)

maybe_quotes :: HasQuotesLength a => Maybe a -> Haskell
maybe_quotes = \case
  Nothing -> ""
  Just a -> single_quotes a

prefix_maybe_quotes :: HasQuotesLength a => String -> Maybe a -> Haskell
prefix_maybe_quotes = \prefix -> \case
  Nothing -> ""
  Just a -> prefix ++ single_quotes a

instance HasQuotesLength Arguments where
  quotes_length = \(As (LEOUs (leou, leous))) -> length $ leou : leous

instance HasQuotesLength TypesInParen where
  quotes_length = \(TIP (st, sts)) -> length $ st : sts

instance HasQuotesLength SubsOrUndersInParen where
  quotes_length = \(SOUIP (sou, sous)) -> length $ sou : sous

instance HasQuotesLength ParamVarsInParen where
  quotes_length = \(PVIP (ptv, ptvs)) -> length $ ptv : ptvs

instance HasQuotesLength AdHocVarsInParen where
  quotes_length = \(AHVIP (ahtv, ahtvs)) -> length $ ahtv : ahtvs

instance HasQuotesLength SubsInParen where
  quotes_length = \(SIP (tvs, tvss)) -> length $ tvs : tvss

-- ParenFuncAppOrId helpers
id_prefix :: Haskell
id_prefix = "v0"

type MargsPair = (Maybe Arguments, Maybe Arguments)
add_margs_to_args_list :: MargsPair -> [Arguments] -> [Arguments]
add_margs_to_args_list = \case
  (Nothing, Nothing) -> id
  (Just args, Nothing) -> ([args] ++)
  (Nothing, Just args) -> (++ [args])
  (Just args1, Just args2) -> \args_list -> [args1] ++ args_list ++ [args2]

calc_args_list :: MargsPair -> [ArgsStr] -> [Arguments]
calc_args_list = \margs_pair args_str_pairs ->
  add_margs_to_args_list margs_pair $ map fst args_str_pairs

-- TupleTypeDef helpers
combine_with_ts :: [Haskell] -> [Haskell] -> Haskell
combine_with_ts = \sid_hs_list types_hs_list ->
  zipWith comb_sid_t_hs sid_hs_list types_hs_list &> concat

combine_with_defs :: [Haskell] -> [Haskell] -> Haskell
combine_with_defs = \sid_hs_list defs_hs_list ->
  zipWith comb_sid_def_hs sid_hs_list defs_hs_list &> concat

comb_sid_t_hs :: Haskell -> Haskell -> Haskell
comb_sid_t_hs = \hs1 hs2 -> "\n" ++ hs1 ++ " :: " ++ hs2

comb_sid_def_hs :: Haskell -> Haskell -> Haskell
comb_sid_def_hs = \hs1 hs2 -> "\n" ++ hs1 ++ " = " ++ hs2

general_proj_hs_list :: [Haskell]
general_proj_hs_list = map (spid_projection_prefix ++) general_hs_list

general_change_hs_list :: [Haskell]
general_change_hs_list = map (spid_change_prefix ++) general_hs_list

general_hs_list :: [Haskell]
general_hs_list = ["1st", "2nd", "3rd", "4th", "5th"]

-- prefixes
projection_prefix :: Haskell
projection_prefix = "p0"

change_prefix :: Haskell
change_prefix = "c0"

spid_projection_prefix :: Haskell
spid_projection_prefix = "p"

spid_change_prefix :: Haskell
spid_change_prefix = "c"

tid_prefix :: Haskell
tid_prefix = "T0"

prop_prefix :: Haskell
prop_prefix = "P0"

constructor_prefix :: Haskell
constructor_prefix = "C"

param_t_var_prefix :: Haskell
param_t_var_prefix = "a"

ad_hoc_t_var_prefix :: Haskell
ad_hoc_t_var_prefix = "b"

-- other
add_to_hs_pair :: HsPair -> HsPair -> HsPair
add_to_hs_pair = \(hs1, hs2) (hs1', hs2') -> (hs1' ++ hs1, hs2' ++ hs2)

run_generator :: State Int a -> a
run_generator = \hs_gen -> evalState hs_gen 0

to_hs_maybe_np :: Maybe NamePart -> Haskell
to_hs_maybe_np = \case
  Nothing -> ""
  Just (NP str) -> "'" ++ str

in_paren_if :: NeedsParenBool -> Haskell -> Haskell
in_paren_if = \case
  NoParen -> id
  Paren -> \hs -> "(" ++ hs ++ ")"

mwe_to_pwe :: Maybe WhereExpr -> PossiblyWhereExpr
mwe_to_pwe = \case
  Nothing -> NoWhereExpr
  Just we -> HasWhereExpr we

under_pfarg_param :: Haskell
under_pfarg_param = "x'"

-- GroupedValueDefs helpers
-- ASTTypes.hs
-- Collect.hs
-- AST.hs
-- Test.hs
