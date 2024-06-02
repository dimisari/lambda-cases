{-# LANGUAGE LambdaCase, FlexibleInstances, FlexibleContexts #-}

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

data NeedsAnnotBool = Annot | NoAnnot

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

class HasArgs a where
  args_length :: a -> Int

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
  1 -> "\\" ++ to_param 0 ++ " -> "
  i -> case i > 1 of
    True -> "\\" ++ to_params_in_paren_hs i ++ " -> "
    False -> error "should be impossible: negative num of params"

to_params_in_paren_hs :: Int -> Haskell
to_params_in_paren_hs = \i ->
  "(" ++ ([0..i-1] &> map to_param &> intercalate ", ") ++ ")"

to_param :: Int -> Haskell
to_param = \j -> param_prefix ++ show j

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
    1 -> to_param 0
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

-- Args length hs
singe_quotes_hs :: HasArgs a => a -> String
singe_quotes_hs = args_length .> \i -> replicate i '\''

args_strs_hs :: HasArgs a => [(a, String)] -> Haskell
args_strs_hs = concatMap (\(a, str) -> singe_quotes_hs a ++ str)

args_nps_hs :: HasArgs a => [(a, NamePart)] -> Haskell
args_nps_hs =
  concatMap (\(a, NP str) -> singe_quotes_hs a ++ str) .> (upper_prefix ++)

nps_args_hs :: HasArgs a => [(NamePart, a)] -> Haskell
nps_args_hs = concatMap (\(NP str, a) -> str ++ singe_quotes_hs a)

maybe_args_hs :: HasArgs a => Maybe a -> Haskell
maybe_args_hs = \case
  Nothing -> ""
  Just a -> singe_quotes_hs a

maybe_prefix_args_hs :: HasArgs a => String -> Maybe a -> Haskell
maybe_prefix_args_hs = \prefix -> \case
  Nothing -> ""
  Just a -> prefix ++ singe_quotes_hs a

instance HasArgs Arguments where
  args_length = \(As (LEOUs (leou, leous))) -> length $ leou : leous

instance HasArgs TypesInParen where
  args_length = \(TIP (st, sts)) -> length $ st : sts

instance HasArgs SubsOrUndersInParen where
  args_length = \(SOUIP (sou, sous)) -> length $ sou : sous

instance HasArgs ParamVarsInParen where
  args_length = \(PVIP (ptv, ptvs)) -> length $ ptv : ptvs

instance HasArgs SubsInParen where
  args_length = \(SIP (tvs, tvss)) -> length $ tvs : tvss

instance HasArgs UndersInParen where
  args_length = \(UIP i) -> i

-- ParenFuncAppOrId helpers
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
comb_sid_def_hs = \hs1 hs2 ->
  "\n" ++ hs1 ++ " = \\new x -> x { " ++ hs2 ++ " = new }"

-- prefixes
lower_prefix :: Haskell
lower_prefix = "a"

upper_prefix :: Haskell
upper_prefix = "A"

spid_projection_prefix :: Haskell
spid_projection_prefix = "p"

change_prefix :: Haskell
change_prefix = "c0"

spid_change_prefix :: Haskell
spid_change_prefix = "c"

constructor_prefix :: Haskell
constructor_prefix = "C"

param_t_var_prefix :: Haskell
param_t_var_prefix = "a"

ad_hoc_t_var_prefix :: Haskell
ad_hoc_t_var_prefix = "b"

param_prefix :: Haskell
param_prefix = "pA"

-- other
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

tn_to_tid_hs :: TypeName -> Haskell
tn_to_tid_hs = \(TN (mpvip1, TId str, pvip_str_pairs, mpvip2)) ->
  maybe_prefix_args_hs upper_prefix mpvip1 ++ str ++
  args_strs_hs pvip_str_pairs ++ maybe_args_hs mpvip2

tn_to_cons_hs :: TypeName -> Haskell
tn_to_cons_hs = tn_to_tid_hs .> (++ "'")

ft_to_st :: FieldType -> SimpleType
ft_to_st = \case
  PBT1 pbt -> pbt_to_st pbt
  PoT3 pt -> PoT1 pt

pbt_to_st :: PowerBaseType -> SimpleType
pbt_to_st = \case
  PTV2 ptv -> PTV1 ptv
  TAIOA2 taioa -> TAIOA1 taioa
  IPT ipt -> ipt_to_st ipt

ipt_to_st :: InParenT -> SimpleType
ipt_to_st = \case
  PT3 pt -> PT1 pt
  FT3 ft -> FT1 ft

change_prop_hs_if_needed :: Haskell -> Haskell
change_prop_hs_if_needed = \case
  "A'Has_Str_Rep" -> "Show"
  hs -> hs

change_id_hs_if_needed :: Haskell -> Haskell
change_id_hs_if_needed = \case
  "a'to_string" -> "show"
  hs -> hs

-- GroupedValueDefs helpers
-- ASTTypes.hs
-- Collect.hs
-- AST.hs
-- Test.hs
