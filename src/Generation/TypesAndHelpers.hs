{-
This file contains various things:
- Most notable are the definitions and some helper functions for the following
  classes:
  - ToHaskell, ToHsWithParamNum, ToHsWithIndentLvl
  These are the classes for which AST.hs is just instances of
- Another interesting class is the class HasArgs
  - For identifiers where arguments are expected in lcases there needs
    to be a mechanism to show that in the equivalent haskell identifiers. This
    is done by substituting the arguments with an equal amount of single quotes
    in the translation
-}

{-# language LambdaCase, FlexibleInstances, FlexibleContexts #-}

module Generation.TypesAndHelpers where

import Control.Monad.State.Lazy

import Data.List
import qualified Data.Set as S

import ASTTypes
import Helpers
import Generation.PrefixesAndHardcoded

-- ToHaskell

type Haskell = String

class ToHaskell a where
  to_haskell :: a -> Haskell

instance ToHaskell a => ToHaskell [a] where
  to_haskell = concatMap to_haskell

instance ToHaskell a => ToHaskell (Maybe a) where
  to_haskell = \case
    Nothing -> ""
    Just a -> to_haskell a

to_hs_prepend_list :: ToHaskell a => String -> [a] -> Haskell
to_hs_prepend_list = \prepend_hs -> concatMap ((prepend_hs ++) . to_haskell)

-- WithParamNum

type WithParamNum = State Int

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

instance ToHsWithParamNum a => ToHsWithParamNum (Maybe a) where
  to_hs_wpn = \case
    Nothing -> return ""
    Just a -> to_hs_wpn a

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_wpn

generate_next_param :: WithParamNum Haskell
generate_next_param = get >$> int_to_param <* modify (+1)

params_hs_gen :: WithParamNum Haskell
params_hs_gen =
  get >$> int_to_params
  where
  int_to_params :: Int -> Haskell
  int_to_params = \case
    0 -> ""
    1 -> "\\" ++ int_to_param 0 ++ " -> "
    i -> case i > 1 of
      True -> "\\" ++ int_to_params_in_paren i ++ " -> "
      False -> error "should be impossible: negative num of params"

int_to_params_in_paren :: Int -> Haskell
int_to_params_in_paren = \i ->
  "(" ++ ([0..i-1] &> map int_to_param &> intercalate ", ") ++ ")"

int_to_param :: Int -> Haskell
int_to_param = \j -> param_prefix ++ show j

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
    1 -> int_to_param 0
    i -> case i > 1 of
      True -> int_to_params_in_paren i
      False -> error "should be impossible: zero or negative cases params"

-- WithIndentLvl

type WithIndentLvl = State Int

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

instance ToHsWithIndentLvl a => ToHsWithIndentLvl (Maybe a) where
  to_hs_wil = \case
    Nothing -> return ""
    Just a -> to_hs_wil a

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = traverse to_hs_wil

increase_il_by :: Int -> WithIndentLvl ()
increase_il_by = \i -> modify (+i)

decrease_il_by :: Int -> WithIndentLvl ()
decrease_il_by = \i -> increase_il_by (-i)

deeper_with_num :: Int -> WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper_with_num = \i hs_gen ->
  increase_il_by i *> hs_gen <* decrease_il_by i

deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper = deeper_with_num 1

twice_deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
twice_deeper = deeper_with_num 2

indent :: WithIndentLvl Haskell
indent = get >$> ind_lvl_to_spaces

indent_all_and_concat :: [Haskell] -> WithIndentLvl Haskell
indent_all_and_concat = \hs_list ->
  indent >$> \indent_hs -> map (indent_hs ++) hs_list &> intercalate "\n"

-- HasArgs

class HasArgs a where
  args_length :: a -> Int

instance HasArgs a => HasArgs (Maybe a) where
  args_length = \case
    Just a -> args_length a
    Nothing -> 0

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

singe_quotes_hs :: HasArgs a => a -> String
singe_quotes_hs = args_length .> \i -> replicate i '\''

args_strs_hs :: HasArgs a => [(a, String)] -> Haskell
args_strs_hs = concatMap (\(a, str) -> singe_quotes_hs a ++ str)

args_nps_hs :: HasArgs a => [(a, NamePart)] -> Haskell
args_nps_hs =
  concatMap (\(a, NP str) -> singe_quotes_hs a ++ str) .> (upper_prefix ++)

nps_args_hs :: HasArgs a => [(NamePart, a)] -> Haskell
nps_args_hs = concatMap (\(NP str, a) -> str ++ singe_quotes_hs a)

maybe_prefix_args_hs :: HasArgs a => String -> Maybe a -> Haskell
maybe_prefix_args_hs = \prefix -> \case
  Nothing -> ""
  Just a -> prefix ++ singe_quotes_hs a

-- types

type DotChangeArgHs = Haskell

newtype WholeParams = Whole Parameters

-- NeedsParenBool

data NeedsParenBool = Paren | NoParen

in_paren_if :: NeedsParenBool -> Haskell -> Haskell
in_paren_if = \case
  NoParen -> id
  Paren -> \hs -> "(" ++ hs ++ ")"

-- NeedsAnnotBool

data NeedsAnnotBool = Annot | NoAnnot

to_hs_needs_annot :: Show a => NeedsAnnotBool -> a -> String -> Haskell
to_hs_needs_annot = \needs_annot a str -> case needs_annot of
  Annot -> "(" ++ show a ++ " :: " ++ str ++ ")"
  NoAnnot -> show a

-- PossiblyWhereExpr

data PossiblyWhereExpr = HasWhereExpr WhereExpr | NoWhereExpr

mwe_to_pwe :: Maybe WhereExpr -> PossiblyWhereExpr
mwe_to_pwe = \case
  Nothing -> NoWhereExpr
  Just we -> HasWhereExpr we

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

-- other

run_generator :: State Int a -> a
run_generator = \hs_gen -> evalState hs_gen 0

to_hs_maybe_np :: Maybe NamePart -> Haskell
to_hs_maybe_np = \case
  Nothing -> ""
  Just (NP str) -> "'" ++ str

tn_to_tid_hs :: TypeName -> Haskell
tn_to_tid_hs = \(TN (mpvip1, TId str, pvip_str_pairs, mpvip2)) ->
  maybe_prefix_args_hs upper_prefix mpvip1 ++ str ++
  args_strs_hs pvip_str_pairs ++ singe_quotes_hs mpvip2

tn_to_cons_hs :: TypeName -> Haskell
tn_to_cons_hs = tn_to_tid_hs .> (++ "'")

change_prop_hs_if_needed :: Haskell -> Haskell
change_prop_hs_if_needed = \case
  "A'Has_Str_Rep" -> show_class
  hs -> hs

change_id_hs_if_needed1 :: Haskell -> Haskell
change_id_hs_if_needed1 = \case
  "a'to_string" -> show_val1
  hs -> hs

change_id_hs_if_needed2 :: Haskell -> Haskell
change_id_hs_if_needed2 = \case
  "a'to_string" -> show_val2
  hs -> hs

-- AST conversions
--   types

ft_to_st :: FieldType -> SimpleType
ft_to_st = \case
  PBT1 pbt -> pbt_to_st pbt
  PoT2 pt -> PoT1 pt

pbt_to_st :: PowerBaseType -> SimpleType
pbt_to_st = \case
  PTV2 ptv -> PTV1 ptv
  TAIOA2 taioa -> TAIOA1 taioa
  IPT ipt -> ipt_to_st ipt

ipt_to_st :: InParenT -> SimpleType
ipt_to_st = \case
  PT3 pt -> PT1 pt
  FT3 ft -> FT1 ft

taioasm_to_sou :: TAIOASMiddle -> SubOrUnder
taioasm_to_sou = \taioasm ->
  TVS1 $ TAIOAS1 $ TAIOAS (Nothing, taioasm, Nothing)

--   identifiers

str_to_sid :: String -> SimpleId
str_to_sid = \str -> SId (IS str, Nothing)

str_to_id :: String -> Identifier
str_to_id = \str -> Id (Nothing, IS str, [], Nothing, Nothing)

sid_to_pfaoi :: SimpleId -> ParenFuncAppOrId
sid_to_pfaoi = \(SId (id_start, mdigit)) ->
  PFAOI (Nothing, id_start, [], mdigit, Nothing)

sid_to_id :: SimpleId -> Identifier
sid_to_id = \(SId (id_start, mdigit)) ->
  Id (Nothing, id_start, [], mdigit, Nothing)

-- AST checks

check_if_pfaoi_is_sid :: ParenFuncAppOrId -> Maybe SimpleId
check_if_pfaoi_is_sid = \case
  PFAOI (Nothing, id_strt, [], mdigit, Nothing) -> Just $ SId (id_strt, mdigit)
  _ -> Nothing

check_if_id_is_sid :: Identifier -> Maybe SimpleId
check_if_id_is_sid = \case
  Id (Nothing, ids, [], mdigit, Nothing) -> Just $ SId (ids, mdigit)
  _ -> Nothing

{-
For fast vim file navigation:
Collect.hs
AST.hs
PrefixesAndHardcoded.hs
-}
