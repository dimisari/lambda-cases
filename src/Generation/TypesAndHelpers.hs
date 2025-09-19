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

import Prelude (($), (++), (<*), (*>), (+), (>), (>>=), (-), (.))
import Prelude qualified as P
import Control.Monad.State.Lazy qualified as MS

import Data.List qualified as L
import Data.Set qualified as S

import ASTTypes qualified as T
import Helpers ((.>), (&>), (>$>))
import Helpers qualified as H
import Generation.PrefixesAndHardcoded qualified as GPH

-- ToHaskell

type Haskell = P.String

class ToHaskell a where
  to_haskell :: a -> Haskell

instance ToHaskell a => ToHaskell [a] where
  to_haskell = P.concatMap to_haskell

instance ToHaskell a => ToHaskell (P.Maybe a) where
  to_haskell = \case
    P.Nothing -> ""
    P.Just a -> to_haskell a

to_hs_prepend_list :: ToHaskell a => P.String -> [a] -> Haskell
to_hs_prepend_list = \prepend_hs -> P.concatMap ((prepend_hs ++) . to_haskell)

-- WithParamNum

type WithParamNum = MS.State P.Int

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

instance ToHsWithParamNum a => ToHsWithParamNum (P.Maybe a) where
  to_hs_wpn = \case
    P.Nothing -> P.return ""
    P.Just a -> to_hs_wpn a

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = P.traverse to_hs_wpn

generate_next_param :: WithParamNum Haskell
generate_next_param = MS.get >$> int_to_param <* MS.modify (+1)

params_hs_gen :: WithParamNum Haskell
params_hs_gen =
  MS.get >$> int_to_params
  where
  int_to_params :: P.Int -> Haskell
  int_to_params = \case
    0 -> ""
    1 -> "\\" ++ int_to_param 0 ++ " -> "
    i -> case i > 1 of
      P.True -> "\\" ++ int_to_params_in_paren i ++ " -> "
      P.False -> P.error "should be impossible: negative num of params"

int_to_params_in_paren :: P.Int -> Haskell
int_to_params_in_paren = \i ->
  "(" ++ ([0..i-1] &> P.map int_to_param &> L.intercalate ", ") ++ ")"

int_to_param :: P.Int -> Haskell
int_to_param = \j -> GPH.param_prefix ++ P.show j

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
  MS.get >$> \case
    1 -> int_to_param 0
    i -> case i > 1 of
      P.True -> int_to_params_in_paren i
      P.False -> P.error "should be impossible: zero or negative cases params"

-- WithIndentLvl

type WithIndentLvl = MS.State P.Int

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

instance ToHsWithIndentLvl a => ToHsWithIndentLvl (P.Maybe a) where
  to_hs_wil = \case
    P.Nothing -> P.return ""
    P.Just a -> to_hs_wil a

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = P.traverse to_hs_wil

increase_il_by :: P.Int -> WithIndentLvl ()
increase_il_by = \i -> MS.modify (+i)

decrease_il_by :: P.Int -> WithIndentLvl ()
decrease_il_by = \i -> increase_il_by (-i)

deeper_with_num :: P.Int -> WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper_with_num = \i hs_gen ->
  increase_il_by i *> hs_gen <* decrease_il_by i

deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
deeper = deeper_with_num 1

twice_deeper :: WithIndentLvl Haskell -> WithIndentLvl Haskell
twice_deeper = deeper_with_num 2

indent :: WithIndentLvl Haskell
indent = MS.get >$> H.ind_lvl_to_spaces

indent_all_and_concat :: [Haskell] -> WithIndentLvl Haskell
indent_all_and_concat = \hs_list ->
  indent >$> \indent_hs -> P.map (indent_hs ++) hs_list &> L.intercalate "\n"

-- HasArgs

class HasArgs a where
  args_length :: a -> P.Int

instance HasArgs a => HasArgs (P.Maybe a) where
  args_length = \case
    P.Just a -> args_length a
    P.Nothing -> 0

instance HasArgs T.Arguments where
  args_length = \(T.As (T.LEOUs (leou, leous))) -> P.length $ leou : leous

instance HasArgs T.TypesInParen where
  args_length = \(T.TIP (st, sts)) -> P.length $ st : sts

instance HasArgs T.SubsOrUndersInParen where
  args_length = \(T.SOUIP (sou, sous)) -> P.length $ sou : sous

instance HasArgs T.ParamVarsInParen where
  args_length = \(T.PVIP (ptv, ptvs)) -> P.length $ ptv : ptvs

instance HasArgs T.SubsInParen where
  args_length = \(T.SIP (tvs, tvss)) -> P.length $ tvs : tvss

instance HasArgs T.UndersInParen where
  args_length = \(T.UIP i) -> i

single_quotes_hs :: HasArgs a => a -> P.String
single_quotes_hs = args_length .> \i -> L.replicate i '\''

args_strs_hs :: HasArgs a => [(a, P.String)] -> Haskell
args_strs_hs = P.concatMap (\(a, str) -> single_quotes_hs a ++ str)

args_nps_hs :: HasArgs a => [(a, T.NamePart)] -> Haskell
args_nps_hs =
  P.concatMap (\(a, T.NP str) -> single_quotes_hs a ++ str) .> (GPH.upper_prefix ++)

nps_args_hs :: HasArgs a => [(T.NamePart, a)] -> Haskell
nps_args_hs = P.concatMap (\(T.NP str, a) -> str ++ single_quotes_hs a)

maybe_prefix_args_hs :: HasArgs a => P.String -> P.Maybe a -> Haskell
maybe_prefix_args_hs = \prefix -> \case
  P.Nothing -> ""
  P.Just a -> prefix ++ single_quotes_hs a

-- types

type DotChangeArgHs = Haskell

newtype WholeParams = Whole T.Parameters

-- NeedsParenBool

data NeedsParenBool = Paren | NoParen

in_paren_if :: NeedsParenBool -> Haskell -> Haskell
in_paren_if = \case
  NoParen -> P.id
  Paren -> \hs -> "(" ++ hs ++ ")"

-- NeedsAnnotBool

data NeedsAnnotBool = Annot | NoAnnot

to_hs_needs_annot :: P.Show a => NeedsAnnotBool -> a -> P.String -> Haskell
to_hs_needs_annot = \needs_annot a str -> case needs_annot of
  Annot -> "(" ++ P.show a ++ " :: " ++ str ++ ")"
  NoAnnot -> P.show a

-- PossiblyWhereExpr

data PossiblyWhereExpr = HasWhereExpr T.WhereExpr | NoWhereExpr

mwe_to_pwe :: P.Maybe T.WhereExpr -> PossiblyWhereExpr
mwe_to_pwe = \case
  P.Nothing -> NoWhereExpr
  P.Just we -> HasWhereExpr we

-- ParenFuncAppOrId helpers

type MargsPair = (P.Maybe T.Arguments, P.Maybe T.Arguments)
add_margs_to_args_list :: MargsPair -> [T.Arguments] -> [T.Arguments]
add_margs_to_args_list = \case
  (P.Nothing, P.Nothing) -> P.id
  (P.Just args, P.Nothing) -> ([args] ++)
  (P.Nothing, P.Just args) -> (++ [args])
  (P.Just args1, P.Just args2) -> \args_list -> [args1] ++ args_list ++ [args2]

calc_args_list :: MargsPair -> [T.ArgsStr] -> [T.Arguments]
calc_args_list = \margs_pair args_str_pairs ->
  add_margs_to_args_list margs_pair $ P.map P.fst args_str_pairs

-- TupleTypeDef helpers

combine_with_ts :: [Haskell] -> [Haskell] -> Haskell
combine_with_ts = \sid_hs_list types_hs_list ->
  P.zipWith comb_sid_t_hs sid_hs_list types_hs_list &> P.concat

combine_with_defs :: [Haskell] -> [Haskell] -> Haskell
combine_with_defs = \sid_hs_list defs_hs_list ->
  P.zipWith comb_sid_def_hs sid_hs_list defs_hs_list &> P.concat

comb_sid_t_hs :: Haskell -> Haskell -> Haskell
comb_sid_t_hs = \hs1 hs2 -> "\n" ++ hs1 ++ " :: " ++ hs2

comb_sid_def_hs :: Haskell -> Haskell -> Haskell
comb_sid_def_hs = \hs1 hs2 ->
  "\n" ++ hs1 ++ " = \\new x -> x { " ++ hs2 ++ " = new }"

-- other

run_generator :: MS.State P.Int a -> a
run_generator = \hs_gen -> MS.evalState hs_gen 0

to_hs_maybe_np :: P.Maybe T.NamePart -> Haskell
to_hs_maybe_np = \case
  P.Nothing -> ""
  P.Just (T.NP str) -> "'" ++ str

tn_to_tid_hs :: T.TypeName -> Haskell
tn_to_tid_hs = \(T.TN (mpvip1, T.TId str, pvip_str_pairs, mpvip2)) ->
  maybe_prefix_args_hs GPH.upper_prefix mpvip1 ++ str ++
  args_strs_hs pvip_str_pairs ++ single_quotes_hs mpvip2

tn_to_cons_hs :: T.TypeName -> Haskell
tn_to_cons_hs = tn_to_tid_hs .> (++ "'")

change_prop_hs_if_needed :: Haskell -> Haskell
change_prop_hs_if_needed = \case
  "A'Has_Str_Rep" -> GPH.show_class
  hs -> hs

change_id_hs_if_needed1 :: Haskell -> Haskell
change_id_hs_if_needed1 = \case
  "a'to_string" -> GPH.show_val1
  hs -> hs

change_id_hs_if_needed2 :: Haskell -> Haskell
change_id_hs_if_needed2 = \case
  "a'to_string" -> GPH.show_val2
  hs -> hs

-- AST conversions
--   types

ft_to_st :: T.FieldType -> T.SimpleType
ft_to_st = \case
  T.PBT1 pbt -> pbt_to_st pbt
  T.PoT2 pt -> T.PoT1 pt

pbt_to_st :: T.PowerBaseType -> T.SimpleType
pbt_to_st = \case
  T.PTV2 ptv -> T.PTV1 ptv
  T.TAIOA2 taioa -> T.TAIOA1 taioa
  T.IPT ipt -> ipt_to_st ipt

ipt_to_st :: T.InParenT -> T.SimpleType
ipt_to_st = \case
  T.PT3 pt -> T.PT1 pt
  T.FT3 ft -> T.FT1 ft

taioasm_to_sou :: T.TAIOASMiddle -> T.SubOrUnder
taioasm_to_sou = \taioasm ->
  T.TVS1 $ T.TAIOAS1 $ T.TAIOAS (P.Nothing, taioasm, P.Nothing)

--   identifiers

str_to_sid :: P.String -> T.SimpleId
str_to_sid = \str -> T.SId (T.IS str, P.Nothing)

str_to_id :: P.String -> T.Identifier
str_to_id = \str -> T.Id (P.Nothing, T.IS str, [], P.Nothing, P.Nothing)

sid_to_pfaoi :: T.SimpleId -> T.ParenFuncAppOrId
sid_to_pfaoi = \(T.SId (id_start, mdigit)) ->
  T.PFAOI (P.Nothing, id_start, [], mdigit, P.Nothing)

sid_to_id :: T.SimpleId -> T.Identifier
sid_to_id = \(T.SId (id_start, mdigit)) ->
  T.Id (P.Nothing, id_start, [], mdigit, P.Nothing)

-- AST checks

check_if_pfaoi_is_sid :: T.ParenFuncAppOrId -> P.Maybe T.SimpleId
check_if_pfaoi_is_sid = \case
  T.PFAOI (P.Nothing, id_strt, [], mdigit, P.Nothing) -> P.Just $ T.SId (id_strt, mdigit)
  _ -> P.Nothing

check_if_id_is_sid :: T.Identifier -> P.Maybe T.SimpleId
check_if_id_is_sid = \case
  T.Id (P.Nothing, ids, [], mdigit, P.Nothing) -> P.Just $ T.SId (ids, mdigit)
  _ -> P.Nothing

{-
For fast vim file navigation:
Collect.hs
AST.hs
PrefixesAndHardcoded.hs
-}
