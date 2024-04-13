{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Generation.TypesAndHelpers where

import Control.Monad.State.Lazy

import Data.List
import qualified Data.Set as S

import ASTTypes
import Helpers

import Generation.FieldIds

-- Haskell types
type Haskell = String

type HsPair = (Haskell, Haskell)

type PostFuncArgHs = Haskell

type PFAWithPostFuncsHs = Haskell

type DotChangeArgHs = Haskell

-- Halper types
data NeedsParenBool = Paren | NoParen

data NeedsAnnotationBool = Annotation | NoAnnotation

data PossiblyDCAHs = WithDCAHs DotChangeArgHs | NoDCAHs

newtype CaseOf = CaseOf CasesParams

newtype WholeParams = Whole Parameters

-- State types
type WithParamNum = State Int

type WithIndentLvl = State Int

type DotChangeState = State (PossiblyDCAHs, FieldIds)

type PNAndDCState = State (Int, PossiblyDCAHs, FieldIds)

-- classes
class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

class ToHsWithDCS a where
  to_hs_wdcs :: a -> DotChangeState Haskell

class ToHsWithPNDCS a where
  to_hs_wpndcs :: a -> PNAndDCState Haskell

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

instance
  ToHsWithParamNum (a, PossiblyDCAHs) =>
  ToHsWithParamNum (Maybe a, PossiblyDCAHs)
  where
  to_hs_wpn = \case
    (Nothing, _) -> return ""
    (Just a, hs) -> to_hs_wpn (a, hs)

instance ToHsWithIndentLvl a => ToHsWithIndentLvl (Maybe a) where
  to_hs_wil = \case
    Nothing -> return ""
    Just a -> to_hs_wil a

instance ToHsWithPNDCS a => ToHsWithPNDCS (Maybe a) where
  to_hs_wpndcs = \case
    Nothing -> return ""
    Just a -> to_hs_wpndcs a

-- list helpers
to_hs_prepend_list :: ToHaskell a => String -> [a] -> Haskell
to_hs_prepend_list = \sep -> concatMap ((sep ++) . to_haskell)

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_wpn

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = traverse to_hs_wil

to_hs_wpndcs_list :: ToHsWithPNDCS a => [a] -> PNAndDCState [Haskell]
to_hs_wpndcs_list = traverse to_hs_wpndcs

-- state helpers
get_param_num :: PNAndDCState Int
get_param_num = get $> \(pn, _, _) -> pn

inc_param_num :: PNAndDCState ()
inc_param_num = modify $ \(pn, pdcahs, fids) -> (pn + 1, pdcahs, fids)

get_pdcahs :: DotChangeState PossiblyDCAHs
get_pdcahs = get $> fst

get_fids :: DotChangeState FieldIds
get_fids = get $> snd

check_if_in_fids :: SimpleId -> DotChangeState Bool
check_if_in_fids = \sid -> S.member sid <$> get_fids

-- params helpers
get_next_param :: WithParamNum Haskell
get_next_param = get $> to_param <* modify (+1)

get_next_param_pndc :: PNAndDCState Haskell
get_next_param_pndc = get_param_num $> to_param <* inc_param_num

params_hs_gen :: WithParamNum Haskell
params_hs_gen = get $> to_params_hs

params_hs_gen_pndc :: PNAndDCState Haskell
params_hs_gen_pndc = get_param_num $> to_params_hs

to_params_hs :: Int -> Haskell
to_params_hs = \case
  0 -> ""
  1 -> "\\x0' -> "
  i -> "\\" ++ to_params_in_paren_hs i ++ " -> "

to_params_in_paren_hs :: Int -> Haskell
to_params_in_paren_hs = \i ->
  "(" ++ ([0..i-1] &> map to_param &> intercalate ", ") ++ ")"

to_param :: Int -> Haskell
to_param = \j -> "x" ++ show j ++ "'"

add_params_to :: WithParamNum Haskell -> WithParamNum Haskell
add_params_to = \hs_gen ->
  hs_gen >>= \hs ->
  params_hs_gen >>= \case
    "" -> return hs
    params_hs -> return $ "(" ++ params_hs ++ hs ++ ")"

add_params_to_pndc :: PNAndDCState Haskell -> PNAndDCState Haskell
add_params_to_pndc = \hs_gen ->
  hs_gen >>= \hs ->
  params_hs_gen_pndc >>= \case
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

-- ParenFuncAppOrId helpers

margs1_to_id_hs :: Maybe Arguments -> Haskell
margs1_to_id_hs = \case
  Nothing -> ""
  Just args -> "a0" ++ single_quotes args

margs2_to_id_hs :: Maybe Arguments -> Haskell
margs2_to_id_hs = \case
  Nothing -> ""
  Just args -> single_quotes args

single_quotes :: Arguments -> Haskell
single_quotes = \(As (LEOUs (leou, leous))) ->
  replicate (length $ leou : leous) '\''

args_str_pairs_to_id_hs :: [ArgsStr] -> Haskell
args_str_pairs_to_id_hs = concatMap (\(args, str) -> single_quotes args ++ str)

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
general_proj_hs_list = map ("p" ++) general_hs_list

general_change_hs_list :: [Haskell]
general_change_hs_list = map ("c" ++) general_hs_list

general_hs_list :: [Haskell]
general_hs_list = ["1st", "2nd", "3rd", "4th", "5th"]

to_proj_hs_list :: [Haskell] -> [Haskell]
to_proj_hs_list = map ("p0" ++)

to_change_hs_list :: [Haskell] -> [Haskell]
to_change_hs_list = map ("c0" ++)

--
add_to_hs_pair :: HsPair -> HsPair -> HsPair
add_to_hs_pair = \(hs1, hs2) (hs1', hs2') -> (hs1' ++ hs1, hs2' ++ hs2)

-- other
run_generator :: State Int a -> a
run_generator = \hs_gen -> evalState hs_gen 0

run_pndc_gen :: PNAndDCState a -> DotChangeState a
run_pndc_gen = \pndc_gen ->
  get $> \(pdcahs, fids) -> evalState pndc_gen (0, pdcahs, fids)

run_no_dcahs :: DotChangeState Haskell -> Haskell
run_no_dcahs = \dc_gen -> evalState dc_gen (NoDCAHs, mempty)

dcs_gen_to_pndc_gen :: DotChangeState a -> PNAndDCState a
dcs_gen_to_pndc_gen = \gcs_gen ->
  get $> \(_, pdcahs, fis) -> evalState gcs_gen (pdcahs, fis)

to_hs_maybe_np :: Maybe NamePart -> Haskell
to_hs_maybe_np = \case
  Nothing -> ""
  Just (NP str) -> "'" ++ str

in_paren_if :: NeedsParenBool -> Haskell -> Haskell
in_paren_if = \case
  NoParen -> id
  Paren -> \hs -> "(" ++ hs ++ ")"

no_dca_hs :: a -> (a, PossiblyDCAHs)
no_dca_hs = \a -> (a, NoDCAHs)

-- GroupedValueDefs helpers
-- ASTTypes.hs
-- FieldIds.hs
-- AST.hs
-- Test.hs
