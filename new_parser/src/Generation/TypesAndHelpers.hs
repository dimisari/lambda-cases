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

newtype THWIL a = THWIL a

-- classes
class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_wpn :: a -> WithParamNum Haskell

class ToHsWithIndentLvl a where
  to_hs_wil :: a -> WithIndentLvl Haskell

-- classes helpers
instance ToHaskell a => ToHaskell (Maybe a) where 
  to_haskell = \case
    Nothing -> ""
    Just a -> to_haskell a

instance ToHaskell a => ToHaskell [a] where 
  to_haskell = concatMap to_haskell

instance ToHsWithParamNum a => ToHsWithParamNum (Maybe a) where
  to_hs_wpn = \case
    Just a -> to_hs_wpn a 
    Nothing -> return ""

instance ToHsWithIndentLvl a => ToHsWithIndentLvl (Maybe a) where
  to_hs_wil = \case
    Just a -> to_hs_wil a 
    Nothing -> return ""

to_hs_prepend_list :: ToHaskell a => String -> [a] -> Haskell
to_hs_prepend_list = \sep -> concatMap ((sep ++) . to_haskell)

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_wpn

to_hs_wil_list :: ToHsWithIndentLvl a => [a] -> WithIndentLvl [Haskell]
to_hs_wil_list = traverse to_hs_wil

to_hs_wpn_pair
  :: (ToHsWithParamNum a, ToHsWithParamNum b) => (a, b) -> WithParamNum Haskell
to_hs_wpn_pair = \(a, b) ->
  to_hs_wpn a >>= \a_hs ->
  to_hs_wpn b >>= \b_hs ->
  return $ a_hs ++ b_hs

-- params helpers
get_next_param :: WithParamNum Haskell
get_next_param = get $> (\i -> "x" ++ show i ++ "'") <* modify (+1)

params_hs_gen :: WithParamNum Haskell
params_hs_gen =
  get $> \case
    0 -> ""
    i -> "\\" ++ concatMap (\j -> "x" ++ show j ++ "' ") [0..i-1] ++ "-> "

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

indent_all_and_concat :: [Haskell] -> WithParamNum Haskell
indent_all_and_concat = \hs_list ->
  indent >>= \indent_hs ->
  return $ concatMap (\hs -> indent_hs ++ hs ++ "\n") hs_list

run_generator :: State Int a -> a
run_generator = \hs_gen -> evalState hs_gen 0

-- case of
case_of_inner_hs_gen :: WithParamNum Haskell
case_of_inner_hs_gen =
  get $> \case
    0 -> error "should be impossible: no cases param"
    1 -> "x0'"
    i ->
      "(" ++ map (\j -> "x" ++ show j ++ "'") [0..i-1] &>
      intercalate ", " ++ ")"

case_of_hs_gen :: Haskell -> WithParamNum Haskell
case_of_hs_gen = \cps_hs ->
  case_of_inner_hs_gen >>= \case_of_inner_hs ->
  return $ "\\" ++ cps_hs ++ " -> case " ++ case_of_inner_hs ++ " of"

-- to_hs_maybe_np
to_hs_maybe_np :: Maybe NamePart -> Haskell
to_hs_maybe_np = \case
  Nothing -> ""
  Just (NP str) -> "'" ++ str

-- Indetation helpers
change_indent_lvl :: Int -> WithIndentLvl ()
change_indent_lvl = \i -> modify (+i)

inc_indent_lvl :: WithIndentLvl ()
inc_indent_lvl = change_indent_lvl 1

dec_indent_lvl :: WithIndentLvl ()
dec_indent_lvl = change_indent_lvl (-1)

indent = indent_spaces <$> get
  :: WithIndentLvl Haskell

indent_spaces = ( \i -> replicate (2 * i) ' ' )
  :: Int -> Haskell

-- GroupedValueDefs helpers
gvds_to_vd_list :: GroupedValueDefs -> [ValueDef]
gvds_to_vd_list = \(GVDs (id, ids, ts, les, les_l)) ->
  let
  total_ids :: [Identifier]
  total_ids = id:ids

  t_list :: [Type]
  t_list = case ts of
    Ts (t, ts) -> t:ts
    All t -> replicate (length total_ids) t

  total_le_list :: [LineExpr]
  total_le_list =
    concatMap (\(LEs (le, le_list)) -> le : le_list) (les : les_l)
  in
  (total_ids, t_list, total_le_list) &> ids_ts_les_to_id_t_le_list &>
  map id_t_le_to_value_def

ids_ts_les_to_id_t_le_list
  :: ([Identifier], [Type], [LineExpr]) -> [(Identifier, Type, LineExpr)]
ids_ts_les_to_id_t_le_list = \case
  ([], [], []) -> []
  (id:ids, t:ts, le:les) ->
    (id, t, le):ids_ts_les_to_id_t_le_list (ids, ts, les)
  _ ->
    error $
      "identifiers, types and expressions don't match in number " ++
      "in grouped value definitions"

id_t_le_to_value_def :: (Identifier, Type, LineExpr) -> ValueDef
id_t_le_to_value_def = \(id, t, le) -> VD (id, t, le_to_ve le, Nothing)

le_to_ve :: LineExpr -> ValueExpr
le_to_ve = \case
  BOAE1 boae -> BOAE5 boae
  LOE2 loe -> OE2 $ LOE3 loe
  LFE2 lfe -> FE2 $ LFE4 lfe

-- Test.hs
