{-
This file contains code that
-}

{-# language
  LambdaCase, TypeSynonymInstances, FlexibleInstances, StandaloneDeriving,
  GeneralizedNewtypeDeriving
#-}

module Generation.Preprocess where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

import ASTTypes
import Helpers
import ShowInstances

import Parsing.AST
import Generation.Collect
import Generation.CheckCompatibility
import Generation.TypesAndHelpers
import Generation.PrefixesAndHardcoded

-- types

data PossiblyInDC =
  InDotChange [PostFuncArg] | NotInDotChange

type StateTuple =
  (PossiblyInDC, FieldIds, EmptyOrValues, RenamingProps, FullOrValuesMap)
type PreprocessState = State StateTuple

-- Preprocess class, automatic instances and

class Preprocess a where
  preprocess :: a -> PreprocessState a

instance Preprocess a => Preprocess [a] where
  preprocess = traverse preprocess

instance Preprocess a => Preprocess (Maybe a) where
  preprocess = traverse preprocess

preprocess_prog :: Program -> Program
preprocess_prog = \prog -> evalState (preprocess prog) (init_state prog)

-- change basic expr to post func app if it is a simple or special id and
-- we are inside a dot change epxression

class ToMaybePostFuncApp a where
  to_maybe_post_func_app :: a -> PreprocessState (Maybe PostFuncApp)

instance ToMaybePostFuncApp BasicExpr where
  to_maybe_post_func_app = \case
    PFAOI1 pfaoi -> to_maybe_post_func_app pfaoi
    SI1 spid -> to_maybe_post_func_app spid
    _ -> return Nothing

instance ToMaybePostFuncApp ParenFuncAppOrId where
  to_maybe_post_func_app =
    check_if_pfaoi_is_sid .> \case
      Nothing -> return Nothing
      Just sid -> to_maybe_post_func_app sid

instance ToMaybePostFuncApp SpecialId where
  to_maybe_post_func_app = \spid -> to_maybe_post_func_app $ SI2 spid

instance ToMaybePostFuncApp SimpleId where
  to_maybe_post_func_app = \sid ->
    check_if_sid_in_fids sid >>= \case
      True -> to_maybe_post_func_app $ SId1 sid
      _ -> return Nothing

instance ToMaybePostFuncApp PostFunc where
  to_maybe_post_func_app = \pf ->
    get_pfarg_if_in_dot_change >$>
    fmap (\pfarg -> pfarg_pf_to_pfapp (pfarg, pf))

pfarg_pf_to_pfapp :: (PostFuncArg, PostFunc) -> PostFuncApp
pfarg_pf_to_pfapp = \(pfarg, pf) ->
  PoFA (change_pfarg_if_under pfarg, PFsMDC ([pf], Nothing))

change_pfarg_if_under :: PostFuncArg -> PostFuncArg
change_pfarg_if_under = \case
  Underscore2 -> BE2 $ PFAOI1 $ sid_to_pfaoi (SId (IS "x'", Nothing))
  other -> other

-- preprocess for pairs and triples

preprocess_pair
  :: (Preprocess a, Preprocess b) => (a, b) -> PreprocessState (a, b)
preprocess_pair = \(a, b) -> preprocess a ++< preprocess b

preprocess_triple
  :: (Preprocess a, Preprocess b, Preprocess c) =>
     (a, b, c) -> PreprocessState (a, b, c)
preprocess_triple = \(a, b, c) ->
  preprocess a ++< preprocess b +++< preprocess c

preprocess_first :: Preprocess a => (a, b) -> PreprocessState (a, b)
preprocess_first = \(a, b) -> preprocess a >$> \a' -> (a', b)

preprocess_second :: Preprocess b => (a, b) -> PreprocessState (a, b)
preprocess_second = \(a, b) -> preprocess b >$> \b' -> (a, b')

-- regular instances

instance Preprocess Identifier where
  preprocess = \id ->
    check_if_id_is_sid id &> \case
      Nothing -> return id
      Just sid -> preprocess sid >$> sid_to_id

instance Preprocess SimpleId where
  preprocess = \sid ->
    check_if_sid_in_ncs sid >$> \case
      True -> add_c_to_sid sid
      False -> change_if_particular_sid sid

deriving instance Preprocess ParenExpr

instance Preprocess InsideParenExpr where
  preprocess = \case
    LOE1 loe -> LOE1 <$> preprocess loe
    LFE1 lfe -> LFE1 <$> preprocess lfe

instance Preprocess Tuple where
  preprocess = \(T t) -> T <$> preprocess_pair t

instance Preprocess LineExprOrUnders where
  preprocess = \(LEOUs leous) -> LEOUs <$> preprocess_pair leous

instance Preprocess LineExprOrUnder where
  preprocess = \case
    LE1 le -> LE1 <$> preprocess le
    Underscore1 -> return Underscore1

instance Preprocess LineExpr where
  preprocess = \case
    BOAE1 boae -> BOAE1 <$> preprocess boae
    LOE2 loe -> LOE2 <$> preprocess loe
    LFE2 lfe -> LFE2 <$> preprocess lfe

instance Preprocess BasicOrAppExpr where
  preprocess = \case
    BE3 be ->
      to_maybe_post_func_app be >>= \case
        Nothing -> BE3 <$> preprocess be
        Just pfapp -> return $ PoFA1 pfapp
    PrFA1 prfa -> PrFA1 <$> preprocess prfa
    PoFA1 pofa -> PoFA1 <$> preprocess pofa

instance Preprocess BasicExpr where
  preprocess = \case
    PFAOI1 pfaoi -> PFAOI1 <$> preprocess pfaoi
    T1 t -> T1 <$> preprocess t
    L1 l -> L1 <$> preprocess l
    other -> return other

instance Preprocess BigTuple where
  preprocess = \(BT (leou, bts, leous, leous_l)) ->
    preprocess_triple (leou, leous, leous_l) >$>
    \(leou', leous', leous_l') -> BT (leou', bts, leous', leous_l')

deriving instance Preprocess List

instance Preprocess BigList where
  preprocess = \(BL bl) -> BL <$> preprocess_pair bl

instance Preprocess ArgsStr where
  preprocess = preprocess_first

instance Preprocess ParenFuncAppOrId where
  preprocess =
    \pfaoi@(PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
    check_if_pfaoi_is_sid pfaoi &> \case
      Just sid -> preprocess sid >$> sid_to_pfaoi
      Nothing ->
        preprocess_triple (margs1, args_str_pairs, margs2) >$>
        \(margs1', args_str_pairs', margs2') ->
        PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

instance Preprocess Arguments where
  preprocess = \(As leous) -> As <$> preprocess leous

instance Preprocess PreFuncApp where
  preprocess = \(PrFA prfa) -> PrFA <$> preprocess_second prfa

instance Preprocess PostFuncApp where
  preprocess = \(PoFA (pfarg, pfae)) ->
    preprocess pfarg >>= \pfarg' ->
    push_post_func_arg pfarg >>
    preprocess pfae >>= \pfae' ->
    pop_post_func_arg >>
    return (PoFA (pfarg', pfae'))

instance Preprocess PostFuncArg where
  preprocess = \case
    PE2 pe -> PE2 <$> preprocess pe
    BE2 be -> BE2 <$> preprocess be
    Underscore2 -> return Underscore2

instance Preprocess PostFuncAppEnd where
  preprocess = \case
    DC1 dc -> DC1 <$> preprocess dc
    PFsMDC pfs_mdc -> PFsMDC <$> preprocess_second pfs_mdc

instance Preprocess DotChange where
  preprocess = \(DC dc) -> DC <$> preprocess_pair dc

instance Preprocess FieldChange where
  preprocess = \(FC fc) -> FC <$> preprocess_second fc

instance Preprocess OpExpr where
  preprocess = \case
    LOE3 loe -> LOE3 <$> preprocess loe
    BOE1 boe -> BOE1 <$> preprocess boe

instance Preprocess OpExprStart where
  preprocess = \(OES oper_op_list) -> OES <$> preprocess oper_op_list

instance Preprocess (Operand, Op) where
  preprocess = preprocess_first

instance Preprocess LineOpExpr where
  preprocess = \(LOE loe) -> LOE <$> preprocess_pair loe

instance Preprocess LineOpExprEnd where
  preprocess = \case
    O1 o -> O1 <$> preprocess o
    LFE3 lfe -> LFE3 <$> preprocess lfe

instance Preprocess BigOpExpr where
  preprocess = \case
    BOEOS1 boeos -> BOEOS1 <$> preprocess boeos
    BOEFS1 boefs -> BOEFS1 <$> preprocess boefs

instance Preprocess BigOpExprOpSplit where
  preprocess = \(BOEOS boeos) -> BOEOS <$> preprocess_triple boeos

instance Preprocess OpSplitLine where
  preprocess = \case
    OESMOFCO oesmofco -> OESMOFCO <$> preprocess_pair oesmofco
    OFCO1 ofco -> OFCO1 <$> preprocess ofco

instance Preprocess OperFCO where
  preprocess = \(OFCO oper_fco) -> OFCO <$> preprocess_first oper_fco

instance Preprocess OpSplitEnd where
  preprocess = \case
    O2 o -> O2 <$> preprocess o
    FE1 fe -> FE1 <$> preprocess fe

instance Preprocess BigOpExprFuncSplit where
  preprocess = \(BOEFS boefs) -> BOEFS <$> preprocess_pair boefs

instance Preprocess BigOrCasesFuncExpr where
  preprocess = \case
    BFE1 bfe -> BFE1 <$> preprocess bfe
    CFE1 cfe -> CFE1 <$> preprocess cfe

instance Preprocess Operand where
  preprocess = \case
    BOAE2 boae -> BOAE2 <$> preprocess boae
    PE3 pe -> PE3 <$> preprocess pe
    Underscore3 -> return Underscore3

instance Preprocess FuncExpr where
  preprocess = \case
    LFE4 lfe -> LFE4 <$> preprocess lfe
    BFE2 bfe -> BFE2 <$> preprocess bfe
    CFE2 cfe -> CFE2 <$> preprocess cfe

instance Preprocess LineFuncExpr where
  preprocess = \(LFE lfe) -> LFE <$> preprocess_second lfe

instance Preprocess BigFuncExpr where
  preprocess = \(BFE bfe) -> BFE <$> preprocess_second bfe

instance Preprocess LineFuncBody where
  preprocess = \case
    BOAE3 boae -> BOAE3 <$> preprocess boae
    LOE4 loe -> LOE4 <$> preprocess loe
    LFE5 lfe -> LFE5 <$> preprocess lfe

instance Preprocess BigFuncBody where
  preprocess = \case
    BOAE4 boae -> BOAE4 <$> preprocess boae
    OE1 oe -> OE1 <$> preprocess oe
    LFE6 lfe -> LFE6 <$> preprocess lfe

instance Preprocess CasesFuncExpr where
  preprocess = \(CFE (cparams, cases, mec)) ->
    preprocess_pair (cases, mec) >$> \(cases', mec') ->
    CFE (cparams, cases', mec')

instance Preprocess Case where
  preprocess = \(Ca om_cb) -> Ca <$> preprocess_pair om_cb

instance Preprocess EndCase where
  preprocess = \(EC ecp_cb) -> EC <$> preprocess_second ecp_cb

instance Preprocess OuterMatching where
  preprocess = \case
    SId3 sid ->
      lookup_sid_in_ovm sid >>= \case
        Just id -> return $ M1 $ PFM (PF sid, Id2 id)
        Nothing -> SId3 <$> preprocess sid
    M1 m -> M1 <$> preprocess m

instance Preprocess Matching where
  preprocess = \case
    PFM pfm -> PFM <$> preprocess_second pfm
    TM1 tm -> TM1 <$> preprocess tm
    LM1 lm -> LM1 <$> preprocess lm
    lit -> return lit

instance Preprocess InnerMatching where
  preprocess = \case
    Star -> return Star
    Id2 id -> Id2 <$> preprocess id
    M2 m -> M2 <$> preprocess m

instance Preprocess TupleMatching where
  preprocess = \(TM ims) -> TM <$> preprocess_pair ims

instance Preprocess ListMatching where
  preprocess = \(LM m_list_internals) -> LM <$> preprocess m_list_internals

instance
  Preprocess (InnerMatching, [InnerMatching], Maybe RestListMatching)
  where
  preprocess = \(im, ims, mrlm) ->
    preprocess_pair (im, ims) >$> \(im', ims') -> (im', ims', mrlm)

instance Preprocess CaseBody where
  preprocess = \case
    LFB1 lfb -> LFB1 <$> preprocess lfb
    BFB1 bfb -> BFB1 <$> preprocess_pair bfb

instance Preprocess ValueDef where
  preprocess = \(VD (id, t, ve, mwe)) ->
    preprocess_pair (ve, mwe) >$> \(ve', mwe') -> VD (id, t, ve', mwe')

instance Preprocess ValueExpr where
  preprocess = \case
    BOAE5 boae -> BOAE5 <$> preprocess boae
    OE2 oe -> OE2 <$> preprocess oe
    FE2 fe -> FE2 <$> preprocess fe
    BT1 bt -> BT1 <$> preprocess bt
    BL1 bl -> BL1 <$> preprocess bl

instance Preprocess GroupedValueDefs where
  preprocess = \(GVDs (id, ids, ts, les, les_l)) ->
    preprocess_pair (les, les_l) >$> \(les', les_l') ->
    GVDs (id, ids, ts, les', les_l')

instance Preprocess LineExprs where
  preprocess = \(LEs les) -> LEs <$> preprocess_pair les

instance Preprocess WhereExpr where
  preprocess = \(WE we) -> WE <$> preprocess_pair we

instance Preprocess WhereDefExpr where
  preprocess = \case
    VD1 vd -> VD1 <$> preprocess vd
    GVDs1 gvds -> GVDs1 <$> preprocess gvds

instance Preprocess TypeTheo where
  preprocess (TT (pnws_l, mpnws, proof)) =
    preprocess_pair (pnws_l, proof) >$> \(pnws_l', proof') ->
    TT (pnws_l', mpnws, proof')

instance {-# OVERLAPS #-} Preprocess [PropNameWithSubs] where
  preprocess = \case
    [pnws] ->
      get_rps >$> map (\rp -> (check_compat(fst rp, pnws), snd rp)) >$>
      filter (fst .> (/= NotCompatible)) >$> \case
        [] -> [pnws]
        [(Compatible m, pns)] ->
          map (\pn -> evalState (add_subs pn) m) pns
        _ -> error "proprocess pnws: more than one rps compatible"
    _ ->
      error
      "Should be impossible: many pnws at type theo before preprocessing"

instance Preprocess Proof where
  preprocess = \case
    P1 iooe_le -> P1 <$> preprocess_second iooe_le
    P2 iooe_ttve -> P2 <$> preprocess_second iooe_ttve

instance Preprocess TTValueExpr where
  preprocess = \case
    LE2 le -> LE2 <$> preprocess le
    VEMWE vemwe -> VEMWE <$> preprocess_pair vemwe

instance Preprocess Program where
  preprocess = \(P pps) -> P <$> preprocess_pair pps

instance Preprocess ProgramPart where
  preprocess = \case
    VD2 vd -> VD2 <$> preprocess vd
    GVDs2 gvds -> GVDs2 <$> preprocess gvds
    TT1 tt -> TT1 <$> preprocess tt
    other -> return other

-- State helpers
--   initial state

init_state :: Program -> StateTuple
init_state = \prog ->
  or_values prog &> \(empty_or_values, full_or_values_map) ->
  ( NotInDotChange, field_ids prog, empty_or_values, renaming_props prog
  , full_or_values_map
  )

--   individual getters

get_pidc :: PreprocessState PossiblyInDC
get_pidc = get >$> \(pidc, _, _, _, _) -> pidc

get_fids :: PreprocessState FieldIds
get_fids = get >$> \(_, fids, _, _, _) -> fids

get_ncs :: PreprocessState EmptyOrValues
get_ncs = get >$> \(_, _, ncs, _, _) -> ncs

get_rps :: PreprocessState RenamingProps
get_rps = get >$> \(_, _, _, rps, _) -> rps

get_ovm :: PreprocessState FullOrValuesMap
get_ovm = get >$> \(_, _, _, _, ovm) -> ovm

--   checking membership and lookup

check_if_sid_in_fids :: SimpleId -> PreprocessState Bool
check_if_sid_in_fids = \sid -> S.member sid <$> get_fids

check_if_sid_in_ncs :: SimpleId -> PreprocessState Bool
check_if_sid_in_ncs = \sid -> S.member sid <$> get_ncs

lookup_sid_in_ovm :: SimpleId -> PreprocessState (Maybe Identifier)
lookup_sid_in_ovm = \sid -> M.lookup sid <$> get_ovm

--   post func arg
--     push/pop

push_post_func_arg :: PostFuncArg -> PreprocessState ()
push_post_func_arg = \pfarg ->
  get_pidc >>= \case
    NotInDotChange -> in_dot_change_with_new_args [pfarg]
    InDotChange pfargs -> in_dot_change_with_new_args $ pfarg : pfargs

pop_post_func_arg :: PreprocessState ()
pop_post_func_arg =
  get_pidc >>= \case
    InDotChange [pfarg] -> change_pidc NotInDotChange
    InDotChange (pfarg : pfargs) -> in_dot_change_with_new_args pfargs
    _ -> error "should not be possible"

change_pidc :: PossiblyInDC -> PreprocessState ()
change_pidc = \pidc ->
  modify (\(_, fids, ncs, rps, ovm) -> (pidc, fids, ncs, rps, ovm))

in_dot_change_with_new_args :: [PostFuncArg] -> PreprocessState ()
in_dot_change_with_new_args = \pfargs -> change_pidc $ InDotChange pfargs

--     get if in dot change

get_pfarg_if_in_dot_change :: PreprocessState (Maybe PostFuncArg)
get_pfarg_if_in_dot_change =
  get_pidc >$> \case
    NotInDotChange -> Nothing
    InDotChange [] -> error "should not be possible"
    InDotChange (pfarg : pfargs) -> Just pfarg

-- helpers

add_c_to_sid :: SimpleId -> SimpleId
add_c_to_sid = \(SId (IS str, mdigit)) -> SId (IS $ "C" ++ str, mdigit)

change_if_particular_sid :: SimpleId -> SimpleId
change_if_particular_sid = \case
  SId (IS str, Nothing) -> str &> change_if_particular_str &> str_to_sid
  sid -> sid

change_if_particular_str :: String -> String
change_if_particular_str = \case
  "true" -> true
  "false" -> false
  "no_value" -> pnothing
  "print" -> pprint
  "undefined" -> pundefined
  "pi" -> ppi
  str -> str

{-
For fast vim file navigation:
CheckCompatibility.hs
AST.hs
-}
