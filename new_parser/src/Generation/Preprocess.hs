{-# LANGUAGE
  TypeSynonymInstances, FlexibleInstances, StandaloneDeriving,
  GeneralizedNewtypeDeriving
#-}

module Generation.Preprocess where

import Data.Set as S
import Control.Monad
import Control.Monad.State

import ASTTypes
import Helpers
import ShowInstances

import Parsing.AST
import Generation.Collect

-- types
data PossiblyInDC = InDotChange [PostFuncArg] | NotInDotChange

type DotChangeState = State (PossiblyInDC, FieldIds, NakedCases)
type DCS = DotChangeState
type PFAOI = ParenFuncAppOrId

-- class
class ChangeIfNeeded a where
  ch_inside_if_needed :: a -> DotChangeState a

-- change_prog_if_needed
change_prog_if_needed :: Program -> Program
change_prog_if_needed = \prog ->
  evalState (ch_inside_if_needed prog) (init_state prog)

-- State helpers: init, get, put
init_state :: Program -> (PossiblyInDC, FieldIds, NakedCases)
init_state = \prog -> (NotInDotChange, field_ids prog, naked_cases prog)

get_pidc :: DotChangeState PossiblyInDC
get_pidc = get >$> \(pidc, _, _) -> pidc

get_fids :: DotChangeState FieldIds
get_fids = get >$> \(_, fids, _) -> fids

get_ncs :: DotChangeState NakedCases
get_ncs = get >$> \(_, _, ncs) -> ncs

put_new_pidc :: PossiblyInDC -> DotChangeState ()
put_new_pidc = \pidc -> modify (\(_, fids, ncs) -> (pidc, fids, ncs))

put_new_args :: [PostFuncArg] -> DotChangeState ()
put_new_args = \pfargs -> put_new_pidc $ InDotChange pfargs

pfarg_if_in_dot_change :: DotChangeState (Maybe PostFuncArg)
pfarg_if_in_dot_change =
  get_pidc >$> \case
    NotInDotChange -> Nothing
    InDotChange [] -> error "should not be possible"
    InDotChange (pfarg : pfargs) -> Just pfarg

-- State helpers: post func arg push/pop
push_post_func_arg :: PostFuncArg -> DotChangeState ()
push_post_func_arg = \pfarg ->
  get_pidc >>= \case
    NotInDotChange -> put_new_args [pfarg]
    InDotChange pfargs -> put_new_args $ pfarg : pfargs

pop_post_func_arg :: DotChangeState ()
pop_post_func_arg =
  get_pidc >>= \case
    InDotChange [pfarg] -> put_new_pidc NotInDotChange
    InDotChange (pfarg : pfargs) -> put_new_args pfargs
    _ -> error "should not be possible"

-- change basic expr to post func app if it is a simple or special id and
-- we are inside a dot change epxression
change_be_if_needed :: BasicExpr -> DotChangeState BasicOrAppExpr
change_be_if_needed = \be -> pfapp_if_be_needs_change be >>= \case
  Nothing -> BE3 <$> ch_inside_if_needed be
  Just pfapp -> return $ PoFA1 pfapp

pfapp_if_be_needs_change :: BasicExpr -> DCS (Maybe PostFuncApp)
pfapp_if_be_needs_change = \case
  PFAOI1 pfaoi -> pfapp_if_pfaoi_needs_change pfaoi
  SI1 spid -> pfapp_if_spid_needs_change spid
  _ -> return Nothing

pfapp_if_pfaoi_needs_change :: PFAOI -> DCS (Maybe PostFuncApp)
pfapp_if_pfaoi_needs_change =
  check_if_pfaoi_is_sid .> \case
    Just sid -> pfapp_if_sid_needs_change sid
    _ -> return Nothing

pfapp_if_spid_needs_change :: SpecialId -> DCS (Maybe PostFuncApp)
pfapp_if_spid_needs_change = \spid ->
  pfarg_if_in_dot_change >$> \case
    Nothing -> Nothing
    Just pfarg -> Just $ pfarg_pf_to_pfapp (pfarg, SI2 spid)

pfapp_if_sid_needs_change :: SimpleId -> DotChangeState (Maybe PostFuncApp)
pfapp_if_sid_needs_change = \sid ->
  check_if_sid_in_fids sid >>= \in_fids ->
  pfarg_if_in_dot_change >>= \mpfarg ->
  return $ case (in_fids, mpfarg) of
    (True, Just pfarg) -> Just $ pfarg_pf_to_pfapp (pfarg, SId1 sid)
    _ -> Nothing

check_if_sid_in_fids :: SimpleId -> DotChangeState Bool
check_if_sid_in_fids = \sid -> S.member sid <$> get_fids

pfarg_pf_to_pfapp :: (PostFuncArg, PostFunc) -> PostFuncApp
pfarg_pf_to_pfapp = \(pfarg, pf) ->
  PoFA (change_pfarg_if_under pfarg, PFsMDC ([pf], Nothing))

change_pfarg_if_under :: PostFuncArg -> PostFuncArg
change_pfarg_if_under = \case
  Underscore2 -> BE2 $ PFAOI1 $ sid_to_pfaoi (SId (IS "x'", Nothing))
  other -> other

-- helpers
check_if_pfaoi_is_sid :: ParenFuncAppOrId -> Maybe SimpleId
check_if_pfaoi_is_sid = \case
  PFAOI (Nothing, id_strt, [], mdigit, Nothing) -> Just $ SId (id_strt, mdigit)
  _ -> Nothing

check_if_sid_in_ncs :: SimpleId -> DotChangeState Bool
check_if_sid_in_ncs = \sid -> S.member sid <$> get_ncs

if_sid_in_ncs_to_new_pfaoi :: SimpleId -> DotChangeState (Maybe PFAOI)
if_sid_in_ncs_to_new_pfaoi = \sid ->
  check_if_sid_in_ncs sid >$> \case
    True -> Just $ sid_to_pfaoi $ add_c_to_sid sid
    False -> Nothing

add_c_to_sid :: SimpleId -> SimpleId
add_c_to_sid = \(SId (IS str, mdigit)) -> SId (IS $ "C" ++ str, mdigit)

change_if_bool_sid :: SimpleId -> SimpleId
change_if_bool_sid = \case
  SId (IS "true", Nothing) -> SId (IS "True", Nothing)
  SId (IS "false", Nothing) -> SId (IS "False", Nothing)
  sid -> sid

change_sid_if_needed :: SimpleId -> DotChangeState SimpleId
change_sid_if_needed = \sid ->
  check_if_sid_in_ncs sid >$> \case
    True -> add_c_to_sid sid
    False -> change_if_bool_sid sid

sid_to_pfaoi :: SimpleId -> ParenFuncAppOrId
sid_to_pfaoi = \(SId (id_start, mdigit)) ->
  PFAOI (Nothing, id_start, [], mdigit, Nothing)

-- automatic instances
instance ChangeIfNeeded a => ChangeIfNeeded [a] where
  ch_inside_if_needed = traverse ch_inside_if_needed

instance ChangeIfNeeded a => ChangeIfNeeded (Maybe a) where
  ch_inside_if_needed = \case
    Nothing -> return Nothing
    Just a -> Just <$> ch_inside_if_needed a

-- ch_inside_if_needed for pairs and triples
ch_inside_if_needed_pair
  :: (ChangeIfNeeded a, ChangeIfNeeded b) => (a, b) -> DotChangeState (a, b)

ch_inside_if_needed_pair = \(a, b) ->
  ch_inside_if_needed a ++< ch_inside_if_needed b

ch_inside_if_needed_triple
  :: (ChangeIfNeeded a, ChangeIfNeeded b, ChangeIfNeeded c) =>
     (a, b, c) -> DotChangeState (a, b, c)

ch_inside_if_needed_triple = \(a, b, c) ->
  ch_inside_if_needed a ++< ch_inside_if_needed b +++< ch_inside_if_needed c

ch_inside_if_needed_first
  :: ChangeIfNeeded a => (a, b) -> DotChangeState (a, b)

ch_inside_if_needed_first = \(a, b) -> ch_inside_if_needed a >$> \a' -> (a', b)

ch_inside_if_needed_second
  :: ChangeIfNeeded b => (a, b) -> DotChangeState (a, b)

ch_inside_if_needed_second = \(a, b) ->
  ch_inside_if_needed b >$> \b' -> (a, b')

-- regular instances
deriving instance ChangeIfNeeded ParenExpr

instance ChangeIfNeeded InsideParenExpr where
  ch_inside_if_needed = \case
    LOE1 loe -> LOE1 <$> ch_inside_if_needed loe
    LFE1 lfe -> LFE1 <$> ch_inside_if_needed lfe

instance ChangeIfNeeded Tuple where
  ch_inside_if_needed = \(T t) -> T <$> ch_inside_if_needed_pair t

instance ChangeIfNeeded LineExprOrUnders where
  ch_inside_if_needed = \(LEOUs leous) ->
    LEOUs <$> ch_inside_if_needed_pair leous

instance ChangeIfNeeded LineExprOrUnder where
  ch_inside_if_needed = \case
    LE1 le -> LE1 <$> ch_inside_if_needed le
    Underscore1 -> return Underscore1

instance ChangeIfNeeded LineExpr where
  ch_inside_if_needed = \case
    BOAE1 boae -> BOAE1 <$> ch_inside_if_needed boae
    LOE2 loe -> LOE2 <$> ch_inside_if_needed loe
    LFE2 lfe -> LFE2 <$> ch_inside_if_needed lfe

instance ChangeIfNeeded BasicOrAppExpr where
  ch_inside_if_needed = \case
    BE3 be -> change_be_if_needed be
    PrFA1 prfa -> PrFA1 <$> ch_inside_if_needed prfa
    PoFA1 pofa -> PoFA1 <$> ch_inside_if_needed pofa

instance ChangeIfNeeded BasicExpr where
  ch_inside_if_needed = \case
    PFAOI1 pfaoi -> PFAOI1 <$> ch_inside_if_needed pfaoi
    T1 t -> T1 <$> ch_inside_if_needed t
    L1 l -> L1 <$> ch_inside_if_needed l
    other -> return other

instance ChangeIfNeeded BigTuple where
  ch_inside_if_needed = \(BT bt) -> BT <$> ch_inside_if_needed_triple bt

deriving instance ChangeIfNeeded List

instance ChangeIfNeeded BigList where
  ch_inside_if_needed = \(BL bl) -> BL <$> ch_inside_if_needed_pair bl

instance ChangeIfNeeded ArgsStr where
  ch_inside_if_needed = ch_inside_if_needed_first

instance ChangeIfNeeded ParenFuncAppOrId where
  ch_inside_if_needed pfaoi =
    case check_if_pfaoi_is_sid pfaoi of
      Just sid -> pfaoi_is_sid_case sid
      _ -> ch_inside_pfaoi_if_needed pfaoi
    where
    pfaoi_is_sid_case :: SimpleId -> DCS ParenFuncAppOrId
    pfaoi_is_sid_case =
      if_sid_in_ncs_to_new_pfaoi >=> \case
        Just new_pfaoi -> return new_pfaoi
        _ -> ch_inside_pfaoi_if_needed pfaoi

    ch_inside_pfaoi_if_needed :: ParenFuncAppOrId -> DCS ParenFuncAppOrId
    ch_inside_pfaoi_if_needed =
      \(PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
        ch_inside_if_needed_triple (margs1, args_str_pairs, margs2) >$>
          \(margs1', args_str_pairs', margs2') ->
          PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

instance ChangeIfNeeded Arguments where
  ch_inside_if_needed = \(As leous) -> As <$> ch_inside_if_needed leous

instance ChangeIfNeeded PreFuncApp where
  ch_inside_if_needed = \(PrFA prfa) ->
    PrFA <$> ch_inside_if_needed_second prfa

instance ChangeIfNeeded PostFuncApp where
  ch_inside_if_needed = \(PoFA (pfarg, pfae)) ->
    ch_inside_if_needed pfarg >>= \pfarg' ->
    push_post_func_arg pfarg >>
    ch_inside_if_needed pfae >>= \pfae' ->
    pop_post_func_arg >>
    return (PoFA (pfarg', pfae'))

instance ChangeIfNeeded PostFuncArg where
  ch_inside_if_needed = \case
    PE2 pe -> PE2 <$> ch_inside_if_needed pe
    BE2 be -> BE2 <$> ch_inside_if_needed be
    Underscore2 -> return Underscore2

instance ChangeIfNeeded PostFuncAppEnd where
  ch_inside_if_needed = \case
    DC1 dc -> DC1 <$> ch_inside_if_needed dc
    PFsMDC pfs_mdc -> PFsMDC <$> ch_inside_if_needed_second pfs_mdc

instance ChangeIfNeeded DotChange where
  ch_inside_if_needed = \(DC dc) -> DC <$> ch_inside_if_needed_pair dc

instance ChangeIfNeeded FieldChange where
  ch_inside_if_needed = \(FC fc) -> FC <$> ch_inside_if_needed_second fc

instance ChangeIfNeeded OpExpr where
  ch_inside_if_needed = \case
    LOE3 loe -> LOE3 <$> ch_inside_if_needed loe
    BOE1 boe -> BOE1 <$> ch_inside_if_needed boe

instance ChangeIfNeeded OpExprStart where
  ch_inside_if_needed = \(OES oper_op_list) ->
    OES <$> ch_inside_if_needed oper_op_list

instance ChangeIfNeeded (Operand, Op) where
  ch_inside_if_needed = ch_inside_if_needed_first

instance ChangeIfNeeded LineOpExpr where
  ch_inside_if_needed = \(LOE loe) -> LOE <$> ch_inside_if_needed_pair loe

instance ChangeIfNeeded LineOpExprEnd where
  ch_inside_if_needed = \case
    O1 o -> O1 <$> ch_inside_if_needed o
    LFE3 lfe -> LFE3 <$> ch_inside_if_needed lfe

instance ChangeIfNeeded BigOpExpr where
  ch_inside_if_needed = \case
    BOEOS1 boeos -> BOEOS1 <$> ch_inside_if_needed boeos
    BOEFS1 boefs -> BOEFS1 <$> ch_inside_if_needed boefs

instance ChangeIfNeeded BigOpExprOpSplit where
  ch_inside_if_needed = \(BOEOS boeos) ->
    BOEOS <$> ch_inside_if_needed_triple boeos

instance ChangeIfNeeded OpSplitLine where
  ch_inside_if_needed = \(OSL osl) -> OSL <$> ch_inside_if_needed_pair osl

instance ChangeIfNeeded OperFCO where
  ch_inside_if_needed = \(OFCO oper_fco) ->
    OFCO <$> ch_inside_if_needed_first oper_fco

instance ChangeIfNeeded OpSplitEnd where
  ch_inside_if_needed = \case
    O2 o -> O2 <$> ch_inside_if_needed o
    FE1 fe -> FE1 <$> ch_inside_if_needed fe

instance ChangeIfNeeded BigOpExprFuncSplit where
  ch_inside_if_needed = \(BOEFS boefs) ->
    BOEFS <$> ch_inside_if_needed_pair boefs

instance ChangeIfNeeded BigOrCasesFuncExpr where
  ch_inside_if_needed = \case
    BFE1 bfe -> BFE1 <$> ch_inside_if_needed bfe
    CFE1 cfe -> CFE1 <$> ch_inside_if_needed cfe

instance ChangeIfNeeded Operand where
  ch_inside_if_needed = \case
    BOAE2 boae -> BOAE2 <$> ch_inside_if_needed boae
    PE3 pe -> PE3 <$> ch_inside_if_needed pe
    Underscore3 -> return Underscore3

instance ChangeIfNeeded FuncExpr where
  ch_inside_if_needed = \case
    LFE4 lfe -> LFE4 <$> ch_inside_if_needed lfe
    BFE2 bfe -> BFE2 <$> ch_inside_if_needed bfe
    CFE2 cfe -> CFE2 <$> ch_inside_if_needed cfe

instance ChangeIfNeeded LineFuncExpr where
  ch_inside_if_needed = \(LFE lfe) -> LFE <$> ch_inside_if_needed_second lfe

instance ChangeIfNeeded BigFuncExpr where
  ch_inside_if_needed = \(BFE bfe) -> BFE <$> ch_inside_if_needed_second bfe

instance ChangeIfNeeded LineFuncBody where
  ch_inside_if_needed = \case
    BOAE3 boae -> BOAE3 <$> ch_inside_if_needed boae
    LOE4 loe -> LOE4 <$> ch_inside_if_needed loe

instance ChangeIfNeeded BigFuncBody where
  ch_inside_if_needed = \case
    BOAE4 boae -> BOAE4 <$> ch_inside_if_needed boae
    OE1 oe -> OE1 <$> ch_inside_if_needed oe

instance ChangeIfNeeded CasesFuncExpr where
  ch_inside_if_needed = \(CFE (cparams, cases, mec)) ->
    ch_inside_if_needed_pair (cases, mec) >$> \(cases', mec') ->
    CFE (cparams, cases', mec')

instance ChangeIfNeeded Case where
  ch_inside_if_needed = \(Ca om_cb) -> Ca <$> ch_inside_if_needed_pair om_cb

instance ChangeIfNeeded EndCase where
  ch_inside_if_needed = \(EC ecp_cb) ->
    EC <$> ch_inside_if_needed_second ecp_cb

instance ChangeIfNeeded OuterMatching where
  ch_inside_if_needed = \case
    SId3 sid -> SId3 <$> change_sid_if_needed sid
    other -> return other

instance ChangeIfNeeded CaseBody where
  ch_inside_if_needed = \case
    LFB1 lfb -> LFB1 <$> ch_inside_if_needed lfb
    BFB1 bfb -> BFB1 <$> ch_inside_if_needed_pair bfb

instance ChangeIfNeeded ValueDef where
  ch_inside_if_needed = \(VD (id, t, ve, mwe)) ->
    ch_inside_if_needed_pair (ve, mwe) >$> \(ve', mwe') ->
    VD (id, t, ve', mwe')

instance ChangeIfNeeded ValueExpr where
  ch_inside_if_needed = \case
    BOAE5 boae -> BOAE5 <$> ch_inside_if_needed boae
    OE2 oe -> OE2 <$> ch_inside_if_needed oe
    FE2 fe -> FE2 <$> ch_inside_if_needed fe
    BT1 bt -> BT1 <$> ch_inside_if_needed bt
    BL1 bl -> BL1 <$> ch_inside_if_needed bl

instance ChangeIfNeeded GroupedValueDefs where
  ch_inside_if_needed = \(GVDs (id, ids, ts, les, les_l)) ->
    ch_inside_if_needed_pair (les, les_l) >$> \(les', les_l') ->
    GVDs (id, ids, ts, les', les_l')

instance ChangeIfNeeded LineExprs where
  ch_inside_if_needed = \(LEs les) -> LEs <$> ch_inside_if_needed_pair les

instance ChangeIfNeeded WhereExpr where
  ch_inside_if_needed = \(WE we) -> WE <$> ch_inside_if_needed_pair we

instance ChangeIfNeeded WhereDefExpr where
  ch_inside_if_needed = \case
    VD1 vd -> VD1 <$> ch_inside_if_needed vd
    GVDs1 gvds -> GVDs1 <$> ch_inside_if_needed gvds

instance ChangeIfNeeded TypeTheo where
  ch_inside_if_needed = \(TT (pnws, mpnws, proof)) ->
    ch_inside_if_needed proof >$> \proof' -> TT (pnws, mpnws, proof')

instance ChangeIfNeeded Proof where
  ch_inside_if_needed = \case
    P1 iooe_le -> P1 <$> ch_inside_if_needed_second iooe_le
    P2 iooe_ttve -> P2 <$> ch_inside_if_needed_second iooe_ttve

instance ChangeIfNeeded TTValueExpr where
  ch_inside_if_needed = \case
    LE2 le -> LE2 <$> ch_inside_if_needed le
    VEMWE vemwe -> VEMWE <$> ch_inside_if_needed_pair vemwe

instance ChangeIfNeeded Program where
  ch_inside_if_needed = \(P p) -> P <$> ch_inside_if_needed_pair p

instance ChangeIfNeeded ProgramPart where
  ch_inside_if_needed = \case
    VD2 vd -> VD2 <$> ch_inside_if_needed vd
    GVDs2 gvds -> GVDs2 <$> ch_inside_if_needed gvds
    TT1 tt -> TT1 <$> ch_inside_if_needed tt
    other -> return other

-- testing

in_file :: FilePath
in_file =
  "/home/gnostis/Desktop/lambda-cases/new_parser/inputs/programs/" ++
  "extended_euclidean.lc"

test_parse :: String -> Program
test_parse = parse .> \case
  Left err -> error $ show err
  Right res -> res

test :: IO ()
test = readFile in_file >>= test_parse .> change_prog_if_needed .> print

-- ASTTypes.hs
-- Test.hs
