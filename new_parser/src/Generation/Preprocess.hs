{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Generation.Preprocess where

import Data.Set as S
import Control.Monad.State

import ASTTypes
import Helpers
import ShowInstances

import Parsing.AST
import Generation.Collect

-- types
data PossiblyInDC = InDotChange [PostFuncArg] | NotInDotChange

type DotChangeState = State (PossiblyInDC, FieldIds)
type DCS = DotChangeState
type PFAOI = ParenFuncAppOrId

-- class
class ChangeIfNeeded a where
  ch_inside_if_needed :: a -> DotChangeState a

-- change_prog_if_needed
change_prog_if_needed :: Program -> Program
change_prog_if_needed = \prog ->
  evalState (ch_inside_if_needed prog) (init_state prog)

-- State helpers
-- get, put
init_state :: Program -> (PossiblyInDC, FieldIds)
init_state = \prog -> (NotInDotChange, field_ids prog)

get_pidc :: DotChangeState PossiblyInDC
get_pidc = get >$> fst

get_fids :: DotChangeState FieldIds
get_fids = get >$> snd

put_new_pidc :: PossiblyInDC -> DotChangeState ()
put_new_pidc = \pidc -> modify (\(_, fids) -> (pidc, fids))

pfa_if_in_dot_change :: DotChangeState (Maybe PostFuncArg)
pfa_if_in_dot_change =
  get_pidc >$> \case
    NotInDotChange -> Nothing
    InDotChange [] -> error "should not be possible"
    InDotChange (pfa : pfas) -> Just pfa

-- push, pop
push_post_func_arg :: PostFuncArg -> DotChangeState ()
push_post_func_arg = \pfa ->
  get_pidc >>= \case
    NotInDotChange -> put_new_pidc $ InDotChange [pfa]
    InDotChange pfas -> put_new_pidc $ InDotChange $ pfa : pfas

pop_post_func_arg :: DotChangeState ()
pop_post_func_arg =
  get_pidc >>= \case
    InDotChange [pfa] -> put_new_pidc NotInDotChange
    InDotChange (pfa : pfas) -> put_new_pidc $ InDotChange pfas
    _ -> error "should not be possible"

-- check
check_if_sid_in_fids :: SimpleId -> DotChangeState Bool
check_if_sid_in_fids = \sid -> S.member sid <$> get_fids

pfa_if_sid_needs_change :: SimpleId -> DotChangeState (Maybe PostFuncArg)
pfa_if_sid_needs_change = \sid ->
  check_if_sid_in_fids sid >>= \in_fids ->
  pfa_if_in_dot_change >>= \mpfa ->
  return $ case (in_fids, mpfa) of
    (True, Just pfa) -> Just $ change_pfa_if_underscore pfa
    _ -> Nothing

change_pfa_if_underscore :: PostFuncArg -> PostFuncArg
change_pfa_if_underscore = \case
  Underscore2 -> x'_pfa
  other -> other

x'_pfa :: PostFuncArg
x'_pfa = BE2 $ PFAOI1 $ PFAOI (Nothing, IS "x'", [], Nothing, Nothing)

pf_pfa_if_pfaoi_needs_change :: PFAOI -> DCS (Maybe (PostFunc, PostFuncArg))
pf_pfa_if_pfaoi_needs_change = \case
  PFAOI (Nothing, ids, [], mdigit, Nothing) ->
    pfa_if_sid_needs_change sid >$> \case
      Nothing -> Nothing
      Just pfa -> Just (SId1 sid, pfa)
    where
    sid :: SimpleId
    sid = SId (ids, mdigit)
  _ -> return Nothing

pf_pfa_if_sp_id_needs_ch :: SpecialId -> DCS (Maybe (PostFunc, PostFuncArg))
pf_pfa_if_sp_id_needs_ch = \sp_id ->
  pfa_if_in_dot_change >$> \case
    Nothing -> Nothing
    Just pfa -> Just (SI2 sp_id, change_pfa_if_underscore pfa)

pf_pfa_if_be_needs_change :: BasicExpr -> DCS (Maybe (PostFunc, PostFuncArg))
pf_pfa_if_be_needs_change = \case
  PFAOI1 pfaoi -> pf_pfa_if_pfaoi_needs_change pfaoi
  SI1 sp_id -> pf_pfa_if_sp_id_needs_ch sp_id
  _ -> return Nothing

change_be_if_needed :: BasicExpr -> DotChangeState BasicOrAppExpr
change_be_if_needed = \be -> pf_pfa_if_be_needs_change be >>= \case
  Nothing -> BE3 <$> ch_inside_if_needed be
  Just (pf, pfa) -> return $ PoFA1 $ PoFA (pfa, PFsMDC ([pf], Nothing))

-- automatic instances
instance ChangeIfNeeded a => ChangeIfNeeded [a] where
  ch_inside_if_needed = mapM ch_inside_if_needed

instance ChangeIfNeeded a => ChangeIfNeeded (Maybe a) where
  ch_inside_if_needed = \case
    Nothing -> return Nothing
    Just a -> Just <$> ch_inside_if_needed a

-- regular instances
instance ChangeIfNeeded ParenExpr where
  ch_inside_if_needed = \(PE ipe) -> PE <$> ch_inside_if_needed ipe

instance ChangeIfNeeded InsideParenExpr where
  ch_inside_if_needed = \case
    LOE1 loe -> LOE1 <$> ch_inside_if_needed loe
    LFE1 lfe -> LFE1 <$> ch_inside_if_needed lfe

instance ChangeIfNeeded Tuple where
  ch_inside_if_needed = \(T (leou, leous)) ->
    T <$> ch_inside_if_needed leou ++< ch_inside_if_needed leous

instance ChangeIfNeeded LineExprOrUnders where
  ch_inside_if_needed = \(LEOUs (leou, leous)) ->
    LEOUs <$> ch_inside_if_needed leou ++< ch_inside_if_needed leous

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
  ch_inside_if_needed = \(BT (leou, leous, leous_l)) ->
    BT <$>
      ch_inside_if_needed leou ++<
      ch_inside_if_needed leous +++<
      ch_inside_if_needed leous_l

instance ChangeIfNeeded List where
  ch_inside_if_needed = \(L mleous) -> L <$> ch_inside_if_needed mleous

instance ChangeIfNeeded BigList where
  ch_inside_if_needed = \(BL (leou, leous)) ->
    BL <$> ch_inside_if_needed leou ++< ch_inside_if_needed leous

instance ChangeIfNeeded ArgsStr where
  ch_inside_if_needed = \(args, str) ->
    ch_inside_if_needed args >$> \args' -> (args', str)

instance ChangeIfNeeded ParenFuncAppOrId where
  ch_inside_if_needed =
    \(PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
    ch_inside_if_needed margs1 >>= \margs1' ->
    ch_inside_if_needed args_str_pairs >>= \args_str_pairs' ->
    ch_inside_if_needed margs2 >>= \margs2' ->
    return $ PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

instance ChangeIfNeeded Arguments where
  ch_inside_if_needed = \(As leous) -> As <$> ch_inside_if_needed leous

instance ChangeIfNeeded PreFuncApp where
  ch_inside_if_needed = \(PrFA (pf, oper)) ->
    ch_inside_if_needed oper >$> \oper' -> PrFA (pf, oper')

instance ChangeIfNeeded PostFuncApp where
  ch_inside_if_needed = \(PoFA (pfa, pfae)) ->
    ch_inside_if_needed pfa >>= \pfa' ->
    push_post_func_arg pfa >>
    ch_inside_if_needed pfae >>= \pfae' ->
    pop_post_func_arg >>
    return (PoFA (pfa', pfae'))

instance ChangeIfNeeded PostFuncArg where
  ch_inside_if_needed = \case
    PE2 pe -> PE2 <$> ch_inside_if_needed pe
    BE2 be -> BE2 <$> ch_inside_if_needed be
    Underscore2 -> return Underscore2

instance ChangeIfNeeded PostFuncAppEnd where
  ch_inside_if_needed = \case
    DC1 dc -> DC1 <$> ch_inside_if_needed dc
    PFsMDC (pfs, mdc) ->
      ch_inside_if_needed mdc >$> \mdc' -> PFsMDC (pfs, mdc')

instance ChangeIfNeeded DotChange where
  ch_inside_if_needed = \(DC (fc, fcs)) ->
    DC <$> ch_inside_if_needed fc ++< ch_inside_if_needed fcs

instance ChangeIfNeeded FieldChange where
  ch_inside_if_needed = \(FC (field, leou)) ->
    ch_inside_if_needed leou >$> \leou' -> FC (field, leou')

instance ChangeIfNeeded OpExpr where
  ch_inside_if_needed = \case
    LOE3 loe -> LOE3 <$> ch_inside_if_needed loe
    BOE1 boe -> BOE1 <$> ch_inside_if_needed boe

instance ChangeIfNeeded OpExprStart where
  ch_inside_if_needed = \(OES oper_op_list) ->
    OES <$> ch_inside_if_needed oper_op_list

instance ChangeIfNeeded (Operand, Op) where
  ch_inside_if_needed = \(oper, op) ->
    ch_inside_if_needed oper >$> \oper' -> (oper', op)

instance ChangeIfNeeded LineOpExpr where
  ch_inside_if_needed = \(LOE (oes, loee)) ->
    LOE <$> ch_inside_if_needed oes ++< ch_inside_if_needed loee

instance ChangeIfNeeded LineOpExprEnd where
  ch_inside_if_needed = \case
    O1 o -> O1 <$> ch_inside_if_needed o
    LFE3 lfe -> LFE3 <$> ch_inside_if_needed lfe

instance ChangeIfNeeded BigOpExpr where
  ch_inside_if_needed = \case
    BOEOS1 boeos -> BOEOS1 <$> ch_inside_if_needed boeos
    BOEFS1 boefs -> BOEFS1 <$> ch_inside_if_needed boefs

instance ChangeIfNeeded BigOpExprOpSplit where
  ch_inside_if_needed = \(BOEOS (osls, moes, ose)) ->
    BOEOS <$>
      ch_inside_if_needed osls ++<
      ch_inside_if_needed moes +++<
      ch_inside_if_needed ose

instance ChangeIfNeeded OpSplitLine where
  ch_inside_if_needed = \(OSL (oes, mofco)) ->
    OSL <$> ch_inside_if_needed oes ++< ch_inside_if_needed mofco

instance ChangeIfNeeded OperFCO where
  ch_inside_if_needed = \(OFCO (oper, fco)) ->
    ch_inside_if_needed oper >$> \oper' -> OFCO (oper, fco)

instance ChangeIfNeeded OpSplitEnd where
  ch_inside_if_needed = \case
    O2 o -> O2 <$> ch_inside_if_needed o
    FE1 fe -> FE1 <$> ch_inside_if_needed fe

instance ChangeIfNeeded BigOpExprFuncSplit where
  ch_inside_if_needed = \(BOEFS (oes, bocfe)) ->
    BOEFS <$> ch_inside_if_needed oes ++< ch_inside_if_needed bocfe

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
  ch_inside_if_needed = \(LFE (params, lfb)) ->
    ch_inside_if_needed lfb >$> \lfb' -> LFE (params, lfb')

instance ChangeIfNeeded BigFuncExpr where
  ch_inside_if_needed = \(BFE (params, bfb)) ->
    ch_inside_if_needed bfb >$> \bfb' -> BFE (params, bfb')

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
    ch_inside_if_needed cases >>= \cases' ->
    ch_inside_if_needed mec >>= \mec' ->
    return $ CFE (cparams, cases', mec')

instance ChangeIfNeeded Case where
  ch_inside_if_needed = \(Ca (om, cb)) ->
    ch_inside_if_needed cb >$> \cb' -> Ca (om, cb')

instance ChangeIfNeeded EndCase where
  ch_inside_if_needed = \(EC (ecp, cb)) ->
    ch_inside_if_needed cb >$> \cb' -> EC (ecp, cb')

instance ChangeIfNeeded CaseBody where
  ch_inside_if_needed = \case
    LFB1 lfb -> LFB1 <$> ch_inside_if_needed lfb
    BFB1 (bfb, mwe) ->
      BFB1 <$> ch_inside_if_needed bfb ++< ch_inside_if_needed mwe

instance ChangeIfNeeded ValueDef where
  ch_inside_if_needed = \(VD (id, t, ve, mwe)) ->
    ch_inside_if_needed ve >>= \ve' ->
    ch_inside_if_needed mwe >>= \mwe' ->
    return $ VD (id, t, ve', mwe')

instance ChangeIfNeeded ValueExpr where
  ch_inside_if_needed = \case
    BOAE5 boae -> BOAE5 <$> ch_inside_if_needed boae
    OE2 oe -> OE2 <$> ch_inside_if_needed oe
    FE2 fe -> FE2 <$> ch_inside_if_needed fe
    BT1 bt -> BT1 <$> ch_inside_if_needed bt
    BL1 bl -> BL1 <$> ch_inside_if_needed bl

instance ChangeIfNeeded GroupedValueDefs where
  ch_inside_if_needed = \(GVDs (id, ids, ts, les, les_l)) ->
    ch_inside_if_needed les >>= \les' ->
    ch_inside_if_needed les_l >>= \les_l' ->
    return $ GVDs (id, ids, ts, les', les_l')

instance ChangeIfNeeded LineExprs where
  ch_inside_if_needed = \(LEs (le, les)) ->
    LEs <$> ch_inside_if_needed le ++< ch_inside_if_needed les

instance ChangeIfNeeded WhereExpr where
  ch_inside_if_needed = \(WE (wde, wdes)) ->
    WE <$> ch_inside_if_needed wde ++< ch_inside_if_needed wdes

instance ChangeIfNeeded WhereDefExpr where
  ch_inside_if_needed = \case
    VD1 vd -> VD1 <$> ch_inside_if_needed vd
    GVDs1 gvds -> GVDs1 <$> ch_inside_if_needed gvds

instance ChangeIfNeeded TypeTheo where
  ch_inside_if_needed = \(TT (pnws, mpnws, proof)) ->
    ch_inside_if_needed proof >$> \proof' -> TT (pnws, mpnws, proof')

instance ChangeIfNeeded Proof where
  ch_inside_if_needed = \case
    P1 (iooe, le) -> ch_inside_if_needed le >$> \le' -> P1 (iooe, le)
    P2 (iooe, ttve) ->
      ch_inside_if_needed ttve >$> \ttve' -> P2 (iooe, ttve')

instance ChangeIfNeeded TTValueExpr where
  ch_inside_if_needed = \case
    LE2 le -> LE2 <$> ch_inside_if_needed le
    VEMWE (ve, mwe) ->
      VEMWE <$> ch_inside_if_needed ve ++< ch_inside_if_needed mwe

instance ChangeIfNeeded Program where
  ch_inside_if_needed = \(P (pp, pps)) ->
    P <$> ch_inside_if_needed pp ++< ch_inside_if_needed pps

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
