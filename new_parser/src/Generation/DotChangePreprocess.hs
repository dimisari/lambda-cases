{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Generation.DotChangePreprocess where

import Data.Set as S
import Control.Monad.State

import ASTTypes
import Helpers

import Generation.FieldIds

-- types
data PossiblyInDC = InDotChange [PostFuncArg] | NotInDotChange

type DotChangeState = State (PossiblyInDC, FieldIds)

-- State helpers
get_pidc :: DotChangeState PossiblyInDC
get_pidc = get $> fst

get_fids :: DotChangeState FieldIds
get_fids = get $> snd

put_new_pidc :: PossiblyInDC -> DotChangeState ()
put_new_pidc = \pidc -> modify (\(_, fids) -> (pidc, fids))

push_post_func_arg :: PostFuncArg -> DotChangeState ()
push_post_func_arg = \pfa ->
  get_pidc >>= \case
    InDotChange pfas -> put_new_pidc $ InDotChange $ pfa : pfas
    NotInDotChange -> put_new_pidc $ InDotChange [pfa]

pop_post_func_arg :: DotChangeState ()
pop_post_func_arg =
  get_pidc >>= \case
    InDotChange [pfa] -> put_new_pidc NotInDotChange
    InDotChange (pfa : pfas) -> put_new_pidc $ InDotChange pfas
    _ -> error "should not be possible"

check_if_in_fids :: SimpleId -> DotChangeState Bool
check_if_in_fids = \sid -> S.member sid <$> get_fids

check_if_in_dot_change :: DotChangeState Bool
check_if_in_dot_change =
  get_pidc $> \case
    NotInDotChange -> False
    InDotChange _ -> True

check_if_needs_change :: SimpleId -> DotChangeState Bool
check_if_needs_change = \sid ->
  check_if_in_fids sid >>= \in_fids ->
  check_if_in_dot_change >>= \in_dot_change ->
  return $ in_fids && in_dot_change

-- class
class ChangeIfNeeded a where
  change_if_needed :: a -> DotChangeState a

-- automatic instances
instance ChangeIfNeeded a => ChangeIfNeeded [a] where
  change_if_needed = mapM change_if_needed

instance ChangeIfNeeded a => ChangeIfNeeded (Maybe a) where
  change_if_needed = \case
    Nothing -> return Nothing
    Just a -> Just <$> change_if_needed a

-- regular instances
instance ChangeIfNeeded ParenExpr where
  change_if_needed = \(PE ipe) -> PE <$> change_if_needed ipe

instance ChangeIfNeeded InsideParenExpr where
  change_if_needed = \case
    LOE1 loe -> LOE1 <$> change_if_needed loe
    LFE1 lfe -> LFE1 <$> change_if_needed lfe

instance ChangeIfNeeded Tuple where
  change_if_needed = \(T (leou, leous)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    return $ T (leou', leous')

instance ChangeIfNeeded LineExprOrUnders where
  change_if_needed = \(LEOUs (leou, leous)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    return $ LEOUs (leou', leous')

instance ChangeIfNeeded LineExprOrUnder where
  change_if_needed = \case
    LE1 le -> LE1 <$> change_if_needed le
    Underscore1 -> return Underscore1

instance ChangeIfNeeded LineExpr where
  change_if_needed = \case
    BOAE1 boae -> BOAE1 <$> change_if_needed boae
    LOE2 loe -> LOE2 <$> change_if_needed loe
    LFE2 lfe -> LFE2 <$> change_if_needed lfe

instance ChangeIfNeeded BasicOrAppExpr where
  change_if_needed = \case
    BE3 be -> BE3 <$> change_if_needed be
    PrFA1 prfa -> PrFA1 <$> change_if_needed prfa
    PoFA1 pofa -> PoFA1 <$> change_if_needed pofa

instance ChangeIfNeeded BasicExpr where
  change_if_needed = \case
    PFAOI1 pfaoi -> PFAOI1 <$> change_if_needed pfaoi
    T1 t -> T1 <$> change_if_needed t
    L1 l -> L1 <$> change_if_needed l
    other -> return other

instance ChangeIfNeeded BigTuple where
  change_if_needed = \(BT (leou, leous, leous_l)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    change_if_needed leous_l >>= \leous_l' ->
    return $ BT (leou', leous', leous_l')

instance ChangeIfNeeded List where
  change_if_needed = \(L mleous) -> L <$> change_if_needed mleous

instance ChangeIfNeeded BigList where
  change_if_needed = \(BL (leou, leous)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    return $ BL (leou', leous')

instance ChangeIfNeeded ArgsStr where
  change_if_needed = \(args, str) ->
    change_if_needed args $> \args' -> (args', str)

instance ChangeIfNeeded ParenFuncAppOrId where
  change_if_needed (PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) =
    change_if_needed margs1 >>= \margs1' ->
    change_if_needed args_str_pairs >>= \args_str_pairs' ->
    change_if_needed margs2 >>= \margs2' ->
    return $ PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')
    where
    msid :: Maybe SimpleId
    msid = case (margs1, args_str_pairs, margs2) of
      (Nothing, [], Nothing) -> Just $ SId (ids, mdigit)
      _ -> Nothing

    check :: DotChangeState Bool
    check = case msid of
      Nothing -> return False
      Just sid -> check_if_needs_change sid

instance ChangeIfNeeded Arguments where
  change_if_needed = \(As leous) -> As <$> change_if_needed leous

instance ChangeIfNeeded PreFuncApp where
  change_if_needed = \(PrFA (pf, oper)) ->
    change_if_needed oper $> \oper' -> PrFA (pf, oper')

instance ChangeIfNeeded PostFuncApp where
  change_if_needed = \(PoFA (pfa, pfae)) ->
    change_if_needed pfa >>= \pfa' ->
    push_post_func_arg pfa >>
    change_if_needed pfae >>= \pfae' ->
    pop_post_func_arg >>
    return (PoFA (pfa', pfae'))

instance ChangeIfNeeded PostFuncArg where
  change_if_needed = \case
    PE2 pe -> PE2 <$> change_if_needed pe
    BE2 be -> BE2 <$> change_if_needed be
    Underscore2 -> return Underscore2

instance ChangeIfNeeded PostFuncAppEnd where
  change_if_needed = \case
    DC1 dc -> DC1 <$> change_if_needed dc
    PFsMDC (pfs, mdc) -> change_if_needed mdc $> \mdc' -> PFsMDC (pfs, mdc')

instance ChangeIfNeeded DotChange where
  change_if_needed = \(DC (fc, fcs)) ->
    change_if_needed fc >>= \fc' ->
    change_if_needed fcs >>= \fcs' ->
    return $ DC (fc', fcs')

instance ChangeIfNeeded FieldChange where
  change_if_needed = \(FC (field, leou)) ->
    change_if_needed leou $> \leou' -> FC (field, leou')

instance ChangeIfNeeded OpExpr where
  change_if_needed = \case
    LOE3 loe -> LOE3 <$> change_if_needed loe
    BOE1 boe -> BOE1 <$> change_if_needed boe

instance ChangeIfNeeded OpExprStart where
  change_if_needed = \(OES oper_op_list) ->
    OES <$> change_if_needed oper_op_list

instance ChangeIfNeeded (Operand, Op) where
  change_if_needed = \(oper, op) ->
    change_if_needed oper $> \oper' -> (oper', op)

instance ChangeIfNeeded LineOpExpr where
  change_if_needed = \(LOE (oes, loee)) ->
    change_if_needed oes >>= \oes' ->
    change_if_needed loee >>= \loee' ->
    return $ LOE (oes', loee')

instance ChangeIfNeeded LineOpExprEnd where
  change_if_needed = \case
    O1 o -> O1 <$> change_if_needed o
    LFE3 lfe -> LFE3 <$> change_if_needed lfe

instance ChangeIfNeeded BigOpExpr where
  change_if_needed = \case
    BOEOS1 boeos -> BOEOS1 <$> change_if_needed boeos
    BOEFS1 boefs -> BOEFS1 <$> change_if_needed boefs

instance ChangeIfNeeded BigOpExprOpSplit where
  change_if_needed = \(BOEOS (osls, moes, ose)) ->
    change_if_needed osls >>= \osls' ->
    change_if_needed moes >>= \moes' ->
    change_if_needed ose >>= \ose' ->
    return $ BOEOS (osls', moes', ose')

instance ChangeIfNeeded OpSplitLine where
  change_if_needed = \(OSL (oes, mofco)) ->
    change_if_needed oes >>= \oes' ->
    change_if_needed mofco >>= \mofco' ->
    return $ OSL (oes', mofco')

instance ChangeIfNeeded OperFCO where
  change_if_needed = \(OFCO (oper, fco)) ->
    change_if_needed oper $> \oper' -> OFCO (oper, fco)

instance ChangeIfNeeded OpSplitEnd where
  change_if_needed = \case
    O2 o -> O2 <$> change_if_needed o
    FE1 fe -> FE1 <$> change_if_needed fe

instance ChangeIfNeeded BigOpExprFuncSplit where
  change_if_needed = \(BOEFS (oes, bocfe)) ->
    change_if_needed oes >>= \oes' ->
    change_if_needed bocfe >>= \bocfe' ->
    return $ BOEFS (oes', bocfe')

instance ChangeIfNeeded BigOrCasesFuncExpr where
  change_if_needed = \case
    BFE1 bfe -> BFE1 <$> change_if_needed bfe
    CFE1 cfe -> CFE1 <$> change_if_needed cfe

instance ChangeIfNeeded Operand where
  change_if_needed = \case
    BOAE2 boae -> BOAE2 <$> change_if_needed boae
    PE3 pe -> PE3 <$> change_if_needed pe
    Underscore3 -> return Underscore3

instance ChangeIfNeeded FuncExpr where
  change_if_needed = \case
    LFE4 lfe -> LFE4 <$> change_if_needed lfe
    BFE2 bfe -> BFE2 <$> change_if_needed bfe
    CFE2 cfe -> CFE2 <$> change_if_needed cfe

instance ChangeIfNeeded LineFuncExpr where
  change_if_needed = \(LFE (params, lfb)) ->
    change_if_needed lfb $> \lfb' -> LFE (params, lfb')

instance ChangeIfNeeded BigFuncExpr where
  change_if_needed = \(BFE (params, bfb)) ->
    change_if_needed bfb $> \bfb' -> BFE (params, bfb')

instance ChangeIfNeeded LineFuncBody where
  change_if_needed = \case
    BOAE3 boae -> BOAE3 <$> change_if_needed boae
    LOE4 loe -> LOE4 <$> change_if_needed loe

instance ChangeIfNeeded BigFuncBody where
  change_if_needed = \case
    BOAE4 boae -> BOAE4 <$> change_if_needed boae
    OE1 oe -> OE1 <$> change_if_needed oe

instance ChangeIfNeeded CasesFuncExpr where
  change_if_needed = \(CFE (cparams, cases, mec)) ->
    change_if_needed cases >>= \cases' ->
    change_if_needed mec >>= \mec' ->
    return $ CFE (cparams, cases', mec')

instance ChangeIfNeeded Case where
  change_if_needed = \(Ca (om, cb)) ->
    change_if_needed cb $> \cb' -> Ca (om, cb')

instance ChangeIfNeeded EndCase where
  change_if_needed = \(EC (ecp, cb)) ->
    change_if_needed cb $> \cb' -> EC (ecp, cb')

instance ChangeIfNeeded CaseBody where
  change_if_needed = \case
    LFB1 lfb -> LFB1 <$> change_if_needed lfb
    BFB1 (bfb, mwe) ->
      change_if_needed bfb >>= \bfb' ->
      change_if_needed mwe >>= \mwe' ->
      return $ BFB1 (bfb', mwe')

instance ChangeIfNeeded ValueDef where
  change_if_needed = \(VD (id, t, ve, mwe)) ->
    change_if_needed ve >>= \ve' ->
    change_if_needed mwe >>= \mwe' ->
    return $ VD (id, t, ve', mwe')

instance ChangeIfNeeded ValueExpr where
  change_if_needed = \case
    BOAE5 boae -> BOAE5 <$> change_if_needed boae
    OE2 oe -> OE2 <$> change_if_needed oe
    FE2 fe -> FE2 <$> change_if_needed fe
    BT1 bt -> BT1 <$> change_if_needed bt
    BL1 bl -> BL1 <$> change_if_needed bl

instance ChangeIfNeeded GroupedValueDefs where
  change_if_needed = \(GVDs (id, ids, ts, les, les_l)) ->
    change_if_needed les >>= \les' ->
    change_if_needed les_l >>= \les_l' ->
    return $ GVDs (id, ids, ts, les', les_l')

instance ChangeIfNeeded LineExprs where
  change_if_needed = \(LEs (le, les)) ->
    change_if_needed le >>= \le' ->
    change_if_needed les >>= \les' ->
    return $ LEs (le', les')

instance ChangeIfNeeded WhereExpr where
  change_if_needed = \(WE (wde, wdes)) ->
    change_if_needed wde >>= \wde' ->
    change_if_needed wdes >>= \wdes' ->
    return $ WE (wde', wdes')

instance ChangeIfNeeded WhereDefExpr where
  change_if_needed = \case
    VD1 vd -> VD1 <$> change_if_needed vd
    GVDs1 gvds -> GVDs1 <$> change_if_needed gvds

instance ChangeIfNeeded TypeTheo where
  change_if_needed = \(TT (pnws, mpnws, proof)) ->
    change_if_needed proof $> \proof' -> TT (pnws, mpnws, proof')

instance ChangeIfNeeded Proof where
  change_if_needed = \case
    P1 (iooe, le) -> change_if_needed le $> \le' -> P1 (iooe, le)
    P2 (iooe, ttve) -> change_if_needed ttve $> \ttve' -> P2 (iooe, ttve')

instance ChangeIfNeeded TTValueExpr where
  change_if_needed = \case
    LE2 le -> LE2 <$> change_if_needed le
    VEMWE (ve, mwe) ->
      change_if_needed ve >>= \ve' ->
      change_if_needed mwe >>= \mwe' ->
      return $ VEMWE (ve', mwe')

instance ChangeIfNeeded Program where
  change_if_needed = \(P (pp, pps)) ->
    change_if_needed pp >>= \pp' ->
    change_if_needed pps >>= \pps' ->
    return $ P (pp', pps')

instance ChangeIfNeeded ProgramPart where
  change_if_needed = \case
    VD2 vd -> VD2 <$> change_if_needed vd
    GVDs2 gvds -> GVDs2 <$> change_if_needed gvds
    TT1 tt -> TT1 <$> change_if_needed tt
    other -> return other

-- ASTTypes.hs
