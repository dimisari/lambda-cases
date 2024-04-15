{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Generation.DotChangePreprocess where

import Control.Monad.State

import ASTTypes
import Helpers

import Generation.TypesAndHelpers

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

instance ChangeIfNeeded ValueDef where
  change_if_needed = \(VD (id, t, ve, mwe)) ->
    change_if_needed ve >>= \ve' ->
    change_if_needed mwe >>= \mwe' ->
    return $ VD (id, t, ve', mwe')

instance ChangeIfNeeded GroupedValueDefs where
  change_if_needed = \(GVDs (id, ids, ts, les, les_l)) ->
    change_if_needed les >>= \les' ->
    change_if_needed les_l >>= \les_l' ->
    return $ GVDs (id, ids, ts, les', les_l')

instance ChangeIfNeeded TypeTheo where
  change_if_needed = \(TT (pnws, mpnws, proof)) ->
    change_if_needed proof $> \proof' -> TT (pnws, mpnws, proof')

instance ChangeIfNeeded ValueExpr where
  change_if_needed = \case
    BOAE5 boae -> BOAE5 <$> change_if_needed boae
    OE2 oe -> OE2 <$> change_if_needed oe
    FE2 fe -> FE2 <$> change_if_needed fe
    other -> return other

instance ChangeIfNeeded WhereExpr where
  change_if_needed = \(WE (wde, wdes)) ->
    change_if_needed wde >>= \wde' ->
    change_if_needed wdes >>= \wdes' ->
    return $ WE (wde', wdes')

instance ChangeIfNeeded LineExprs where
  change_if_needed = \(LEs (le, les)) ->
    change_if_needed le >>= \le' ->
    change_if_needed les >>= \les' ->
    return $ LEs (le', les')

instance ChangeIfNeeded Proof where
  change_if_needed = \case
    P1 (iooe, le) -> change_if_needed le $> \le' -> P1 (iooe, le)
    P2 (iooe, ttve) -> change_if_needed ttve $> \ttve' -> P2 (iooe, ttve')

instance ChangeIfNeeded BasicOrAppExpr where
  change_if_needed = \case
    BE3 be -> BE3 <$> change_if_needed be
    PrFA1 prfa -> PrFA1 <$> change_if_needed prfa
    PoFA1 pofa -> PoFA1 <$> change_if_needed pofa

instance ChangeIfNeeded OpExpr where
  change_if_needed = \case
    LOE3 loe -> LOE3 <$> change_if_needed loe
    boe -> return boe

instance ChangeIfNeeded FuncExpr where
  change_if_needed = \case
    LFE4 lfe -> LFE4 <$> change_if_needed lfe
    other -> return other

instance ChangeIfNeeded WhereDefExpr where
  change_if_needed = \case
    VD1 vd -> VD1 <$> change_if_needed vd
    GVDs1 gvds -> GVDs1 <$> change_if_needed gvds

instance ChangeIfNeeded LineExpr where
  change_if_needed = \case
    BOAE1 boae -> BOAE1 <$> change_if_needed boae
    LOE2 loe -> LOE2 <$> change_if_needed loe
    LFE2 lfe -> LFE2 <$> change_if_needed lfe

instance ChangeIfNeeded TTValueExpr where
  change_if_needed = \case
    LE2 le -> LE2 <$> change_if_needed le
    VEMWE (ve, mwe) ->
      change_if_needed ve >>= \ve' ->
      change_if_needed mwe >>= \mwe' ->
      return $ VEMWE (ve', mwe')

instance ChangeIfNeeded BasicExpr where
  change_if_needed = \case
    PFAOI1 pfaoi -> PFAOI1 <$> change_if_needed pfaoi
    T1 t -> T1 <$> change_if_needed t
    L1 l -> L1 <$> change_if_needed l
    other -> return other

instance ChangeIfNeeded PreFuncApp where
  change_if_needed = \(PrFA (pf, oper)) ->
    change_if_needed oper $> \oper' -> PrFA (pf, oper')

instance ChangeIfNeeded PostFuncApp where
  change_if_needed = \(PoFA (pfa, pfae)) ->
    change_if_needed pfa >>= \pfa' ->
    change_if_needed pfae >>= \pfae' ->
    return $ PoFA (pfa', pfae')

instance ChangeIfNeeded LineOpExpr where
  change_if_needed = \(LOE (oes, loee)) ->
    change_if_needed oes >>= \oes' ->
    change_if_needed loee >>= \loee' ->
    return $ LOE (oes', loee')

instance ChangeIfNeeded LineFuncExpr where
  change_if_needed = \(LFE (params, lfb)) ->
    change_if_needed lfb $> \lfb' -> LFE (params, lfb')

instance ChangeIfNeeded ParenFuncAppOrId where
  change_if_needed = \(PFAOI (margs1, ids, args_str_pairs, mdigit, margs2)) ->
    change_if_needed margs1 >>= \margs1' ->
    change_if_needed args_str_pairs >>= \args_str_pairs' ->
    change_if_needed margs2 >>= \margs2' ->
    return $ PFAOI (margs1', ids, args_str_pairs', mdigit, margs2')

instance ChangeIfNeeded Tuple where
  change_if_needed = \(T (leou, leous)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    return $ T (leou', leous')

instance ChangeIfNeeded List where
  change_if_needed = \(L mleous) -> L <$> change_if_needed mleous

instance ChangeIfNeeded Operand where
  change_if_needed = \case
    BOAE2 boae -> BOAE2 <$> change_if_needed boae
    PE3 pe -> PE3 <$> change_if_needed pe
    Underscore3 -> return Underscore3

instance ChangeIfNeeded PostFuncArg where
  change_if_needed = \case
    PE2 pe -> PE2 <$> change_if_needed pe
    BE2 be -> BE2 <$> change_if_needed be
    Underscore2 -> return Underscore2

instance ChangeIfNeeded PostFuncAppEnd where
  change_if_needed = undefined

instance ChangeIfNeeded OpExprStart where
  change_if_needed = \(OES oper_op_list) -> OES <$> change_if_needed oper_op_list

instance ChangeIfNeeded LineOpExprEnd where
  change_if_needed = \case
    O1 o -> O1 <$> change_if_needed o
    LFE3 lfe -> LFE3 <$> change_if_needed lfe

instance ChangeIfNeeded LineFuncBody where
  change_if_needed = \case
    BOAE3 boae -> BOAE3 <$> change_if_needed boae
    LOE4 loe -> LOE4 <$> change_if_needed loe

instance ChangeIfNeeded Arguments where
  change_if_needed = \(As leous) -> As <$> change_if_needed leous

instance ChangeIfNeeded ArgsStr where
  change_if_needed = \(args, str) ->
    change_if_needed args $> \args' -> (args', str)

instance ChangeIfNeeded LineExprOrUnder where
  change_if_needed = \case
    LE1 le -> LE1 <$> change_if_needed le
    Underscore1 -> return Underscore1

instance ChangeIfNeeded LineExprOrUnders where
  change_if_needed = \(LEOUs (leou, leous)) ->
    change_if_needed leou >>= \leou' ->
    change_if_needed leous >>= \leous' ->
    return $ LEOUs (leou', leous')

instance ChangeIfNeeded ParenExpr where
  change_if_needed = \(PE ipe) -> PE <$> change_if_needed ipe

instance ChangeIfNeeded (Operand, Op) where
  change_if_needed = \(oper, op) ->
    change_if_needed oper $> \oper' -> (oper', op)

instance ChangeIfNeeded InsideParenExpr where
  change_if_needed = \case
    LOE1 loe -> LOE1 <$> change_if_needed loe
    LFE1 lfe -> LFE1 <$> change_if_needed lfe

