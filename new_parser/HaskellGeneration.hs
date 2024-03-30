{-# LANGUAGE LambdaCase, FlexibleInstances #-}

module HaskellGeneration where

import Control.Monad.State.Lazy

import Data.List
import Data.Char

import ASTTypes

-- types
type Haskell = String

type HaskellPair = (Haskell, Haskell)

type WithParamNum = State Int

-- classes
class ToHaskell a where
  to_haskell :: a -> Haskell

class ToHsWithParamNum a where
  to_hs_with_param_num :: a -> WithParamNum Haskell

-- params helpers
get_next_param :: WithParamNum Haskell
get_next_param = get $> (\i -> "x" ++ show i ++ "'") <* modify (+1)

params_hs_generator :: WithParamNum Haskell
params_hs_generator =
  get $> \case
    0 -> ""
    i -> "\\" ++ concatMap (\j -> "x" ++ show j ++ "' ") [0..i-1] ++ "-> "

add_params_to :: WithParamNum Haskell -> WithParamNum Haskell
add_params_to = \hs_generator ->
  hs_generator >>= \hs ->
  params_hs_generator >>= \params_hs ->
  return $ params_hs ++ hs 

run_generator :: WithParamNum Haskell -> Haskell
run_generator = \hs_generator -> evalState hs_generator 0

-- helper ops
($>) :: Functor f => f a -> (a -> b) -> f b
($>) = flip fmap

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

(&>) :: a -> (a -> b) -> b
(&>) = flip ($)

-- helpers
to_hs_prepend_list :: ToHaskell a => String -> [a] -> Haskell
to_hs_prepend_list = \sep -> concatMap ((sep ++) . to_haskell)

to_hs_prepend_comma_list :: ToHaskell a => [a] -> Haskell
to_hs_prepend_comma_list = to_hs_prepend_list ", "

to_hs_wpn_list :: ToHsWithParamNum a => [a] -> WithParamNum [Haskell]
to_hs_wpn_list = traverse to_hs_with_param_num

to_hs_wpn_pair
  :: (ToHsWithParamNum a, ToHsWithParamNum b) => (a, b) -> WithParamNum Haskell
to_hs_wpn_pair = \(a, b) ->
  to_hs_with_param_num a >>= \a_hs ->
  to_hs_with_param_num b >>= \b_hs ->
  return $ a_hs ++ b_hs

-- a => Maybe a instance, a b => (a, b) instance
instance ToHaskell a => ToHaskell (Maybe a) where 
  to_haskell = \case
    Nothing -> ""
    Just a -> to_haskell a

instance (ToHaskell a, ToHaskell b) => ToHaskell (a, b) where 
  to_haskell = \(a, b) -> to_haskell a ++ to_haskell b

instance ToHaskell a => ToHaskell [a] where 
  to_haskell = concatMap to_haskell

-- 

instance ToHsWithParamNum a => ToHsWithParamNum (Maybe a) where
  to_hs_with_param_num = \case
    Just a -> to_hs_with_param_num a 
    Nothing -> return ""

-- Values: Literal, Identifier, ParenExpr, Tuple, List, ParenFuncApp
instance ToHaskell Char where 
  to_haskell = show

instance ToHaskell Literal where 
  to_haskell = \case
    Int i -> show i
    R r -> show r
    Ch c -> show c
    S s -> show s

instance ToHaskell Identifier where
  to_haskell = \(Id (strs, maybe_digit)) ->
    intercalate "'" strs ++ to_haskell maybe_digit

instance ToHaskell SimpleId where
  to_haskell = \(SId str) -> str

instance ToHaskell ParenExpr where
  to_haskell = \(PE ipe) -> "(" ++ to_haskell ipe ++ ")"

instance ToHaskell InsideParenExpr where
  to_haskell = \case
    LOE1 soe -> to_haskell soe
    LFE1 lfe -> to_haskell lfe

instance ToHaskell Tuple where
  to_haskell (T (leou, leous)) =
    "(" ++ run_generator (add_params_to tuple_hs_generator) ++ ")"
    where
    tuple_hs_generator :: WithParamNum Haskell
    tuple_hs_generator =
      to_hs_with_param_num leou >>= \leou_hs ->
      to_hs_with_param_num leous >>= \leous_hs ->
      return $ "(" ++ leou_hs ++ ", " ++ leous_hs ++ ")"

instance ToHsWithParamNum LineExprOrUnders where
  to_hs_with_param_num = \(LEOUs (leou, leous)) ->
    to_hs_with_param_num leou >>= \leou_hs ->
    to_hs_wpn_list leous >>= \leous_hs ->
    return $ intercalate "," $ leou_hs : leous_hs

instance ToHsWithParamNum LineExprOrUnder where
  to_hs_with_param_num = \case
    LE1 le -> return $ to_haskell le
    Underscore1 -> get_next_param

instance ToHaskell LineExpr where
  to_haskell = \case
    BOAE1 boae -> to_haskell boae
    LOE2 soe -> to_haskell soe
    LFE2 lfe -> to_haskell lfe

instance ToHaskell BasicOrAppExpr where
  to_haskell = \case
    BE3 be -> to_haskell be
    PrFA1 prfa -> to_haskell prfa
    PoFA1 pofa -> to_haskell pofa

instance ToHaskell BasicExpr where
  to_haskell = \case
    Lit1 lit -> to_haskell lit
    Id1 id -> to_haskell id
    T1 tuple -> to_haskell tuple
    L1 list -> to_haskell list
    PFA pfa -> to_haskell pfa
    SI1 sid -> to_haskell sid

instance ToHaskell BigTuple where
  to_haskell (BT (leou, leous, leous_l)) =
    "(" ++ run_generator (add_params_to big_tuple_hs_generator) ++ ")"
    where
    big_tuple_hs_generator :: WithParamNum Haskell
    big_tuple_hs_generator =
      to_hs_with_param_num leou >>= \leou_hs ->
      to_hs_with_param_num leous >>= \leous_hs ->
      to_hs_wpn_list leous_l >>= \leous_hs_l ->
      return $
        "(" ++ leou_hs ++ ", " ++ intercalate "\n, " (leous_hs : leous_hs_l) ++
        ")"

instance ToHaskell List where
  to_haskell (L maybe_leous) =
    "(" ++ run_generator (add_params_to list_hs_generator) ++ ")"
    where
    list_hs_generator :: WithParamNum Haskell
    list_hs_generator =
      to_hs_with_param_num maybe_leous >>= \maybe_leous_hs ->
      return $ "[" ++ maybe_leous_hs ++ "]"

instance ToHaskell BigList where
  to_haskell (BL (leous, leous_l)) =
    "(" ++ run_generator (add_params_to big_list_hs_generator) ++ ")"
    where
    big_list_hs_generator :: WithParamNum Haskell
    big_list_hs_generator =
      to_hs_with_param_num leous >>= \leous_hs ->
      to_hs_wpn_list leous_l >>= \leous_hs_l ->
      return $ "[" ++ intercalate "\n, " (leous_hs : leous_hs_l) ++ "]"

instance ToHaskell ParenFuncApp where
  to_haskell pfa =
    "(" ++ run_generator (add_params_to paren_func_app_hs_generator) ++ ")"
    where
    paren_func_app_hs_generator :: WithParamNum Haskell
    paren_func_app_hs_generator = case pfa of
      IWA1 iwa_pfa -> to_hs_with_param_num iwa_pfa
      AI ai_pfa -> to_hs_with_param_num ai_pfa
      IA (id, args) -> (to_haskell id ++) <$> to_hs_with_param_num args

instance ToHsWithParamNum IWAParenFuncApp where
  to_hs_with_param_num = \(maybe_args1, id_with_args, maybe_args2) ->
    to_hs_with_param_num maybe_args1 >>= \maybe_args1_hs ->
    iwa_to_hs_with_param_num id_with_args >>= \(id_hs, args_hs) ->
    to_hs_with_param_num maybe_args2 >>= \maybe_args2_hs ->
    return $ id_hs ++ maybe_args1_hs ++ args_hs ++ maybe_args2_hs 

instance ToHsWithParamNum AIParenFuncApp where
  to_hs_with_param_num =  \(args, id, maybe_args) ->
    to_hs_with_param_num args >>= \args_hs ->
    to_hs_with_param_num maybe_args >>= \maybe_args_hs ->
    return $ to_haskell id ++ args_hs ++ maybe_args_hs

instance ToHsWithParamNum Arguments where
  to_hs_with_param_num = \(As (LEOUs (leou, leous))) ->
    to_hs_wpn_list (leou : leous) $> concatMap (" " ++)

iwa_to_hs_with_param_num :: IdentWithArgs -> WithParamNum HaskellPair
iwa_to_hs_with_param_num = 
    \(IWA (iwas, args, str, epoa_str_pairs, maybe_ch)) ->
    to_hs_with_param_num args >>= \args_hs1 ->
    esp_to_hs_wpn ("", "") epoa_str_pairs >>= \(id_rest_hs, args_hs2) ->
    let
    id_hs = to_haskell iwas ++ "'" ++ str ++ id_rest_hs ++ to_haskell maybe_ch
    args_hs = args_hs1 ++ args_hs2
    in
    return (id_hs, args_hs)
    where
    esp_to_hs_wpn :: HaskellPair -> [EpoaStr] -> WithParamNum HaskellPair
    esp_to_hs_wpn (id_rest_hs_prev, args_hs_prev) = \case
      [] -> return (id_rest_hs_prev, args_hs_prev)
      (epoa, str) : rest ->
        epoa_to_hs_wpn epoa >>= \epoa_hs ->
        let
        id_rest_hs_next = id_rest_hs_prev ++ "'" ++ str
        args_hs_next = args_hs_prev ++ epoa_hs
        in
        esp_to_hs_wpn (id_rest_hs_next, args_hs_next) rest

    epoa_to_hs_wpn :: EmptyParenOrArgs -> WithParamNum Haskell
    epoa_to_hs_wpn = \case
      EmptyParen -> return ""
      As1 args -> to_hs_with_param_num args


instance ToHaskell IdentWithArgsStart where
  to_haskell = \(IWAS strs) -> intercalate "'" strs

-- Values: PreFunc, PostFunc, BasicExpr, Change
instance ToHaskell PreFunc where
  to_haskell = \(PF id) -> "C" ++ to_haskell id

instance ToHaskell PreFuncApp where
  to_haskell = \(PrFA (pf, oper)) ->
    to_haskell pf ++
    " (" ++ run_generator (add_params_to $ to_hs_with_param_num oper) ++ ")"
    -- oper is probably gonna be ToHsWithParamNum

instance ToHaskell PostFunc where
  to_haskell = \case
    SId1 sid -> to_haskell sid
    SI2 sid -> to_haskell sid
    C1 c -> to_haskell c

instance ToHaskell SpecialId where
  to_haskell = \case
    First -> "first'"
    Second -> "second'"
    Third -> "third'"
    Fourth -> "fourth'"
    Fifth -> "fifth'"

instance ToHaskell PostFuncApp where
  to_haskell (PoFA (pfa, pfs)) =
    maybe_param_hs ++ pfs_pfa_to_haskell (reverse pfs) 
    where
    pfs_pfa_to_haskell :: [PostFunc] -> Haskell
    pfs_pfa_to_haskell = \case
      [] -> to_haskell pfa 
      [pf] -> to_haskell pf ++ " " ++ to_haskell pfa 
      pf:pfs -> to_haskell pf ++ " (" ++ pfs_pfa_to_haskell pfs ++ ")"
    
    maybe_param_hs :: Haskell
    maybe_param_hs = case pfa of
      Underscore2 -> "\\x' -> "
      _ -> ""

instance ToHaskell PostFuncArg where
  to_haskell = \case
    PE2 pe -> to_haskell pe
    BE2 be -> to_haskell be
    Underscore2 -> "x'"

instance ToHaskell Change where
  to_haskell (C (fc, fcs)) =
    "(\\y' -> " ++ run_generator (add_params_to change_hs_generator) ++ ")"
    where
    change_hs_generator :: WithParamNum Haskell
    change_hs_generator =
      to_hs_with_param_num fc >>= \fc_hs ->
      to_hs_wpn_list fcs >>= \fcs_hs ->
      return $ "y' {" ++ intercalate ", " (fc_hs : fcs_hs) ++ "}"

instance ToHsWithParamNum FieldChange where
  to_hs_with_param_num = \(FC (f, leou)) ->
    to_hs_with_param_num leou >>= \leou_hs ->
    return $ to_haskell f ++ " = " ++ leou_hs

instance ToHaskell Field where
  to_haskell = \case
    SId2 id -> to_haskell id
    SI3 sid -> to_haskell sid

-- Values: OpExpr
instance ToHaskell OpExpr where
  to_haskell = \case
    LOE3 soe -> to_haskell soe
    BOE1 boe -> to_haskell boe

instance ToHsWithParamNum OpExprStart where
  to_hs_with_param_num = \(OES oper_op_pairs) -> 
    to_hs_wpn_list oper_op_pairs $> concat

instance ToHsWithParamNum (Operand, Op) where
  to_hs_with_param_num = \(oper, op) ->
    (to_haskell op ++) <$> to_hs_with_param_num oper

instance ToHaskell LineOpExpr where
  to_haskell (LOE oes_loee) =
    "(" ++ run_generator (add_params_to $ to_hs_wpn_pair oes_loee) ++ ")"

instance ToHsWithParamNum LineOpExprEnd where
  to_hs_with_param_num = \case
    O1 o -> to_hs_with_param_num o
    LFE3 lfe -> return $ to_haskell lfe

instance ToHaskell BigOpExpr where
  to_haskell = \case
    BOEOS1 boeos -> to_haskell boeos
    BOEFS1 boefs -> to_haskell boefs

instance ToHaskell BigOpExprOpSplit where
  to_haskell (BOEOS (osls, maybe_oes, ose)) =
    "(" ++ run_generator (add_params_to boeos_hs_generator) ++ ")"
    where
    boeos_hs_generator :: WithParamNum Haskell
    boeos_hs_generator =
      to_hs_wpn_list osls >>= \osls_hs ->
      to_hs_with_param_num maybe_oes >>= \maybe_oes_hs ->
      to_hs_with_param_num ose >>= \ose_hs ->
      return $ concat osls_hs ++ maybe_oes_hs ++ ose_hs

instance ToHsWithParamNum OpSplitLine where
  to_hs_with_param_num = \(OSL oes_mofco) ->
    to_hs_wpn_pair oes_mofco $> (++ "\n")

instance ToHsWithParamNum OperFCO where
  to_hs_with_param_num = \(OFCO (oper, fco)) ->
    to_hs_with_param_num oper $> (++ to_haskell fco)

instance ToHsWithParamNum OpSplitEnd where
  to_hs_with_param_num = \case
    O2 o -> to_hs_with_param_num o
    FE1 fe -> return $ to_haskell fe

instance ToHaskell BigOpExprFuncSplit where
  to_haskell = \(BOEFS (oes, bocfe)) ->
    "(" ++ run_generator (add_params_to $ to_hs_with_param_num oes) ++
    to_haskell bocfe ++ ")"

instance ToHaskell BigOrCasesFuncExpr where
  to_haskell = \case
    BFE1 bfe -> to_haskell bfe
    CFE1 cfe -> to_haskell cfe
  
instance ToHsWithParamNum Operand where
  to_hs_with_param_num = \case
    BOAE2 boae -> return $ to_haskell boae
    PE3 pe -> return $ to_haskell pe
    Underscore3 -> get_next_param

instance ToHaskell Op where
  to_haskell = \case
    FCO3 co -> " " ++ to_haskell co ++ " "
    OSO oso -> " " ++ to_haskell oso ++ " "

instance ToHaskell FuncCompOp where
  to_haskell = \case
    RightComp -> ".>"
    LeftComp -> "<."

instance ToHaskell OptionalSpacesOp where
  to_haskell = \case
    RightApp -> "&>"
    LeftApp -> "<&"
    Power -> "!^" 
    Mult -> "!*" 
    Div -> "!/" 
    Plus -> "!+" 
    Minus -> "!-" 
    Equal -> "!==" 
    NotEqual -> "!!="
    Greater -> "!>" 
    Less -> "!<" 
    GrEq -> "!>="
    LeEq -> "!<="
    And -> "!&" 
    Or -> "!|" 
    Use -> "!>>="
    Then -> "!>>" 

-- Values: FuncExpr
instance ToHaskell FuncExpr where
  to_haskell = \case
    LFE4 lfe -> to_haskell lfe
    BFE2 bfe -> to_haskell bfe
    CFE2 cfe -> to_haskell cfe

instance ToHaskell LineFuncExpr where
  to_haskell = \(LFE (params, lfb)) ->
    "\\" ++ to_haskell params ++ " -> " ++ to_haskell lfb

instance ToHaskell BigFuncExpr where
  to_haskell = \(BFE (params, bfb)) ->
    "\\" ++ to_haskell params ++ " ->\n" ++ to_haskell bfb

instance ToHaskell Parameters where
  to_haskell = \case
    ParamId id -> to_haskell id
    Star1 -> "_"
    Params (params, params_l) ->
      "(" ++ to_haskell params ++ to_hs_prepend_comma_list params_l ++ ")"

instance ToHaskell LineFuncBody where
  to_haskell = \case
    BOAE3 boae -> to_haskell boae
    LOE4 soe -> to_haskell soe

instance ToHaskell BigFuncBody where
  to_haskell = \case
    BOAE4 boae -> to_haskell boae
    OE1 oe -> to_haskell oe

instance ToHaskell CasesFuncExpr where
  to_haskell (CFE (cps, cs, maybe_ec)) =
    run_generator cases_func_expr_hs_generator
    where
    cases_func_expr_hs_generator :: WithParamNum Haskell
    cases_func_expr_hs_generator =
      to_hs_with_param_num cps >>= \cps_hs ->
      case_of_hs_generator >>= \case_of_hs ->
      return $
        "\\" ++ cps_hs ++ " ->" ++ case_of_hs ++ to_haskell cs ++
        to_haskell maybe_ec

    case_of_hs_generator :: WithParamNum Haskell
    case_of_hs_generator =
      get $> \case
        0 -> ""
        i ->
          "case (" ++ map (\j -> "x" ++ show j ++ "'") [0..i-1] &>
          intercalate ", " ++ ") of"

instance ToHsWithParamNum CasesParams where
  to_hs_with_param_num = \case
    CParamId id -> return $ to_haskell id
    CasesKeyword -> get_next_param
    Star2 -> return "_"
    CParams (cps, cps_l) ->
      to_hs_with_param_num cps >>= \cps_hs ->
      to_hs_wpn_list cps_l >>= \cps_l_hs ->
      return $ "(" ++ intercalate ", " (cps_hs : cps_l_hs) ++ ")"

instance ToHaskell Case where
  to_haskell = \(Ca (m, cb)) -> "\n" ++ to_haskell m ++ " ->" ++ to_haskell cb

instance ToHaskell EndCase where
  to_haskell = \(EC (ecp, cb)) ->
    "\n" ++ to_haskell ecp ++ " ->" ++ to_haskell cb

instance ToHaskell EndCaseParam where
  to_haskell = \case
    IWP1 id_with_paren -> to_haskell id_with_paren
    Ellipsis -> "_"

instance ToHaskell Matching where
  to_haskell = \case
    Lit2 lit -> to_haskell lit
    SId3 id -> to_haskell id
    PFM (pf, im) -> to_haskell pf ++ " (" ++  to_haskell im ++ ")"
    TM1 tm -> to_haskell tm
    LM1 lm -> to_haskell lm

instance ToHaskell InnerMatching where
  to_haskell = \case
    M1 m -> to_haskell m
    IWP2 iwp -> to_haskell iwp
    Star -> "_"

instance ToHaskell TupleMatching where
  to_haskell = \(TM (im, im_l)) ->
    "(" ++ to_haskell im ++ to_hs_prepend_comma_list im_l ++ ")"

instance ToHaskell ListMatching where
  to_haskell (LM maybe_ims) =
    "[" ++ maybe_ims_hs ++ "]"
    where
    maybe_ims_hs :: Haskell
    maybe_ims_hs = case maybe_ims of
      Nothing -> ""
      Just (im, im_l) -> to_haskell im ++ to_hs_prepend_comma_list im_l

instance ToHaskell IdWithParen where
  to_haskell = \(IWP iwp) -> to_haskell (Id iwp)

instance ToHaskell CaseBody where
  to_haskell = \case
    LFB1 lfb -> " " ++  to_haskell lfb
    BFB1 (bfb, maybe_we) -> "\n" ++ to_haskell maybe_we ++ to_haskell bfb

-- Values: ValueDef, GroupedValueDefs, WhereExpr
instance ToHaskell ValueDef where
  to_haskell = \(VD (id, t, ve, maybe_we)) ->
    to_haskell id ++ " :: " ++ to_haskell t ++
    to_haskell id ++ " = " ++ to_haskell maybe_we ++ to_haskell ve

instance ToHaskell ValueExpr where
  to_haskell = \case
    BOAE5 boae -> to_haskell boae
    OE2 oe -> to_haskell oe
    FE2 fe -> to_haskell fe
    BT1 bt -> to_haskell bt
    BL1 bl -> to_haskell bl

instance ToHaskell GroupedValueDefs where
  to_haskell = \(GVDs (id, ids, ts, les, les_l)) ->
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
    map (id_t_le_to_value_def .> to_haskell) &> intercalate "\n\n"
    where
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

instance ToHaskell WhereExpr where
  to_haskell = \(WE (wde, wdes)) ->
    "\nlet\n" ++ to_haskell wde ++ to_hs_prepend_list "\n\n" wdes ++ "\nin\n"

instance ToHaskell WhereDefExpr where
  to_haskell = \case
    VD1 vd -> to_haskell vd
    GVDs1 gvd -> to_haskell gvd

-- Type
instance ToHaskell Type where
  to_haskell = \(Ty (maybe_c, st)) -> to_haskell maybe_c ++ to_haskell st

instance ToHaskell SimpleType where
  to_haskell = \case
    TIOV1 tiov -> to_haskell tiov
    TA1 ta -> to_haskell ta
    PoT1 pt -> to_haskell pt
    PT1 pt -> to_haskell pt
    FT1 ft -> to_haskell ft

instance ToHaskell TypeIdOrVar where
  to_haskell = \case
    TId1 tid -> to_haskell tid
    TV1 tv -> to_haskell tv

instance ToHaskell TypeId where
  to_haskell = \(TId str) -> str

instance ToHaskell TypeVar where
  to_haskell = \case
    PTV1 ptv -> to_haskell ptv
    AHTV1 ahtv -> to_haskell ahtv

instance ToHaskell ParamTVar where
  to_haskell = \(PTV i) -> "a" ++ show i

instance ToHaskell AdHocTVar where
  to_haskell = \(AHTV c) -> "b" ++ show (ord c - 97)

instance ToHaskell TypeApp where
  to_haskell = \case
    TIWA1 tiwa -> to_haskell tiwa
    TIPTI tipti -> to_haskell tipti
    TITIP (tid_or_tv, tip) -> to_haskell tid_or_tv ++ to_haskell tip

instance ToHaskell TIWATypeApp where
  to_haskell = \(maybe_tip1, tiwa, maybe_tip2) ->
    tiwa_to_hs tiwa &> \(tid_hs, tip_hs) ->
    tid_hs ++ to_haskell maybe_tip1 ++ tip_hs ++ to_haskell maybe_tip2
    where
    tiwa_to_hs :: TypeIdWithArgs -> HaskellPair
    tiwa_to_hs = \(TIWA (tid, tip_str_pairs)) ->
      tip_str_pairs_to_hs ("", "") tip_str_pairs &> \(tid_cont_hs, tip_hs) ->
      (to_haskell tid ++ tid_cont_hs, tip_hs)

    tip_str_pairs_to_hs :: HaskellPair -> [TIPSTR] -> HaskellPair
    tip_str_pairs_to_hs = \(tid_hs_prev, tip_hs_prev) -> \case
      [] -> (tid_hs_prev, tip_hs_prev)
      (tip, str):tip_strs ->
        let
        tid_hs_next = tid_hs_prev ++ "'" ++ str
        tip_hs_next = tip_hs_prev ++ to_haskell tip
        in
        tip_str_pairs_to_hs (tid_hs_next, tip_hs_next) tip_strs

instance ToHaskell TIPTITypeApp where
  to_haskell = \(tip, tioahtv, maybe_tip) ->
    to_haskell tioahtv ++ to_haskell tip ++ to_haskell maybe_tip

instance ToHaskell TIdOrAdHocTVar where
  to_haskell = \case
    TId2 tid -> to_haskell tid
    AHTV2 ahtv -> to_haskell ahtv

instance ToHaskell TypesInParen where
  to_haskell = \(TIP (st, sts)) -> to_hs_prepend_list " " (st : sts)

instance ToHaskell ProdType where
  to_haskell = \(PT (ft, fts)) ->
    "(" ++ ((ft : fts) &> map to_haskell &> intercalate ",") ++ ")"

instance ToHaskell FieldType where
  to_haskell = \case
    PBT1 ft -> to_haskell ft
    PoT3 pt -> to_haskell pt

instance ToHaskell PowerBaseType where
  to_haskell = \case
    TIOV3 tiov -> to_haskell tiov
    TA3 ta -> to_haskell ta
    IPT ipt -> to_haskell ipt

instance ToHaskell InParenT where
  to_haskell = \case
    PT3 pt -> "(" ++ to_haskell pt ++ ")"
    FT3 ft -> "(" ++ to_haskell ft ++ ")"  

instance ToHaskell PowerType where
  to_haskell = \(PoT (ft, i)) -> 
    "(" ++ (replicate i ft &> map to_haskell &> intercalate ",") ++ ")"

instance ToHaskell FuncType where
  to_haskell = \(FT (it, ot)) -> to_haskell it ++ " -> " ++ to_haskell ot

instance ToHaskell InOrOutType where
  to_haskell = \case
    TIOV2 tiov -> to_haskell tiov
    TA2 ta -> to_haskell ta
    PoT2 pt -> to_haskell pt
    PT2 pt -> to_haskell pt
    FT2 ft -> "(" ++ to_haskell ft ++ ")"

instance ToHaskell Condition where
  to_haskell = \(Co pn) -> to_haskell pn ++ " => "

-- TypeDef, TypeNickname
instance ToHaskell TypeDef where
  to_haskell = \case
    TTD1 ttd -> to_haskell ttd
    OTD1 otd -> to_haskell otd

instance ToHaskell TupleTypeDef where
  to_haskell (TTD (tn, PCSIs (si, sis), popt)) =
    "data " ++ tid_hs ++ " " ++ params_hs ++ " =\n  " ++
    tid_hs ++ "\n    {" ++ id_tuple_popt_hs ++ "\n    }"
    where
    (tid_hs, params_hs) = tn_to_hspair tn 
      :: HaskellPair

    id_tuple_popt_hs :: Haskell
    id_tuple_popt_hs = 
      zip (si : sis) popt_t_list &> map to_hs_id_st &> intercalate "\n    , "

    popt_t_list :: [SimpleType]
    popt_t_list = case popt of 
      PT4 (PT (ft, fts)) -> map ft_to_st $ ft : fts
      PoT4 (PoT (pbt, i)) -> replicate i $ pbt_to_st pbt

    to_hs_id_st :: (SimpleId, SimpleType) -> Haskell
    to_hs_id_st = \(sid, st) -> to_haskell sid ++ " :: " ++ to_haskell st

    ft_to_st :: FieldType -> SimpleType
    ft_to_st = \case
      PBT1 pbt -> pbt_to_st pbt
      PoT3 pot -> PoT1 pot

    pbt_to_st :: PowerBaseType -> SimpleType
    pbt_to_st = \case
      TIOV3 tiov -> TIOV1 tiov
      TA3 ta -> TA1 ta
      IPT ipt -> ipt_to_st ipt

    ipt_to_st :: InParenT -> SimpleType
    ipt_to_st = \case
      PT3 pt -> PT1 pt
      FT3 ft -> FT1 ft

tn_to_hspair :: TypeName -> HaskellPair
tn_to_hspair (TN (maybe_pvip1, tid, pvip_str_pairs, maybe_pvip2)) =
  ( to_haskell tid ++ tid_cont_hs
  , to_haskell maybe_pvip1 ++ pvip_hs2 ++ to_haskell maybe_pvip2
  )
  where
  (tid_cont_hs, pvip_hs2) =
    pvip_str_pairs_to_hs_pair ("", "") pvip_str_pairs
    :: HaskellPair

  pvip_str_pairs_to_hs_pair :: HaskellPair -> [PVIPStr] -> HaskellPair
  pvip_str_pairs_to_hs_pair (tid_cont_hs_prev, pvip_hs2_prev) = \case
    [] -> (tid_cont_hs_prev, pvip_hs2_prev)
    (pvip, str) : pvip_strs ->
      let 
      tid_cont_hs_next :: Haskell
      tid_cont_hs_next = tid_cont_hs_prev ++ "'" ++ str

      pvip_hs2_next :: Haskell
      pvip_hs2_next = pvip_hs2_prev ++ to_haskell pvip
      in
      pvip_str_pairs_to_hs_pair (tid_cont_hs_next, pvip_hs2_next) pvip_strs

instance ToHaskell ParamVarsInParen where
  to_haskell = \(PVIP (ptv, ptvs)) ->
    concatMap (to_haskell .> (" " ++)) (ptv : ptvs)

instance ToHaskell OrTypeDef where
  to_haskell (OTD (tn, id, mst, id_mst_pairs)) =
    "data " ++ tid_hs ++ " " ++ params_hs ++ " =\n  " ++
    (map id_mst_pair_to_hs ((id, mst) : id_mst_pairs) &> intercalate " |\n  ")
    where
    (tid_hs, params_hs) = tn_to_hspair tn 
      :: HaskellPair

    id_mst_pair_to_hs :: (SimpleId, Maybe SimpleType) -> Haskell
    id_mst_pair_to_hs = \(id, mst) ->
      "C" ++ to_haskell id ++ case mst of
        Nothing -> ""
        Just st -> " " ++ to_haskell mst

instance ToHaskell TypeNickname where
  to_haskell = \(TNN (tn, st)) ->
    "type_nickname " ++
    (tn_to_hspair tn &> \(tid_hs, params_hs) -> tid_hs ++ params_hs) ++
    " = " ++ to_haskell st

-- TypePropDef
instance ToHaskell TypePropDef where
  to_haskell = \case
    APD1 apd -> to_haskell apd
    RPD1 rpd -> to_haskell rpd

instance ToHaskell AtomPropDef where
  to_haskell = \(APD (PNL pn, id, st)) ->
    "class " ++ to_haskell pn ++ "where\n  " ++
    to_haskell id ++ " :: " ++ to_haskell st

-- instance ToHaskell RenamingPropDef where
--   to_haskell = \(RPD (pnl, pn, pns)) ->
--     to_haskell pnl ++ "\nequivalent\n  " ++ to_haskell pn ++
--     to_hs_prepend_comma_list pns

-- instance ToHaskell PropName where
--   to_haskell = \case
--     NPStart1 (c, np_ahvip_pairs, maybe_np) ->
--       [c] ++ to_haskell np_ahvip_pairs ++ to_haskell maybe_np
--     AHVIPStart1 (ahvip_np_pairs, maybe_ahvip) ->
--       to_haskell ahvip_np_pairs ++ to_haskell maybe_ahvip
-- 
-- instance ToHaskell AdHocVarsInParen where
--   to_haskell = \(AHVIP (ahtv, ahtvs)) ->
--     "(" ++ to_haskell ahtv ++ to_hs_prepend_comma_list ahtvs ++ ")"
-- 
-- instance ToHaskell NamePart where
--   to_haskell = \(NP str) -> str
-- 
-- -- TypeTheo 
-- instance ToHaskell TypeTheo where
--   to_haskell = \(TT (pnws, maybe_pnws, proof)) ->
--     "type_theorem " ++ to_haskell pnws ++ show_mpnws maybe_pnws ++
--     "\nproof" ++ to_haskell proof
--     where
--     show_mpnws :: Maybe PropNameWithSubs -> String
--     show_mpnws = \case
--       Nothing -> ""
--       Just pnws -> " => " ++ to_haskell pnws
-- 
-- instance ToHaskell PropNameWithSubs where
--   to_haskell = \case
--     NPStart2 (c, np_sip_pairs, maybe_np) ->
--       [c] ++ to_haskell np_sip_pairs ++ to_haskell maybe_np
--     SIPStart (sip_np_pairs, maybe_sip) ->
--       to_haskell sip_np_pairs ++ to_haskell maybe_sip
-- 
-- instance ToHaskell SubsInParen where
--   to_haskell = \(SIP (tvs, tvss)) ->
--     "(" ++ to_haskell tvs ++ to_hs_prepend_comma_list tvss ++ ")"
-- 
-- instance ToHaskell TVarSub where
--   to_haskell = \case
--     TIOV4 tiov -> to_haskell tiov
--     TAS1 tas -> to_haskell tas
--     PoTS1 pts -> to_haskell pts
--     PTS1 pts -> to_haskell pts
--     FTS1 fts -> to_haskell fts
-- 
-- instance ToHaskell TypeAppSub where
--   to_haskell = \case
--     TIWS1 (maybe_souip1, tiws, maybe_souip2) ->
--       to_haskell maybe_souip1 ++ to_haskell tiws ++
--       to_haskell maybe_souip2
--     SOUIP_TI (souip, tid_or_tv, maybe_souip) ->
--       to_haskell souip ++ to_haskell tid_or_tv ++ to_haskell maybe_souip
--     TI_SOUIP (tid_or_tv, souip) ->
--       to_haskell tid_or_tv ++ to_haskell souip
-- 
-- instance ToHaskell TypeIdWithSubs where
--   to_haskell = \(TIWS (tid, souip_str_pairs)) ->
--     to_haskell tid ++
--     concatMap (\(souip, str) -> to_haskell souip ++ str) souip_str_pairs
-- 
-- instance ToHaskell SubsOrUndersInParen where
--   to_haskell = \(SOUIP (sou, sous)) ->
--     "(" ++ to_haskell sou ++ to_hs_prepend_comma_list sous ++ ")"
-- 
-- instance ToHaskell SubOrUnder where
--   to_haskell = \case
--     TVS1 tvs -> to_haskell tvs
--     Underscore4 -> "_"
-- 
-- instance ToHaskell PowerTypeSub where
--   to_haskell = \(PoTS (pbts, i)) -> to_haskell pbts ++ "^" ++ show i
-- 
-- instance ToHaskell PowerBaseTypeSub where
--   to_haskell = \case
--     Underscore5 -> "_"
--     TIOV5 tid_or_var -> to_haskell tid_or_var
--     TAS2 tas -> to_haskell tas
--     IPTS1 ipts -> "(" ++ to_haskell ipts ++ ")"
-- 
-- instance ToHaskell InParenTSub where
--   to_haskell = \case
--     PTS2 pts -> to_haskell pts
--     FTS2 fts -> to_haskell fts
-- 
-- instance ToHaskell ProdTypeSub where
--   to_haskell = \(PTS (fts, fts_l)) ->
--     to_haskell fts ++ to_hs_prepend_list " x " fts_l
-- 
-- instance ToHaskell FieldTypeSub where
--   to_haskell = \case
--     PBTS1 pbts -> to_haskell pbts
--     PoTS2 pots -> to_haskell pots
-- 
-- instance ToHaskell FuncTypeSub where
--   to_haskell = \(FTS (ioots1, ioots2)) ->
--     to_haskell ioots1 ++ " => " ++ to_haskell ioots2 
-- 
-- instance ToHaskell InOrOutTypeSub where
--   to_haskell = \case
--     Underscore6 -> "_"
--     TIOV6 tiov -> to_haskell tiov
--     TAS3 tas -> to_haskell tas
--     PoTS3 pots -> to_haskell pots
--     PTS3 pts -> to_haskell pts
--     FTS3 fts -> to_haskell fts
-- 
-- instance ToHaskell Proof where
--   to_haskell = \case
--     P1 (iooe, le) -> " " ++ to_haskell iooe ++ " " ++ to_haskell le
--     P2 (iooe, ttve) -> "\n  " ++ to_haskell iooe ++ to_haskell ttve
-- 
-- instance ToHaskell IdOrOpEq where
--   to_haskell = \(IOOE (id, maybe_op_id)) ->
--     to_haskell id ++ show_moi maybe_op_id ++ " ="
--     where
--     show_moi :: Maybe (Op, Identifier) -> String
--     show_moi = \case
--       Nothing -> ""
--       Just (op, id) -> to_haskell op ++ to_haskell id
-- 
-- instance ToHaskell TTValueExpr where
--   to_haskell = \case
--     LE2 le -> " " ++ to_haskell le
--     VEMWE (ve, mwe) -> "\n    " ++ to_haskell ve ++ to_haskell mwe
-- 
-- -- Program
-- instance ToHaskell Program where
--   to_haskell = \(P (pp, pps)) -> to_haskell pp ++ to_hs_prepend_list "\n\n" pps
-- 
-- instance ToHaskell ProgramPart where
--   to_haskell = \case
--     VD2 vd -> to_haskell vd
--     GVDs2 gvds -> to_haskell gvds
--     TD td -> to_haskell td
--     TNN1 tnn -> to_haskell tnn
--     TPD tpd -> to_haskell tpd
--     TT1 tt -> to_haskell tt
-- 
-- -- For fast vim navigation
-- -- Parsers.hs
-- -- Testing.hs
-- -- ASTTypes.hs
