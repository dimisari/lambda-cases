{-# language LambdaCase #-}

module SyntaxTreeGen.ASTInstances where

import Prelude ((++), ($))
import Helpers ((.>), (&>))
import Prelude qualified as P
import ASTTypes qualified as T
import SyntaxTreeGen.TypesAndClasses qualified as STC
import SyntaxTreeGen.Helpers qualified as SH

import Control.Monad.State.Lazy qualified as CMSL

to_dot_final :: STC.ToDot a => a -> STC.Dot
to_dot_final =
  STC.to_dot .> \a_to_dot ->
  CMSL.evalState a_to_dot 0 &> \(root, dot) ->
  "graph " ++ root ++ "\n{\n" ++ dot ++  "\n}"

instance STC.ToDot T.Literal where
  to_dot = \case
    T.Int i -> SH.connect_node_with_new_root "Literal" $ P.show i
    T.R d -> SH.connect_node_with_new_root "Literal" $ P.show d
    T.Ch c -> SH.connect_node_with_new_root "Literal" $ P.show c
    T.S s -> SH.connect_node_with_new_root "Literal" s

instance STC.ToDot T.Identifier where
  to_dot = \(T.Id i) -> SH.add_new_root "Identifier" i

instance STC.ToDot T.SimpleId where
  to_dot = \(T.SId si) -> SH.add_new_root "SimpleId" si

instance STC.ToDot T.IdStart where
  to_dot = \(T.IS is) -> SH.add_new_root "IdStart" is

instance STC.ToDot T.IdCont where
  to_dot = \(T.IC ic) -> SH.add_new_root "IdCont" ic

instance STC.ToDot T.UndersInParen where
  to_dot = \(T.UIP uip) -> SH.add_new_root "UndersInParen" $ P.show uip

instance STC.ToDot T.ParenExpr where
  to_dot = \(T.PE pe) -> SH.add_new_root "ParenExpr" pe

instance STC.ToDot T.InsideParenExpr where
  to_dot = \case
    T.LOE1 loe -> SH.add_new_root "InsideParenExpr" loe
    T.LFE1 lfe -> SH.add_new_root "InsideParenExpr" lfe

instance STC.ToDot T.Tuple where
  to_dot = \(T.T t) -> SH.add_new_root "Tuple" t

instance STC.ToDot T.LineExprOrUnders where
  to_dot = \(T.LEOUs leous) -> SH.add_new_root "LineExprOrUnders" leous

instance STC.ToDot T.LineExprOrUnder where
  to_dot = \case
    T.LE1 le -> SH.add_new_root "LineExprOrUnder" le
    T.Underscore1 -> SH.connect_node_with_new_root "LineExprOrUnder" "_"

instance STC.ToDot T.LineExpr where
  to_dot = \case
    T.BOAE1 boae -> SH.add_new_root "LineExpr" boae
    T.LOE2 loe -> SH.add_new_root "LineExpr" loe
    T.LFE2 le -> SH.add_new_root "LineExpr" le

instance STC.ToDot T.BasicOrAppExpr where
  to_dot = \case
    T.BE3 be -> SH.add_new_root "BasicOrAppExpr" be
    T.PrFA1 pfa -> SH.add_new_root "BasicOrAppExpr" pfa
    T.PoFA1 pfa -> SH.add_new_root "BasicOrAppExpr" pfa

instance STC.ToDot T.BasicExpr where
  to_dot = \case
    T.Lit1 lit -> SH.add_new_root "BasicExpr" lit
    T.PFAOI1 pfaoi -> SH.add_new_root "BasicExpr" pfaoi
    T.T1 t -> SH.add_new_root "BasicExpr" t
    T.L1 l -> SH.add_new_root "BasicExpr" l
    T.SI1 si -> SH.add_new_root "BasicExpr" si

instance STC.ToDot T.BigTuple where
  to_dot = \(T.BT bt) -> SH.add_new_root "BigTuple" bt

instance STC.ToDot T.BigTupleSplit where
  to_dot = \case
    T.Split -> SH.connect_node_with_new_root "BigTupleSplit" "Split"
    T.NoSplit -> SH.connect_node_with_new_root "BigTupleSplit" "NoSplit"

instance STC.ToDot T.List where
  to_dot = \(T.L l) -> SH.add_new_root "List" l

instance STC.ToDot T.BigList where
  to_dot = \(T.BL bl) -> SH.add_new_root "BigList" bl

instance STC.ToDot T.ParenFuncAppOrId where
  to_dot = \(T.PFAOI pfaoi) -> SH.add_new_root "ParenFuncAppOrId" pfaoi

instance STC.ToDot T.Arguments where
  to_dot = \(T.As as) -> SH.add_new_root "Arguments" as

instance STC.ToDot T.PreFunc where
  to_dot = \(T.PF pf) -> SH.add_new_root "PreFunc" pf

instance STC.ToDot T.PreFuncApp where
  to_dot = \(T.PrFA prfa) -> SH.add_new_root "PreFuncApp" prfa

instance STC.ToDot T.DotId where
  to_dot = \(T.DI di) -> SH.add_new_root "DotId" di

instance STC.ToDot T.SimpleOrSpecialId where
  to_dot = \case
    T.SId1 si -> SH.add_new_root "SimpleOrSpecialId" si
    T.SI2 si -> SH.add_new_root "SimpleOrSpecialId" si

instance STC.ToDot T.SpecialId where
  to_dot = \case
    T.First -> SH.connect_node_with_new_root "SpecialId" "1st"
    T.Second -> SH.connect_node_with_new_root "SpecialId" "2nd"
    T.Third -> SH.connect_node_with_new_root "SpecialId" "3rd"
    T.Fourth -> SH.connect_node_with_new_root "SpecialId" "4th"
    T.Fifth -> SH.connect_node_with_new_root "SpecialId" "5th"

instance STC.ToDot T.PostFuncApp where
  to_dot = \case
    T.DIA1 dia -> SH.add_new_root "PostFuncApp" dia
    T.DCA1 dca -> SH.add_new_root "PostFuncApp" dca

instance STC.ToDot T.DotIdsApp where
  to_dot = \(T.DIA dia) -> SH.add_new_root "DotIdsApp" dia

instance STC.ToDot T.PostFuncArg where
  to_dot = \case
    T.BE2 be -> SH.add_new_root "PostFuncArg" be
    T.PE2 pe -> SH.add_new_root "PostFuncArg" pe
    T.Underscore2 -> SH.connect_node_with_new_root "PostFuncArg" "_"

instance STC.ToDot T.DotChangeApp where
  to_dot = \(T.DCA dca) -> SH.add_new_root "DotChangeApp" dca

instance STC.ToDot T.DotChangeArg where
  to_dot = \case
    T.PFA pfa -> SH.add_new_root "DotChangeArg" pfa
    T.DIA2 dia -> SH.add_new_root "DotChangeArg" dia

instance STC.ToDot T.DotChange where
  to_dot = \(T.DC dc) -> SH.add_new_root "DotChange" dc

instance STC.ToDot T.FieldChange where
  to_dot = \(T.FC fc) -> SH.add_new_root "FieldChange" fc

instance STC.ToDot T.OpExpr where
  to_dot = \case
    T.LOE3 loe -> SH.add_new_root "OpExpr" loe
    T.BOE1 boe -> SH.add_new_root "OpExpr" boe

instance STC.ToDot T.OpExprStart where
  to_dot = \(T.OES oes) -> SH.add_new_root "OpExprStart" oes

instance STC.ToDot T.LineOpExpr where
  to_dot = \(T.LOE loe) -> SH.add_new_root "LineOpExpr" loe

instance STC.ToDot T.LineOpExprEnd where
  to_dot = \case
    T.O1 op -> SH.add_new_root "LineOpExprEnd" op
    T.LFE3 loe -> SH.add_new_root "LineOpExprEnd" loe

instance STC.ToDot T.BigOpExpr where
  to_dot = \case
    T.BOEOS1 boeos -> SH.add_new_root "BigOpExpr" boeos
    T.BOEFS1 boefs -> SH.add_new_root "BigOpExpr" boefs

instance STC.ToDot T.BigOpExprOpSplit where
  to_dot = \(T.BOEOS boeos) -> SH.add_new_root "BigOpExprOpSplit" boeos

instance STC.ToDot T.OpSplitLine where
  to_dot = \case
    T.OESMOFCO oesmofco -> SH.add_new_root "OpSplitLine" oesmofco
    T.OFCO1 ofco -> SH.add_new_root "OpSplitLine" ofco

instance STC.ToDot T.OperFCO where
  to_dot = \(T.OFCO ofco) -> SH.add_new_root "OperFCO" ofco

instance STC.ToDot T.OpSplitEnd where
  to_dot = \case
    T.O2 op -> SH.add_new_root "OpSplitEnd" op
    T.FE1 fe -> SH.add_new_root "OpSplitEnd" fe

instance STC.ToDot T.BigOpExprFuncSplit where
  to_dot = \(T.BOEFS boefs) -> SH.add_new_root "BigOpExprFuncSplit" boefs

instance STC.ToDot T.BigOrCasesFuncExpr where
  to_dot = \case
    T.BFE1 bfe -> SH.add_new_root "BigOrCasesFuncExpr" bfe
    T.CFE1 cfe -> SH.add_new_root "BigOrCasesFuncExpr" cfe

instance STC.ToDot T.Operand where
  to_dot = \case
    T.BOAE2 boae -> SH.add_new_root "Operand" boae
    T.PE3 pe -> SH.add_new_root "Operand" pe
    T.Underscore3 -> SH.connect_node_with_new_root "Operand" "_"

instance STC.ToDot T.Op where
  to_dot = \case
    T.FCO3 fco -> SH.add_new_root "Op" fco
    T.OSO oso -> SH.add_new_root "Op" oso

instance STC.ToDot T.FuncCompOp where
  to_dot = \case
    T.RightComp -> SH.connect_node_with_new_root "FuncCompOp" "o>"
    T.LeftComp -> SH.connect_node_with_new_root "FuncCompOp" "<o"

instance STC.ToDot T.OptionalSpacesOp where
  to_dot = \case
    T.RightApp -> SH.connect_node_with_new_root "OptionalSpacesOp" "->"
    T.LeftApp -> SH.connect_node_with_new_root "OptionalSpacesOp" "<-"
    T.Power -> SH.connect_node_with_new_root "OptionalSpacesOp" "^"
    T.Mult -> SH.connect_node_with_new_root "OptionalSpacesOp" "*"
    T.Div -> SH.connect_node_with_new_root "OptionalSpacesOp" "/"
    T.Plus -> SH.connect_node_with_new_root "OptionalSpacesOp" "+"
    T.Minus -> SH.connect_node_with_new_root "OptionalSpacesOp" "-"
    T.Equal -> SH.connect_node_with_new_root "OptionalSpacesOp" "=="
    T.NotEqual -> SH.connect_node_with_new_root "OptionalSpacesOp" "!="
    T.Greater -> SH.connect_node_with_new_root "OptionalSpacesOp" ">"
    T.Less -> SH.connect_node_with_new_root "OptionalSpacesOp" "<"
    T.GrEq -> SH.connect_node_with_new_root "OptionalSpacesOp" ">="
    T.LeEq -> SH.connect_node_with_new_root "OptionalSpacesOp" "<="
    T.And -> SH.connect_node_with_new_root "OptionalSpacesOp" "&"
    T.Or -> SH.connect_node_with_new_root "OptionalSpacesOp" "|"
    T.Use -> SH.connect_node_with_new_root "OptionalSpacesOp" ">>"
    T.Then -> SH.connect_node_with_new_root "OptionalSpacesOp" ";"

instance STC.ToDot T.FuncExpr where
  to_dot = \case
    T.LFE4 lfe -> SH.add_new_root "FuncExpr" lfe
    T.BFE2 bfe -> SH.add_new_root "FuncExpr" bfe
    T.CFE2 cfe -> SH.add_new_root "FuncExpr" cfe

instance STC.ToDot T.LineFuncExpr where
  to_dot = \(T.LFE lfe) -> SH.add_new_root "LineFuncExpr" lfe

instance STC.ToDot T.BigFuncExpr where
  to_dot = \(T.BFE bfe) -> SH.add_new_root "BigFuncExpr" bfe

instance STC.ToDot T.BigFuncBodyOrDeeperBody where
  to_dot = \case
    T.BFB bfb -> SH.add_new_root "BigFuncBodyOrDeeperBody" bfb
    T.DB db -> SH.add_new_root "BigFuncBodyOrDeeperBody" db

instance STC.ToDot T.LineFuncBody where
  to_dot = \case
    T.BOAE3 boae -> SH.add_new_root "LineFuncBody" boae
    T.LOE4 loe -> SH.add_new_root "LineFuncBody" loe
    T.PLFE1 plfe -> SH.add_new_root "LineFuncBody" plfe

instance STC.ToDot T.ParenLineFuncExpr where
  to_dot = \(T.PLFE plfe) -> SH.add_new_root "ParenLineFuncExpr" plfe

instance STC.ToDot T.BigFuncBody where
  to_dot = \case
    T.BOAE4 boae -> SH.add_new_root "BigFuncBody" boae
    T.OE1 oe -> SH.add_new_root "BigFuncBody" oe
    T.PLFE2 plfe -> SH.add_new_root "BigFuncBody" plfe

instance STC.ToDot T.CasesFuncExpr where
  to_dot = \(T.CFE cfe) -> SH.add_new_root "CasesFuncExpr" cfe

instance STC.ToDot T.CasesParams where
  to_dot = \case
    T.CParamId id -> SH.add_new_root "CasesParams" id
    T.QuestionMark -> SH.connect_node_with_new_root "CasesParams" "?"
    T.Star2 -> SH.connect_node_with_new_root "CasesParams" "*"
    T.CParams cps -> SH.add_new_root "CasesParams" cps

instance STC.ToDot T.Case where
  to_dot = \(T.Ca ca) -> SH.add_new_root "Case" ca

instance STC.ToDot T.EndCase where
  to_dot = \(T.EC ec) -> SH.add_new_root "EndCase" ec

instance STC.ToDot T.OuterMatching where
  to_dot = \case
    T.SId2 sid -> SH.add_new_root "OuterMatching" sid
    T.M1 m -> SH.add_new_root "OuterMatching" m

instance STC.ToDot T.EndCaseParam where
  to_dot = \case
    T.Id2 id -> SH.add_new_root "EndCaseParam" id
    T.Ellipsis -> SH.connect_node_with_new_root "EndCaseParam" "..."

instance STC.ToDot T.Matching where
  to_dot = \case
    T.Lit2 lit -> SH.add_new_root "Matching" lit
    T.PFM pfm -> SH.add_new_root "Matching" pfm
    T.TM1 tm -> SH.add_new_root "Matching" tm
    T.LM1 lm -> SH.add_new_root "Matching" lm

instance STC.ToDot T.InnerMatching where
  to_dot = \case
    T.Star -> SH.connect_node_with_new_root "TypeId" "*"
    T.Id3 id -> SH.add_new_root "InnerMatching" id
    T.M2 m -> SH.add_new_root "InnerMatching" m

instance STC.ToDot T.TupleMatching where
  to_dot = \(T.TM tm) -> SH.add_new_root "TupleMatching" tm

instance STC.ToDot T.ListMatching where
  to_dot = \(T.LM lm) -> SH.add_new_root "ListMatching" lm

instance STC.ToDot T.RestListMatching where
  to_dot = \(T.RLM rlm) -> SH.add_new_root "RestListMatching" rlm

instance STC.ToDot T.CaseBody where
  to_dot = \case
    T.LFB1 lfb -> SH.add_new_root "CaseBody" lfb
    T.BFB1 bfb -> SH.add_new_root "CaseBody" bfb

instance STC.ToDot T.ValueDef where
  to_dot = \(T.VD vd) -> SH.add_new_root "ValueDef" vd

instance STC.ToDot T.TypeMaybeValueEquals where
  to_dot = \(T.TMVE tmve) -> SH.add_new_root "TypeMaybeValueEquals" tmve

instance STC.ToDot T.ValueExprMaybeWhere where
  to_dot = \(T.VE ve) -> SH.add_new_root "ValueExprMaybeWhere" ve

instance STC.ToDot T.ValueExpr where
  to_dot = \case
    T.BOAE5 boae -> SH.add_new_root "ValueExpr" boae
    T.OE2 oe -> SH.add_new_root "ValueExpr" oe
    T.FE2 fe -> SH.add_new_root "ValueExpr" fe
    T.BT1 bt -> SH.add_new_root "ValueExpr" bt
    T.BL1 bl -> SH.add_new_root "ValueExpr" bl

instance STC.ToDot T.WhereExpr where
  to_dot = \(T.WE we) -> SH.add_new_root "WhereExpr" we

instance STC.ToDot T.ListValueDefs where
  to_dot = \(T.LVDs lvds) -> SH.add_new_root "ListValueDefs" lvds

instance STC.ToDot T.IdList where
  to_dot = \(T.IL il) -> SH.add_new_root "IdList" il

instance STC.ToDot T.TupleValueDefs where
  to_dot = \(T.TVDs tvds) -> SH.add_new_root "TupleValueDefs" tvds

instance STC.ToDot T.IdTuple where
  to_dot = \(T.IT it) -> SH.add_new_root "IdTuple" it

instance STC.ToDot T.ValueDefs where
  to_dot = \case
    T.VD1 vd -> SH.add_new_root "ValueDefs" vd
    T.LVDs1 lvd -> SH.add_new_root "ValueDefs" lvd
    T.TVDs1 tvd -> SH.add_new_root "ValueDefs" tvd

instance STC.ToDot T.Type where
  to_dot = \(T.Ty ty) -> SH.add_new_root "Type" ty

instance STC.ToDot T.SimpleType where
  to_dot = \case
    T.TAIOT1 taiot -> SH.add_new_root "SimpleType" taiot
    T.POPT1 popt -> SH.add_new_root "SimpleType" popt
    T.FT1 ft -> SH.add_new_root "SimpleType" ft

instance STC.ToDot T.ProdOrPowerType where
  to_dot = \case
    T.PT4 pt -> SH.add_new_root "ProdOrPowerType" pt
    T.PoT5 pt -> SH.add_new_root "ProdOrPowerType" pt

instance STC.ToDot T.TypeId where
  to_dot = \(T.TId tid) -> SH.connect_node_with_new_root "TypeId" tid

instance STC.ToDot T.ParamTVar where
  to_dot = \(T.PTV ptv) -> SH.connect_node_with_new_root "ParamTVar" $ P.show ptv

instance STC.ToDot T.AdHocTVar where
  to_dot = \(T.AHTV ahtv) -> SH.connect_node_with_new_root "AdHocTVar" [ahtv]

instance STC.ToDot T.TypeAppIdOrTV where
  to_dot = \case
    T.TAIOA1 taioa -> SH.add_new_root "TypeAppIdOrTV" taioa
    T.PTV1 ptv -> SH.add_new_root "TypeAppIdOrTV" ptv

instance STC.ToDot T.TypeAppIdOrAHTV where
  to_dot = \(T.TAIOA taioa) -> SH.add_new_root "TypeAppIdOrAHTV" taioa

instance STC.ToDot T.TAIOAMiddle where
  to_dot = \case
    T.TIdStart1 tis -> SH.add_new_root "TAIOAMiddle" tis
    T.AHTV1 ahtv -> SH.add_new_root "TAIOAMiddle" ahtv

instance STC.ToDot T.TypesInParen where
  to_dot = \(T.TIP tip) -> SH.add_new_root "TypesInParen" tip

instance STC.ToDot T.ProdType where
  to_dot = \(T.PT pt) -> SH.add_new_root "ProdType" pt

instance STC.ToDot T.FieldType where
  to_dot = \case
    T.PBT1 pbt -> SH.add_new_root "FieldType" pbt
    T.PoT2 pt -> SH.add_new_root "FieldType" pt

instance STC.ToDot T.PowerBaseType where
  to_dot = \case
    T.TAIOT2 taoit -> SH.add_new_root "PowerBaseType" taoit
    T.IPT ipt -> SH.add_new_root "PowerBaseType" ipt

instance STC.ToDot T.InParenT where
  to_dot = \case
    T.PT3 pt -> SH.add_new_root "InParenT" pt
    T.FT3 ft -> SH.add_new_root "InParenT" ft
    T.PoT3 pt -> SH.add_new_root "InParenT" pt

instance STC.ToDot T.PowerType where
  to_dot = \(T.PoT pt) -> SH.add_new_root "PowerType" pt

instance STC.ToDot T.FuncType where
  to_dot = \(T.FT ft) -> SH.add_new_root "FuncType" ft

instance STC.ToDot T.InOrOutType where
  to_dot = \case
    T.TAIOT3 taiot -> SH.add_new_root "InOrOutType" taiot
    T.POPT2 popt -> SH.add_new_root "InOrOutType" popt
    T.FT2 ft -> SH.add_new_root "InOrOutType" ft

instance STC.ToDot T.Condition where
  to_dot = \(T.Co co) -> SH.add_new_root "Condition" co

instance STC.ToDot T.TypeDef where
  to_dot = \case
    T.TTD1 ttd -> SH.add_new_root "TypeDef" ttd
    T.OTD1 otd -> SH.add_new_root "TypeDef" otd

instance STC.ToDot T.TupleTypeDef where
  to_dot = \(T.TTD ttd) -> SH.add_new_root "TupleTypeDef" ttd

instance STC.ToDot T.TypeName where
  to_dot = \(T.TN tn) -> SH.add_new_root "TypeName" tn

instance STC.ToDot T.ParamVarsInParen where
  to_dot = \(T.PVIP pvip) -> SH.add_new_root "ParamVarsInParen" pvip

instance STC.ToDot T.FieldNames where
  to_dot = \(T.FN fn) -> SH.add_new_root "FieldNames" fn

instance STC.ToDot T.SimpleIds where
  to_dot = \(T.SIds sids) -> SH.add_new_root "SimpleIds" sids

instance STC.ToDot T.OrTypeDef where
  to_dot = \(T.OTD otd) -> SH.add_new_root "OrTypeDef" otd

instance STC.ToDot T.OrTypeValuesLine where
  to_dot = \(T.OTVL otvl) -> SH.add_new_root "OrTypeValueLine" otvl

instance STC.ToDot T.OrTypeValuesLines where
  to_dot = \(T.OTVLs otvls) -> SH.add_new_root "OrTypeValueLines" otvls

instance STC.ToDot T.OrTypeValue where
  to_dot = \(T.OTV otv) -> SH.add_new_root "OrTypeValue" otv

instance STC.ToDot T.InternalValue where
  to_dot = \(T.IV iv) -> SH.add_new_root "InternalValue" iv

instance STC.ToDot T.TypeNickname where
  to_dot = \(T.TNN tnn) -> SH.add_new_root "TypeNickname" tnn

instance STC.ToDot T.TypePropDef where
  to_dot = \case
    T.TSB1 tsb -> SH.add_new_root "TypePropDef" tsb
    T.RPD1 rpd -> SH.add_new_root "TypePropDef" rpd

instance STC.ToDot T.TypeSigBlock where
  to_dot = \(T.TSB tsb) -> SH.add_new_root "TypeSigBlock" tsb

instance STC.ToDot T.RenamingPropDef where
  to_dot = \(T.RPD rpd) -> SH.add_new_root "RenamingPropDef" rpd

instance STC.ToDot T.PropName where
  to_dot = \case
    T.NPStart1 nps -> SH.add_new_root "PropName" nps
    T.TIPStart tips -> SH.add_new_root "PropName" tips

instance STC.ToDot T.NamePart where
  to_dot = \(T.NP str) -> SH.connect_node_with_new_root "NamePart" str

instance STC.ToDot T.ImplementationBlock where
  to_dot = \(T.IB ib) -> SH.add_new_root "ImplementationBlock" ib

instance STC.ToDot T.PropNameWithSubs where
  to_dot = \case
    T.NPStart2 nps -> SH.add_new_root "PropNameWithSubs" nps
    T.SIPStart sips -> SH.add_new_root "PropNameWithSubs" sips

instance STC.ToDot T.SubsInParen where
  to_dot = \(T.SIP sip) -> SH.add_new_root "SubsInParen" sip

instance STC.ToDot T.TVarSub where
  to_dot = \case
    T.TAIOTS1 taiots -> SH.add_new_root "TVarSub" taiots
    T.POPTS1 popts -> SH.add_new_root "TVarSub" popts
    T.FTS1 fts -> SH.add_new_root "TVarSub" fts

instance STC.ToDot T.ProdOrPowerTypeSub where
  to_dot = \case
    T.PTS1 pts -> SH.add_new_root "ProdOrPowerTypeSub" pts
    T.PoTS1 pots -> SH.add_new_root "ProdOrPowerTypeSub" pots

instance STC.ToDot T.TypeAppIdOrTVSub where
  to_dot = \case
    T.TAIOAS1 taioas -> SH.add_new_root "TypeAppIdOrTVSub" taioas
    T.PTV2 ptv -> SH.add_new_root "TypeAppIdOrTVSub" ptv

instance STC.ToDot T.TypeAppIdOrAHTVSub where
  to_dot = \(T.TAIOAS taioas) -> SH.add_new_root "TypeAppIdOrAHTVSub" taioas

instance STC.ToDot T.TAIOASMiddle where
  to_dot = \case
    T.TIdStart2 tid_start_pair -> SH.add_new_root "TAIOASMiddle" tid_start_pair
    T.AHTV2 ahtv -> SH.add_new_root "TAIOASMiddle" ahtv

instance STC.ToDot T.SubsOrUndersInParen where
  to_dot = \(T.SOUIP souip) -> SH.add_new_root "SubsOrUndersInParen" souip

instance STC.ToDot T.SubOrUnder where
  to_dot = \case
    T.TVS1 tvs -> SH.add_new_root "SubOrUnder" tvs
    T.Underscore4 -> SH.connect_node_with_new_root "SubOrUnder" "_"

instance STC.ToDot T.PowerTypeSub where
  to_dot = \(T.PoTS pots_pair) -> SH.add_new_root "PowerTypeSub" pots_pair

instance STC.ToDot T.PowerBaseTypeSub where
  to_dot = \case
    T.Underscore5 -> SH.connect_node_with_new_root "PowerBaseTypeSub" "_"
    T.TAIOTS2 taiots -> SH.add_new_root "PowerBaseTypeSub" taiots
    T.IPTS ipts -> SH.add_new_root "PowerBaseTypeSub" ipts

instance STC.ToDot T.InParenTSub where
  to_dot = \case
    T.PTS2 pts -> SH.add_new_root "InParenTSub" pts
    T.FTS2 fts -> SH.add_new_root "InParenTSub" fts

instance STC.ToDot T.ProdTypeSub where
  to_dot = \(T.PTS pts) -> SH.add_new_root "FuncTypeSub" pts

instance STC.ToDot T.FieldTypeSub where
  to_dot = \case
    T.PBTS1 pbts -> SH.add_new_root "FieldTypeSub" pbts
    T.PoTS2 pts -> SH.add_new_root "FieldTypeSub" pts

instance STC.ToDot T.FuncTypeSub where
  to_dot = \(T.FTS fts_pair) -> SH.add_new_root "FuncTypeSub" fts_pair

instance STC.ToDot T.InOrOutTypeSub where
  to_dot = \case
    T.Underscore6 -> SH.connect_node_with_new_root "InOrOutTypeSub" "_"
    T.TAIOTS3 taiots -> SH.add_new_root "InOrOutTypeSub" taiots
    T.POPTS2 popts -> SH.add_new_root "InOrOutTypeSub" popts
    T.FTS3 fts -> SH.add_new_root "InOrOutTypeSub" fts

instance STC.ToDot T.Implementation where
  to_dot = \(T.I imoi_veme) -> SH.add_new_root "Implementation" imoi_veme

instance STC.ToDot T.IdMaybeOpId where
  to_dot = \(T.IMOI imoi) -> SH.add_new_root "IdMaybeOpId" imoi

instance STC.ToDot T.Comment where
  to_dot = \(T.C c) ->
    SH.connect_node_with_new_root "Comment"  c

instance STC.ToDot T.Program where
  to_dot = \(T.P p) -> SH.add_new_root "Program" p

instance STC.ToDot T.ProgramPart where
  to_dot = \case
    T.VDD vds -> SH.add_new_root "ProgramPart" vds
    T.TD td -> SH.add_new_root "ProgramPart" td
    T.TNN1 tn -> SH.add_new_root "ProgramPart" tn
    T.TPD tpd -> SH.add_new_root "ProgramPart" tpd
    T.TT1 ib -> SH.add_new_root "ProgramPart" ib
    T.C1 c -> SH.add_new_root "ProgramPart" c

