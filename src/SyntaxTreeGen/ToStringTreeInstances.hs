{-# language LambdaCase #-}

module SyntaxTreeGen.ToStringTreeInstances where

import Prelude (($))
import Prelude qualified as P

import ASTTypes qualified as T
import SyntaxTreeGen.TypesAndClasses qualified as STC
import SyntaxTreeGen.Helpers qualified as SH

instance STC.ToStringTree T.Literal where
  to_string_tree = \case
    T.Int i -> SH.add_new_root "Literal" i
    T.R d -> SH.add_new_root "Literal" d
    T.Ch c -> SH.add_new_root "Literal" c
    T.S s -> SH.root_and_node_to_tree "Literal" s

instance STC.ToStringTree T.Identifier where
  to_string_tree = \(T.Id i) -> SH.add_new_root "Identifier" i

instance STC.ToStringTree T.SimpleId where
  to_string_tree = \(T.SId si) -> SH.add_new_root "SimpleId" si

instance STC.ToStringTree T.IdStart where
  to_string_tree = \(T.IS is) -> SH.root_and_node_to_tree "IdStart" is

instance STC.ToStringTree T.IdCont where
  to_string_tree = \(T.IC ic) -> SH.add_new_root "IdCont" ic

instance STC.ToStringTree T.UndersInParen where
  to_string_tree = \(T.UIP uip) -> SH.add_new_root "UndersInParen" $ P.show uip

instance STC.ToStringTree T.ParenExpr where
  to_string_tree = \(T.PE pe) -> SH.add_new_root "ParenExpr" pe

instance STC.ToStringTree T.InsideParenExpr where
  to_string_tree = \case
    T.LOE1 loe -> SH.add_new_root "InsideParenExpr" loe
    T.LFE1 lfe -> SH.add_new_root "InsideParenExpr" lfe

instance STC.ToStringTree T.Tuple where
  to_string_tree = \(T.T t) -> SH.add_new_root "Tuple" t

instance STC.ToStringTree T.LineExprOrUnders where
  to_string_tree = \(T.LEOUs leous) -> SH.add_new_root "LineExprOrUnders" leous

instance STC.ToStringTree T.LineExprOrUnder where
  to_string_tree = \case
    T.LE1 le -> SH.add_new_root "LineExprOrUnder" le
    T.Underscore1 -> SH.root_and_node_to_tree "LineExprOrUnder" "_"

instance STC.ToStringTree T.LineExpr where
  to_string_tree = \case
    T.BOAE1 boae -> SH.add_new_root "LineExpr" boae
    T.LOE2 loe -> SH.add_new_root "LineExpr" loe
    T.LFE2 le -> SH.add_new_root "LineExpr" le

instance STC.ToStringTree T.BasicOrAppExpr where
  to_string_tree = \case
    T.BE3 be -> SH.add_new_root "BasicOrAppExpr" be
    T.PrFA1 pfa -> SH.add_new_root "BasicOrAppExpr" pfa
    T.PoFA1 pfa -> SH.add_new_root "BasicOrAppExpr" pfa

instance STC.ToStringTree T.BasicExpr where
  to_string_tree = \case
    T.Lit1 lit -> SH.add_new_root "BasicExpr" lit
    T.PFAOI1 pfaoi -> SH.add_new_root "BasicExpr" pfaoi
    T.T1 t -> SH.add_new_root "BasicExpr" t
    T.L1 l -> SH.add_new_root "BasicExpr" l
    T.SI1 si -> SH.add_new_root "BasicExpr" si

instance STC.ToStringTree T.BigTuple where
  to_string_tree = \(T.BT bt) -> SH.add_new_root "BigTuple" bt

instance STC.ToStringTree T.BigTupleSplit where
  to_string_tree = \case
    T.Split -> SH.root_and_node_to_tree "BigTupleSplit" "Split"
    T.NoSplit -> SH.root_and_node_to_tree "BigTupleSplit" "NoSplit"

instance STC.ToStringTree T.List where
  to_string_tree = \(T.L l) -> SH.add_new_root "List" l

instance STC.ToStringTree T.BigList where
  to_string_tree = \(T.BL bl) -> SH.add_new_root "BigList" bl

instance STC.ToStringTree T.ParenFuncAppOrId where
  to_string_tree = \(T.PFAOI pfaoi) -> SH.add_new_root "ParenFuncAppOrId" pfaoi

instance STC.ToStringTree T.Arguments where
  to_string_tree = \(T.As as) -> SH.add_new_root "Arguments" as

instance STC.ToStringTree T.PreFunc where
  to_string_tree = \(T.PF pf) -> SH.add_new_root "PreFunc" pf

instance STC.ToStringTree T.PreFuncApp where
  to_string_tree = \(T.PrFA prfa) -> SH.add_new_root "PreFuncApp" prfa

instance STC.ToStringTree T.DotId where
  to_string_tree = \(T.DI di) -> SH.add_new_root "DotId" di

instance STC.ToStringTree T.SimpleOrSpecialId where
  to_string_tree = \case
    T.SId1 si -> SH.add_new_root "SimpleOrSpecialId" si
    T.SI2 si -> SH.add_new_root "SimpleOrSpecialId" si

instance STC.ToStringTree T.SpecialId where
  to_string_tree = \case
    T.First -> SH.root_and_node_to_tree "SpecialId" "1st"
    T.Second -> SH.root_and_node_to_tree "SpecialId" "2nd"
    T.Third -> SH.root_and_node_to_tree "SpecialId" "3rd"
    T.Fourth -> SH.root_and_node_to_tree "SpecialId" "4th"
    T.Fifth -> SH.root_and_node_to_tree "SpecialId" "5th"

instance STC.ToStringTree T.PostFuncApp where
  to_string_tree = \case
    T.DIA1 dia -> SH.add_new_root "PostFuncApp" dia
    T.DCA1 dca -> SH.add_new_root "PostFuncApp" dca

instance STC.ToStringTree T.DotIdsApp where
  to_string_tree = \(T.DIA dia) -> SH.add_new_root "DotIdsApp" dia

instance STC.ToStringTree T.PostFuncArg where
  to_string_tree = \case
    T.BE2 be -> SH.add_new_root "PostFuncArg" be
    T.PE2 pe -> SH.add_new_root "PostFuncArg" pe
    T.Underscore2 -> SH.root_and_node_to_tree "PostFuncArg" "_"

instance STC.ToStringTree T.DotChangeApp where
  to_string_tree = \(T.DCA dca) -> SH.add_new_root "DotChangeApp" dca

instance STC.ToStringTree T.DotChangeArg where
  to_string_tree = \case
    T.PFA pfa -> SH.add_new_root "DotChangeArg" pfa
    T.DIA2 dia -> SH.add_new_root "DotChangeArg" dia

instance STC.ToStringTree T.DotChange where
  to_string_tree = \(T.DC dc) -> SH.add_new_root "DotChange" dc

instance STC.ToStringTree T.FieldChange where
  to_string_tree = \(T.FC fc) -> SH.add_new_root "FieldChange" fc

instance STC.ToStringTree T.OpExpr where
  to_string_tree = \case
    T.LOE3 loe -> SH.add_new_root "OpExpr" loe
    T.BOE1 boe -> SH.add_new_root "OpExpr" boe

instance STC.ToStringTree T.OpExprStart where
  to_string_tree = \(T.OES oes) -> SH.add_new_root "OpExprStart" oes

instance STC.ToStringTree T.LineOpExpr where
  to_string_tree = \(T.LOE loe) -> SH.add_new_root "LineOpExpr" loe

instance STC.ToStringTree T.LineOpExprEnd where
  to_string_tree = \case
    T.O1 op -> SH.add_new_root "LineOpExprEnd" op
    T.LFE3 loe -> SH.add_new_root "LineOpExprEnd" loe

instance STC.ToStringTree T.BigOpExpr where
  to_string_tree = \case
    T.BOEOS1 boeos -> SH.add_new_root "BigOpExpr" boeos
    T.BOEFS1 boefs -> SH.add_new_root "BigOpExpr" boefs

instance STC.ToStringTree T.BigOpExprOpSplit where
  to_string_tree = \(T.BOEOS boeos) -> SH.add_new_root "BigOpExprOpSplit" boeos

instance STC.ToStringTree T.OpSplitLine where
  to_string_tree = \case
    T.OESMOFCO oesmofco -> SH.add_new_root "OpSplitLine" oesmofco
    T.OFCO1 ofco -> SH.add_new_root "OpSplitLine" ofco

instance STC.ToStringTree T.OperFCO where
  to_string_tree = \(T.OFCO ofco) -> SH.add_new_root "OperFCO" ofco

instance STC.ToStringTree T.OpSplitEnd where
  to_string_tree = \case
    T.O2 op -> SH.add_new_root "OpSplitEnd" op
    T.FE1 fe -> SH.add_new_root "OpSplitEnd" fe

instance STC.ToStringTree T.BigOpExprFuncSplit where
  to_string_tree = \(T.BOEFS boefs) -> SH.add_new_root "BigOpExprFuncSplit" boefs

instance STC.ToStringTree T.BigOrCasesFuncExpr where
  to_string_tree = \case
    T.BFE1 bfe -> SH.add_new_root "BigOrCasesFuncExpr" bfe
    T.CFE1 cfe -> SH.add_new_root "BigOrCasesFuncExpr" cfe

instance STC.ToStringTree T.Operand where
  to_string_tree = \case
    T.BOAE2 boae -> SH.add_new_root "Operand" boae
    T.PE3 pe -> SH.add_new_root "Operand" pe
    T.Underscore3 -> SH.root_and_node_to_tree "Operand" "_"

instance STC.ToStringTree T.Op where
  to_string_tree = \case
    T.FCO3 fco -> SH.add_new_root "Op" fco
    T.OSO oso -> SH.add_new_root "Op" oso

instance STC.ToStringTree T.FuncCompOp where
  to_string_tree = \case
    T.RightComp -> SH.root_and_node_to_tree "FuncCompOp" "o>"
    T.LeftComp -> SH.root_and_node_to_tree "FuncCompOp" "<o"

instance STC.ToStringTree T.OptionalSpacesOp where
  to_string_tree = \case
    T.RightApp -> SH.root_and_node_to_tree "OptionalSpacesOp" "->"
    T.LeftApp -> SH.root_and_node_to_tree "OptionalSpacesOp" "<-"
    T.Power -> SH.root_and_node_to_tree "OptionalSpacesOp" "^"
    T.Mult -> SH.root_and_node_to_tree "OptionalSpacesOp" "*"
    T.Div -> SH.root_and_node_to_tree "OptionalSpacesOp" "/"
    T.Plus -> SH.root_and_node_to_tree "OptionalSpacesOp" "+"
    T.Minus -> SH.root_and_node_to_tree "OptionalSpacesOp" "-"
    T.Equal -> SH.root_and_node_to_tree "OptionalSpacesOp" "=="
    T.NotEqual -> SH.root_and_node_to_tree "OptionalSpacesOp" "!="
    T.Greater -> SH.root_and_node_to_tree "OptionalSpacesOp" ">"
    T.Less -> SH.root_and_node_to_tree "OptionalSpacesOp" "<"
    T.GrEq -> SH.root_and_node_to_tree "OptionalSpacesOp" ">="
    T.LeEq -> SH.root_and_node_to_tree "OptionalSpacesOp" "<="
    T.And -> SH.root_and_node_to_tree "OptionalSpacesOp" "&"
    T.Or -> SH.root_and_node_to_tree "OptionalSpacesOp" "|"
    T.Use -> SH.root_and_node_to_tree "OptionalSpacesOp" ">>"
    T.Then -> SH.root_and_node_to_tree "OptionalSpacesOp" ";"

instance STC.ToStringTree T.FuncExpr where
  to_string_tree = \case
    T.LFE4 lfe -> SH.add_new_root "FuncExpr" lfe
    T.BFE2 bfe -> SH.add_new_root "FuncExpr" bfe
    T.CFE2 cfe -> SH.add_new_root "FuncExpr" cfe

instance STC.ToStringTree T.LineFuncExpr where
  to_string_tree = \(T.LFE lfe) -> SH.add_new_root "LineFuncExpr" lfe

instance STC.ToStringTree T.BigFuncExpr where
  to_string_tree = \(T.BFE bfe) -> SH.add_new_root "BigFuncExpr" bfe

instance STC.ToStringTree T.BigFuncBodyOrDeeperBody where
  to_string_tree = \case
    T.BFB bfb -> SH.add_new_root "BigFuncBodyOrDeeperBody" bfb
    T.DB db -> SH.add_new_root "BigFuncBodyOrDeeperBody" db

instance STC.ToStringTree T.LineFuncBody where
  to_string_tree = \case
    T.BOAE3 boae -> SH.add_new_root "LineFuncBody" boae
    T.LOE4 loe -> SH.add_new_root "LineFuncBody" loe
    T.PLFE1 plfe -> SH.add_new_root "LineFuncBody" plfe

instance STC.ToStringTree T.ParenLineFuncExpr where
  to_string_tree = \(T.PLFE plfe) -> SH.add_new_root "ParenLineFuncExpr" plfe

instance STC.ToStringTree T.BigFuncBody where
  to_string_tree = \case
    T.BOAE4 boae -> SH.add_new_root "BigFuncBody" boae
    T.OE1 oe -> SH.add_new_root "BigFuncBody" oe
    T.PLFE2 plfe -> SH.add_new_root "BigFuncBody" plfe

instance STC.ToStringTree T.CasesFuncExpr where
  to_string_tree = \(T.CFE cfe) -> SH.add_new_root "CasesFuncExpr" cfe

instance STC.ToStringTree T.CasesParams where
  to_string_tree = \case
    T.CParamId id -> SH.add_new_root "CasesParams" id
    T.QuestionMark -> SH.root_and_node_to_tree "CasesParams" "?"
    T.Star2 -> SH.root_and_node_to_tree "CasesParams" "*"
    T.CParams cps -> SH.add_new_root "CasesParams" cps

instance STC.ToStringTree T.Case where
  to_string_tree = \(T.Ca ca) -> SH.add_new_root "Case" ca

instance STC.ToStringTree T.EndCase where
  to_string_tree = \(T.EC ec) -> SH.add_new_root "EndCase" ec

instance STC.ToStringTree T.OuterMatching where
  to_string_tree = \case
    T.SId2 sid -> SH.add_new_root "OuterMatching" sid
    T.M1 m -> SH.add_new_root "OuterMatching" m

instance STC.ToStringTree T.EndCaseParam where
  to_string_tree = \case
    T.Id2 id -> SH.add_new_root "EndCaseParam" id
    T.Ellipsis -> SH.root_and_node_to_tree "EndCaseParam" "..."

instance STC.ToStringTree T.Matching where
  to_string_tree = \case
    T.Lit2 lit -> SH.add_new_root "Matching" lit
    T.PFM pfm -> SH.add_new_root "Matching" pfm
    T.TM1 tm -> SH.add_new_root "Matching" tm
    T.LM1 lm -> SH.add_new_root "Matching" lm

instance STC.ToStringTree T.InnerMatching where
  to_string_tree = \case
    T.Star -> SH.root_and_node_to_tree "TypeId" "*"
    T.Id3 id -> SH.add_new_root "InnerMatching" id
    T.M2 m -> SH.add_new_root "InnerMatching" m

instance STC.ToStringTree T.TupleMatching where
  to_string_tree = \(T.TM tm) -> SH.add_new_root "TupleMatching" tm

instance STC.ToStringTree T.ListMatching where
  to_string_tree = \(T.LM lm) -> SH.add_new_root "ListMatching" lm

instance STC.ToStringTree T.RestListMatching where
  to_string_tree = \(T.RLM rlm) -> SH.add_new_root "RestListMatching" rlm

instance STC.ToStringTree T.CaseBody where
  to_string_tree = \case
    T.LFB1 lfb -> SH.add_new_root "CaseBody" lfb
    T.BFB1 bfb -> SH.add_new_root "CaseBody" bfb

instance STC.ToStringTree T.ValueDef where
  to_string_tree = \(T.VD vd) -> SH.add_new_root "ValueDef" vd

instance STC.ToStringTree T.TypeMaybeValueEquals where
  to_string_tree = \(T.TMVE tmve) -> SH.add_new_root "TypeMaybeValueEquals" tmve

instance STC.ToStringTree T.ValueExprMaybeWhere where
  to_string_tree = \(T.VE ve) -> SH.add_new_root "ValueExprMaybeWhere" ve

instance STC.ToStringTree T.ValueExpr where
  to_string_tree = \case
    T.BOAE5 boae -> SH.add_new_root "ValueExpr" boae
    T.OE2 oe -> SH.add_new_root "ValueExpr" oe
    T.FE2 fe -> SH.add_new_root "ValueExpr" fe
    T.BT1 bt -> SH.add_new_root "ValueExpr" bt
    T.BL1 bl -> SH.add_new_root "ValueExpr" bl

instance STC.ToStringTree T.WhereExpr where
  to_string_tree = \(T.WE we) -> SH.add_new_root "WhereExpr" we

instance STC.ToStringTree T.ListValueDefs where
  to_string_tree = \(T.LVDs lvds) -> SH.add_new_root "ListValueDefs" lvds

instance STC.ToStringTree T.IdList where
  to_string_tree = \(T.IL il) -> SH.add_new_root "IdList" il

instance STC.ToStringTree T.TupleValueDefs where
  to_string_tree = \(T.TVDs tvds) -> SH.add_new_root "TupleValueDefs" tvds

instance STC.ToStringTree T.IdTuple where
  to_string_tree = \(T.IT it) -> SH.add_new_root "IdTuple" it

instance STC.ToStringTree T.ValueDefs where
  to_string_tree = \case
    T.VD1 vd -> SH.add_new_root "ValueDefs" vd
    T.LVDs1 lvd -> SH.add_new_root "ValueDefs" lvd
    T.TVDs1 tvd -> SH.add_new_root "ValueDefs" tvd

instance STC.ToStringTree T.Type where
  to_string_tree = \(T.Ty ty) -> SH.add_new_root "Type" ty

instance STC.ToStringTree T.SimpleType where
  to_string_tree = \case
    T.TAIOT1 taiot -> SH.add_new_root "SimpleType" taiot
    T.POPT1 popt -> SH.add_new_root "SimpleType" popt
    T.FT1 ft -> SH.add_new_root "SimpleType" ft

instance STC.ToStringTree T.ProdOrPowerType where
  to_string_tree = \case
    T.PT4 pt -> SH.add_new_root "ProdOrPowerType" pt
    T.PoT5 pt -> SH.add_new_root "ProdOrPowerType" pt

instance STC.ToStringTree T.TypeId where
  to_string_tree = \(T.TId tid) -> SH.root_and_node_to_tree "TypeId" tid

instance STC.ToStringTree T.ParamTVar where
  to_string_tree = \(T.PTV ptv) -> SH.add_new_root "ParamTVar" ptv

instance STC.ToStringTree T.AdHocTVar where
  to_string_tree = \(T.AHTV ahtv) -> SH.add_new_root "AdHocTVar" ahtv

instance STC.ToStringTree T.TypeAppIdOrTV where
  to_string_tree = \case
    T.TAIOA1 taioa -> SH.add_new_root "TypeAppIdOrTV" taioa
    T.PTV1 ptv -> SH.add_new_root "TypeAppIdOrTV" ptv

instance STC.ToStringTree T.TypeAppIdOrAHTV where
  to_string_tree = \(T.TAIOA taioa) -> SH.add_new_root "TypeAppIdOrAHTV" taioa

instance STC.ToStringTree T.TAIOAMiddle where
  to_string_tree = \case
    T.TIdStart1 tis -> SH.add_new_root "TAIOAMiddle" tis
    T.AHTV1 ahtv -> SH.add_new_root "TAIOAMiddle" ahtv

instance STC.ToStringTree T.TypesInParen where
  to_string_tree = \(T.TIP tip) -> SH.add_new_root "TypesInParen" tip

instance STC.ToStringTree T.ProdType where
  to_string_tree = \(T.PT pt) -> SH.add_new_root "ProdType" pt

instance STC.ToStringTree T.FieldType where
  to_string_tree = \case
    T.PBT1 pbt -> SH.add_new_root "FieldType" pbt
    T.PoT2 pt -> SH.add_new_root "FieldType" pt

instance STC.ToStringTree T.PowerBaseType where
  to_string_tree = \case
    T.TAIOT2 taoit -> SH.add_new_root "PowerBaseType" taoit
    T.IPT ipt -> SH.add_new_root "PowerBaseType" ipt

instance STC.ToStringTree T.InParenT where
  to_string_tree = \case
    T.PT3 pt -> SH.add_new_root "InParenT" pt
    T.FT3 ft -> SH.add_new_root "InParenT" ft
    T.PoT3 pt -> SH.add_new_root "InParenT" pt

instance STC.ToStringTree T.PowerType where
  to_string_tree = \(T.PoT pt) -> SH.add_new_root "PowerType" pt

instance STC.ToStringTree T.FuncType where
  to_string_tree = \(T.FT ft) -> SH.add_new_root "FuncType" ft

instance STC.ToStringTree T.InOrOutType where
  to_string_tree = \case
    T.TAIOT3 taiot -> SH.add_new_root "InOrOutType" taiot
    T.POPT2 popt -> SH.add_new_root "InOrOutType" popt
    T.FT2 ft -> SH.add_new_root "InOrOutType" ft

instance STC.ToStringTree T.Condition where
  to_string_tree = \(T.Co co) -> SH.add_new_root "Condition" co

instance STC.ToStringTree T.TypeDef where
  to_string_tree = \case
    T.TTD1 ttd -> SH.add_new_root "TypeDef" ttd
    T.OTD1 otd -> SH.add_new_root "TypeDef" otd

instance STC.ToStringTree T.TupleTypeDef where
  to_string_tree = \(T.TTD ttd) -> SH.add_new_root "TupleTypeDef" ttd

instance STC.ToStringTree T.TypeName where
  to_string_tree = \(T.TN tn) -> SH.add_new_root "TypeName" tn

instance STC.ToStringTree T.ParamVarsInParen where
  to_string_tree = \(T.PVIP pvip) -> SH.add_new_root "ParamVarsInParen" pvip

instance STC.ToStringTree T.FieldNames where
  to_string_tree = \(T.FN fn) -> SH.add_new_root "FieldNames" fn

instance STC.ToStringTree T.SimpleIds where
  to_string_tree = \(T.SIds sids) -> SH.add_new_root "SimpleIds" sids

instance STC.ToStringTree T.OrTypeDef where
  to_string_tree = \(T.OTD otd) -> SH.add_new_root "OrTypeDef" otd

instance STC.ToStringTree T.OrTypeValuesLine where
  to_string_tree = \(T.OTVL otvl) -> SH.add_new_root "OrTypeValueLine" otvl

instance STC.ToStringTree T.OrTypeValuesLines where
  to_string_tree = \(T.OTVLs otvls) -> SH.add_new_root "OrTypeValueLines" otvls

instance STC.ToStringTree T.OrTypeValue where
  to_string_tree = \(T.OTV otv) -> SH.add_new_root "OrTypeValue" otv

instance STC.ToStringTree T.InternalValue where
  to_string_tree = \(T.IV iv) -> SH.add_new_root "InternalValue" iv

instance STC.ToStringTree T.TypeNickname where
  to_string_tree = \(T.TNN tnn) -> SH.add_new_root "TypeNickname" tnn

instance STC.ToStringTree T.TypePropDef where
  to_string_tree = \case
    T.TSB1 tsb -> SH.add_new_root "TypePropDef" tsb
    T.RPD1 rpd -> SH.add_new_root "TypePropDef" rpd

instance STC.ToStringTree T.TypeSigBlock where
  to_string_tree = \(T.TSB tsb) -> SH.add_new_root "TypeSigBlock" tsb

instance STC.ToStringTree T.RenamingPropDef where
  to_string_tree = \(T.RPD rpd) -> SH.add_new_root "RenamingPropDef" rpd

instance STC.ToStringTree T.PropName where
  to_string_tree = \case
    T.NPStart1 nps -> SH.add_new_root "PropName" nps
    T.TIPStart tips -> SH.add_new_root "PropName" tips

instance STC.ToStringTree T.NamePart where
  to_string_tree = \(T.NP str) -> SH.root_and_node_to_tree "NamePart" str

instance STC.ToStringTree T.ImplementationBlock where
  to_string_tree = \(T.IB ib) -> SH.add_new_root "ImplementationBlock" ib

instance STC.ToStringTree T.PropNameWithSubs where
  to_string_tree = \case
    T.NPStart2 nps -> SH.add_new_root "PropNameWithSubs" nps
    T.SIPStart sips -> SH.add_new_root "PropNameWithSubs" sips

instance STC.ToStringTree T.SubsInParen where
  to_string_tree = \(T.SIP sip) -> SH.add_new_root "SubsInParen" sip

instance STC.ToStringTree T.TVarSub where
  to_string_tree = \case
    T.TAIOTS1 taiots -> SH.add_new_root "TVarSub" taiots
    T.POPTS1 popts -> SH.add_new_root "TVarSub" popts
    T.FTS1 fts -> SH.add_new_root "TVarSub" fts

instance STC.ToStringTree T.ProdOrPowerTypeSub where
  to_string_tree = \case
    T.PTS1 pts -> SH.add_new_root "ProdOrPowerTypeSub" pts
    T.PoTS1 pots -> SH.add_new_root "ProdOrPowerTypeSub" pots

instance STC.ToStringTree T.TypeAppIdOrTVSub where
  to_string_tree = \case
    T.TAIOAS1 taioas -> SH.add_new_root "TypeAppIdOrTVSub" taioas
    T.PTV2 ptv -> SH.add_new_root "TypeAppIdOrTVSub" ptv

instance STC.ToStringTree T.TypeAppIdOrAHTVSub where
  to_string_tree = \(T.TAIOAS taioas) -> SH.add_new_root "TypeAppIdOrAHTVSub" taioas

instance STC.ToStringTree T.TAIOASMiddle where
  to_string_tree = \case
    T.TIdStart2 tid_start_pair -> SH.add_new_root "TAIOASMiddle" tid_start_pair
    T.AHTV2 ahtv -> SH.add_new_root "TAIOASMiddle" ahtv

instance STC.ToStringTree T.SubsOrUndersInParen where
  to_string_tree = \(T.SOUIP souip) -> SH.add_new_root "SubsOrUndersInParen" souip

instance STC.ToStringTree T.SubOrUnder where
  to_string_tree = \case
    T.TVS1 tvs -> SH.add_new_root "SubOrUnder" tvs
    T.Underscore4 -> SH.root_and_node_to_tree "SubOrUnder" "_"

instance STC.ToStringTree T.PowerTypeSub where
  to_string_tree = \(T.PoTS pots_pair) -> SH.add_new_root "PowerTypeSub" pots_pair

instance STC.ToStringTree T.PowerBaseTypeSub where
  to_string_tree = \case
    T.Underscore5 -> SH.root_and_node_to_tree "PowerBaseTypeSub" "_"
    T.TAIOTS2 taiots -> SH.add_new_root "PowerBaseTypeSub" taiots
    T.IPTS ipts -> SH.add_new_root "PowerBaseTypeSub" ipts

instance STC.ToStringTree T.InParenTSub where
  to_string_tree = \case
    T.PTS2 pts -> SH.add_new_root "InParenTSub" pts
    T.FTS2 fts -> SH.add_new_root "InParenTSub" fts

instance STC.ToStringTree T.ProdTypeSub where
  to_string_tree = \(T.PTS pts) -> SH.add_new_root "FuncTypeSub" pts

instance STC.ToStringTree T.FieldTypeSub where
  to_string_tree = \case
    T.PBTS1 pbts -> SH.add_new_root "FieldTypeSub" pbts
    T.PoTS2 pts -> SH.add_new_root "FieldTypeSub" pts

instance STC.ToStringTree T.FuncTypeSub where
  to_string_tree = \(T.FTS fts_pair) -> SH.add_new_root "FuncTypeSub" fts_pair

instance STC.ToStringTree T.InOrOutTypeSub where
  to_string_tree = \case
    T.Underscore6 -> SH.root_and_node_to_tree "InOrOutTypeSub" "_"
    T.TAIOTS3 taiots -> SH.add_new_root "InOrOutTypeSub" taiots
    T.POPTS2 popts -> SH.add_new_root "InOrOutTypeSub" popts
    T.FTS3 fts -> SH.add_new_root "InOrOutTypeSub" fts

instance STC.ToStringTree T.Implementation where
  to_string_tree = \(T.I imoi_veme) -> SH.add_new_root "Implementation" imoi_veme

instance STC.ToStringTree T.IdMaybeOpId where
  to_string_tree = \(T.IMOI imoi) -> SH.add_new_root "IdMaybeOpId" imoi

instance STC.ToStringTree T.Comment where
  to_string_tree = \(T.C c) -> SH.root_and_node_to_tree "Comment" c

instance STC.ToStringTree T.Program where
  to_string_tree = \(T.P p) -> SH.add_new_root "Program" p

instance STC.ToStringTree T.ProgramPart where
  to_string_tree = \case
    T.VDD vds -> SH.add_new_root "ProgramPart" vds
    T.TD td -> SH.add_new_root "ProgramPart" td
    T.TNN1 tn -> SH.add_new_root "ProgramPart" tn
    T.TPD tpd -> SH.add_new_root "ProgramPart" tpd
    T.TT1 ib -> SH.add_new_root "ProgramPart" ib
    T.C1 c -> SH.add_new_root "ProgramPart" c

