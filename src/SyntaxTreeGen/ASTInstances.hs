module SyntaxTreeGen.ASTInstances where

import Prelude ((++))
import Helpers ((.>))
import Prelude qualified as P
import ASTTypes qualified as T
import SyntaxTreeGen.TypesAndClasses qualified as STC

to_dot_final :: STC.ToDot a => a -> STC.Dot
to_dot_final = STC.to_dot .> \(root, dot) ->
  "graph " ++ root ++ "\n{\n" ++ dot ++  "\n}"

instance STC.ToDot P.Integer where
  to_dot = P.undefined

instance STC.ToDot P.Double where
  to_dot = P.undefined

instance STC.ToDot P.Char where
  to_dot = P.undefined

instance STC.ToDot P.String where
  to_dot = P.undefined

instance STC.ToDot T.Literal where
  to_dot = P.undefined

instance STC.ToDot T.Identifier where
  to_dot = P.undefined

instance STC.ToDot T.SimpleId where
  to_dot = P.undefined

instance STC.ToDot T.IdStart where
  to_dot = P.undefined

instance STC.ToDot T.IdCont where
  to_dot = P.undefined

instance STC.ToDot T.UndersInParen where
  to_dot = P.undefined

instance STC.ToDot T.ParenExpr where
  to_dot = P.undefined

instance STC.ToDot T.InsideParenExpr where
  to_dot = P.undefined

instance STC.ToDot T.Tuple where
  to_dot = P.undefined

instance STC.ToDot T.LineExprOrUnders where
  to_dot = P.undefined

instance STC.ToDot T.LineExprOrUnder where
  to_dot = P.undefined

instance STC.ToDot T.LineExpr where
  to_dot = P.undefined

instance STC.ToDot T.BasicOrAppExpr where
  to_dot = P.undefined

instance STC.ToDot T.BasicExpr where
  to_dot = P.undefined

instance STC.ToDot T.BigTuple where
  to_dot = P.undefined

instance STC.ToDot T.List where
  to_dot = P.undefined

instance STC.ToDot T.BigList where
  to_dot = P.undefined

instance STC.ToDot T.ArgsStr where
  to_dot = P.undefined

instance STC.ToDot T.ParenFuncAppOrId where
  to_dot = P.undefined

instance STC.ToDot T.Arguments where
  to_dot = P.undefined

instance STC.ToDot T.PreFunc where
  to_dot = P.undefined

instance STC.ToDot T.PreFuncApp where
  to_dot = P.undefined

instance STC.ToDot T.DotId where
  to_dot = P.undefined

instance STC.ToDot T.SimpleOrSpecialId where
  to_dot = P.undefined

instance STC.ToDot T.SpecialId where
  to_dot = P.undefined

instance STC.ToDot T.PostFuncApp where
  to_dot = P.undefined

instance STC.ToDot T.DotChangeApp where
  to_dot = P.undefined

instance STC.ToDot T.DotIdsApp where
  to_dot = P.undefined

instance STC.ToDot T.PostFuncArg where
  to_dot = P.undefined

instance STC.ToDot T.DotChange where
  to_dot = P.undefined

instance STC.ToDot T.FieldChange where
  to_dot = P.undefined

instance STC.ToDot T.OpExpr where
  to_dot = P.undefined

instance STC.ToDot T.OpExprStart where
  to_dot = P.undefined

instance STC.ToDot T.LineOpExpr where
  to_dot = P.undefined

instance STC.ToDot T.LineOpExprEnd where
  to_dot = P.undefined

instance STC.ToDot T.BigOpExpr where
  to_dot = P.undefined

instance STC.ToDot T.BigOpExprOpSplit where
  to_dot = P.undefined

instance STC.ToDot T.OpSplitLine where
  to_dot = P.undefined

instance STC.ToDot T.OperFCO where
  to_dot = P.undefined

instance STC.ToDot T.OpSplitEnd where
  to_dot = P.undefined

instance STC.ToDot T.BigOpExprFuncSplit where
  to_dot = P.undefined

instance STC.ToDot T.BigOrCasesFuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.Operand where
  to_dot = P.undefined

instance STC.ToDot T.Op where
  to_dot = P.undefined

instance STC.ToDot T.FuncCompOp where
  to_dot = P.undefined

instance STC.ToDot T.OptionalSpacesOp where
  to_dot = P.undefined

instance STC.ToDot T.FuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.LineFuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.BigFuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.BigFuncBodyOrDeeperBody where
  to_dot = P.undefined

instance STC.ToDot T.LineFuncBody where
  to_dot = P.undefined

instance STC.ToDot T.ParenLineFuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.BigFuncBody where
  to_dot = P.undefined

instance STC.ToDot T.CasesFuncExpr where
  to_dot = P.undefined

instance STC.ToDot T.CasesParams where
  to_dot = P.undefined

instance STC.ToDot T.Case where
  to_dot = P.undefined

instance STC.ToDot T.EndCase where
  to_dot = P.undefined

instance STC.ToDot T.OuterMatching where
  to_dot = P.undefined

instance STC.ToDot T.EndCaseParam where
  to_dot = P.undefined

instance STC.ToDot T.Matching where
  to_dot = P.undefined

instance STC.ToDot T.InnerMatching where
  to_dot = P.undefined

instance STC.ToDot T.TupleMatching where
  to_dot = P.undefined

instance STC.ToDot T.ListMatching where
  to_dot = P.undefined

instance STC.ToDot T.RestListMatching where
  to_dot = P.undefined

instance STC.ToDot T.CaseBody where
  to_dot = P.undefined

instance STC.ToDot T.ValueDef where
  to_dot = P.undefined

instance STC.ToDot T.TypeMaybeValueEquals where
  to_dot = P.undefined

instance STC.ToDot T.ValueExprMaybeWhere where
  to_dot = P.undefined

instance STC.ToDot T.ValueExpr where
  to_dot = P.undefined

instance STC.ToDot T.WhereExpr where
  to_dot = P.undefined

instance STC.ToDot T.ListValueDefs where
  to_dot = P.undefined

instance STC.ToDot T.IdList where
  to_dot = P.undefined

instance STC.ToDot T.TupleValueDefs where
  to_dot = P.undefined

instance STC.ToDot T.IdTuple where
  to_dot = P.undefined

instance STC.ToDot T.ValueDefs where
  to_dot = P.undefined

instance STC.ToDot T.Type where
  to_dot = P.undefined

instance STC.ToDot T.SimpleType where
  to_dot = P.undefined

instance STC.ToDot T.ProdOrPowerType where
  to_dot = P.undefined

instance STC.ToDot T.TypeId where
  to_dot = P.undefined

instance STC.ToDot T.ParamTVar where
  to_dot = P.undefined

instance STC.ToDot T.AdHocTVar where
  to_dot = P.undefined

instance STC.ToDot T.TypeAppIdOrTV where
  to_dot = P.undefined

instance STC.ToDot T.TypeAppIdOrAHTV where
  to_dot = P.undefined

instance STC.ToDot T.TAIOAMiddle where
  to_dot = P.undefined

instance STC.ToDot T.TypesInParen where
  to_dot = P.undefined

instance STC.ToDot T.ProdType where
  to_dot = P.undefined

instance STC.ToDot T.FieldType where
  to_dot = P.undefined

instance STC.ToDot T.PowerBaseType where
  to_dot = P.undefined

instance STC.ToDot T.InParenT where
  to_dot = P.undefined

instance STC.ToDot T.PowerType where
  to_dot = P.undefined

instance STC.ToDot T.FuncType where
  to_dot = P.undefined

instance STC.ToDot T.InOrOutType where
  to_dot = P.undefined

instance STC.ToDot T.Condition where
  to_dot = P.undefined

instance STC.ToDot T.TypeDef where
  to_dot = P.undefined

instance STC.ToDot T.TupleTypeDef where
  to_dot = P.undefined

instance STC.ToDot T.TypeName where
  to_dot = P.undefined

instance STC.ToDot T.ParamVarsInParen where
  to_dot = P.undefined

instance STC.ToDot T.FieldNames where
  to_dot = P.undefined

instance STC.ToDot T.SimpleIds where
  to_dot = P.undefined

instance STC.ToDot T.OrTypeDef where
  to_dot = P.undefined

instance STC.ToDot T.OrTypeValuesLine where
  to_dot = P.undefined

instance STC.ToDot T.OrTypeValuesLines where
  to_dot = P.undefined

instance STC.ToDot T.OrTypeValue where
  to_dot = P.undefined

instance STC.ToDot T.InternalValue where
  to_dot = P.undefined

instance STC.ToDot T.TypeNickname where
  to_dot = P.undefined

instance STC.ToDot T.TypePropDef where
  to_dot = P.undefined

instance STC.ToDot T.TypeSigBlock where
  to_dot = P.undefined

instance STC.ToDot T.RenamingPropDef where
  to_dot = P.undefined

instance STC.ToDot T.PropName where
  to_dot = P.undefined

instance STC.ToDot T.NamePart where
  to_dot = P.undefined

instance STC.ToDot T.ImplementationBlock where
  to_dot = P.undefined

instance STC.ToDot T.PropNameWithSubs where
  to_dot = P.undefined

instance STC.ToDot T.SubsInParen where
  to_dot = P.undefined

instance STC.ToDot T.TVarSub where
  to_dot = P.undefined

instance STC.ToDot T.ProdOrPowerTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.TypeAppIdOrTVSub where
  to_dot = P.undefined

instance STC.ToDot T.TypeAppIdOrAHTVSub where
  to_dot = P.undefined

instance STC.ToDot T.TAIOASMiddle where
  to_dot = P.undefined

instance STC.ToDot T.SubsOrUndersInParen where
  to_dot = P.undefined

instance STC.ToDot T.SubOrUnder where
  to_dot = P.undefined

instance STC.ToDot T.PowerTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.PowerBaseTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.InParenTSub where
  to_dot = P.undefined

instance STC.ToDot T.ProdTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.FieldTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.FuncTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.InOrOutTypeSub where
  to_dot = P.undefined

instance STC.ToDot T.Implementation where
  to_dot = P.undefined

instance STC.ToDot T.IdMaybeOpId where
  to_dot = P.undefined

instance STC.ToDot T.Comment where
  to_dot = P.undefined

instance STC.ToDot T.Program where
  to_dot = \_ -> ("prog", "dot -- out")

instance STC.ToDot T.ProgramPart where
  to_dot = P.undefined
