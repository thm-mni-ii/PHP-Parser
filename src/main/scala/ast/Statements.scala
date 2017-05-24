package ast

import ast.Expressions.{Expression, SimpleNameVar}
import ast.Basic._

object Statements {

  sealed abstract class Statement

  case class EmptyStmnt(override val text: Option[Text]) extends Statement with EndTagElement
  case class EchoTagStmnt(exps : Seq[Expression], override val text: Option[Text]) extends Statement with EndTagElement

  case class CompoundStmnt(stmnts: Seq[Statement]) extends Statement
  case class NamedLabelStmnt(name: Name, stmt: Statement) extends Statement

  case class ExpressionStmnt(exp: Expression, override val text: Option[Text]) extends Statement with EndTagElement

  sealed abstract class SelectionStmnt extends Statement

  case class IfStmnt(exp: Expression, stmnts: Seq[Statement], elseifs: Seq[(Expression, Seq[Statement])], elseStmnts: Option[Seq[Statement]], override val text: Option[Text]) extends SelectionStmnt with EndTagElement
  case class SwitchStmnt(exp: Expression, cases: Seq[SwitchBlock], override val text: Option[Text]) extends SelectionStmnt with EndTagElement
  sealed abstract class SwitchBlock
  case class CaseBlock(exp: Expression, stmnts: Seq[Statement]) extends SwitchBlock
  case class DefaultBlock(stmnts: Seq[Statement]) extends SwitchBlock

  sealed abstract class IterationStmnt extends Statement

  case class WhileStmnt(cond: Expression, stmnts: Seq[Statement], override val text: Option[Text]) extends IterationStmnt with EndTagElement
  case class DoStmnt(cond: Expression, stmnt: Statement, override val text: Option[Text]) extends IterationStmnt with EndTagElement
  case class ForExpressionList(exps: Seq[Expression], override val text: Option[Text]) extends EndTagElement
  case class ForStmnt(init: ForExpressionList, control: ForExpressionList, end: ForExpressionList, stmnts: Seq[Statement], override val text: Option[Text]) extends IterationStmnt with EndTagElement
  case class ForeachStmnt(collection: Expression, key: Option[Expression], valueDesVar: Boolean, value: Expression, stmnts: Seq[Statement], override val text: Option[Text]) extends IterationStmnt with EndTagElement

  sealed abstract class JumpStmnt extends Statement

  case class GotoStmnt(name: Name, override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class ContinueStmnt(lvl: Option[IntegerLiteral], override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class BreakStmnt(lvl: Option[IntegerLiteral], override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class ReturnStmnt(exp: Option[Expression], override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class ThrowStmnt(exp: Expression, override val text: Option[Text]) extends JumpStmnt with EndTagElement

  case class TryStmnt(stmnt: CompoundStmnt, catches: Seq[CatchClause], fin: Option[CompoundStmnt]) extends Statement
  case class CatchClause(qualifiedName: QualifiedName, name: SimpleNameVar, stmnt: CompoundStmnt)

  case class DeclareStmnt(decl: DeclareDeclarative.Value, declLiteral: Literal, stmnts: Seq[Statement], override val text: Option[Text]) extends Statement with EndTagElement
  case object DeclareDeclarative extends Enumeration {
    val TICKS, ENCODING, STRICT_TYPES = Value
  }

  sealed abstract class Declaration extends Statement

  case class ConstDecl(elems: Seq[ConstElement]) extends Declaration
  case class ConstElement(name: Name, exp: Expression)
  case class ClassDecl(mod: Option[ClassModifier], name: Option[Name], extend: Option[QualifiedName], impl: Option[Seq[QualifiedName]], body: Seq[MemberDecl]) extends Declaration

  trait MemberDecl

  case class ClassConstDecl(mod: Option[VisibilityModifier], elems: Seq[ConstElement]) extends MemberDecl
  case class PropertyDecl(mod: PropertyModifier, elems: Seq[PropertyElement]) extends MemberDecl
  case class PropertyElement(name: SimpleNameVar, initValue: Option[Expression])
  case class MethodDecl(mods: Seq[MethodModifier], header: FuncHeader, body: Option[CompoundStmnt]) extends MemberDecl
  case class TraitUseClause(traits: Seq[QualifiedName], useSpec: Seq[TraitUseSpec]) extends MemberDecl

  sealed abstract class TraitUseSpec

  case class SelectInsteadofClause(from: Name, to: Name) extends TraitUseSpec
  case class TraitAliasClause(from: Name, asMod: Option[VisibilityModifier], as: Option[Name]) extends TraitUseSpec

  sealed abstract class Modifier

  trait ClassModifier extends MethodModifier
  trait VisibilityModifier extends MethodModifier
  trait PropertyModifier
  trait StaticModifier extends MethodModifier
  trait MethodModifier

  case object NoMod extends Modifier with PropertyModifier
  case class CombinedMod(staticMod: Option[StaticModifier], visMod: Option[VisibilityModifier]) extends Modifier with PropertyModifier
  case object StaticMod extends Modifier with StaticModifier
  case object AbstractMod extends Modifier with ClassModifier
  case object FinalMod extends Modifier with ClassModifier
  case object PublicMod extends Modifier with VisibilityModifier
  case object ProtectedMod extends Modifier with VisibilityModifier
  case object PrivateMod extends Modifier with VisibilityModifier

  case class InterfaceDecl(name: Name, extend: Option[Seq[QualifiedName]], body: Seq[MemberDecl]) extends Declaration

  case class TraitDecl(name: Name, body: Seq[MemberDecl]) extends Declaration

  case class NamespaceUseDecl(useType: Option[NamespaceUseType.Value], namespaceName: Option[Seq[Name]], clauses: Seq[NamespaceUseClause], override val text: Option[Text]) extends Declaration with EndTagElement
  case class NamespaceUseClause(useType: Option[NamespaceUseType.Value], from: Either[QualifiedName, Seq[Name]], as: Option[Name])
  case object NamespaceUseType extends Enumeration {
    val FUNCTION, CONST = Value
  }

  case class GlobalDecl(vars: Seq[SimpleNameVar]) extends Declaration

  case class FuncStaticDecl(vars: Seq[StaticVarElement]) extends Declaration
  case class StaticVarElement(name: SimpleNameVar, initValue: Option[Expression])

  sealed abstract class Definition extends Statement

  case class FuncDef(header: FuncHeader, body: CompoundStmnt) extends Definition
  case class FuncHeader(returnRef: Boolean, name: Name, params: Seq[ParameterDecl], ret: Option[PossibleTypes])
  sealed abstract class ParameterDecl
  case class SimpleParam(t: Option[TypeDecl], desVar: Boolean, name: SimpleNameVar, defValue: Option[Expression]) extends ParameterDecl
  case class VariadicParam(t: Option[TypeDecl], desVar: Boolean, name: SimpleNameVar) extends ParameterDecl
  sealed abstract class PossibleTypes
  case object VoidType extends PossibleTypes
  sealed abstract class TypeDecl extends PossibleTypes
  case object ArrayType extends TypeDecl
  case object CallableType extends TypeDecl
  case object IterableType extends TypeDecl
  sealed abstract class ScalarTypeDecl extends TypeDecl
  case object BoolType extends ScalarTypeDecl
  case object FloatType extends ScalarTypeDecl
  case object IntType extends ScalarTypeDecl
  case object StringType extends ScalarTypeDecl
  case class QualifiedType(name: QualifiedName) extends TypeDecl()

  case class NamespaceDef(name: Option[Name], stmnt: Option[CompoundStmnt]) extends Definition
}
