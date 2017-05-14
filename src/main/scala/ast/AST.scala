package ast

/**
  * Created by tobias on 02.05.17.
  */
/**
  * The definition of the abstract syntax
  */
object Ast {

  case class Script(leadingText: Text, s: Seq[Statement])

  case class Text(text: String)

  trait EndTagElement {
    val text: Option[Text]
  }

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
  case class ForeachStmnt() extends IterationStmnt
  sealed abstract class ForeachValue
  case class ForeachValueExp(exp: Expression, designateVar: Boolean) extends ForeachValue
  case class ForeachValueSeq(Seq: SeqIntrinsic) extends ForeachValue

  sealed abstract class JumpStmnt extends Statement

  case class GotoStmnt(name: Name, override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class ContinueStmnt(lvl: IntegerLiteral, override val text: Option[Text]) extends JumpStmnt with EndTagElement
  case class BreakStmnt(lvl: IntegerLiteral, override val text: Option[Text]) extends JumpStmnt with EndTagElement
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
  case class ClassDecl(mod: Option[ClassModifier], name: Name, extend: Option[QualifiedName], impl: Option[Seq[QualifiedName]], body: Seq[MemberDecl]) extends Declaration

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

  case class NamespaceUseDecl() extends Declaration

  case class GlobalDecl(vars: Seq[SimpleNameVar]) extends Declaration

  case class FuncStaticDecl(vars: Seq[StaticVarElement]) extends Declaration

  case class StaticVarElement(name: SimpleNameVar, initValue: Option[Expression])

  case object VoidReturn

  sealed abstract class Definition extends Statement

  case class FuncHeader(returnRef: Boolean, name: Name, params: Seq[ParameterDecl], ret: Option[PossibleTypes])

  case class FuncDef(header: FuncHeader, body: CompoundStmnt) extends Statement

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

  case class NamespaceDef(name: Option[Name], stmnt: Option[CompoundStmnt]) extends Statement

  case class Name(name: String)



  case class SeqIntrinsic()


  sealed abstract class Expression

  case class SpecialExp() extends Expression
  sealed abstract class DereferencableExp extends Expression
  case class IncludeExp(exp: Expression) extends Expression
  case class IncludeOnceExp(exp: Expression) extends Expression
  case class RequireExp(exp: Expression) extends Expression
  case class RequireOnceExp(exp: Expression) extends Expression
  case class YieldExp(exp: Expression) extends Expression
  case class YieldFromExp(a: ArrayElementInitializer) extends Expression
  case class LogicalOrExp2(exp1: Expression, exp2: Expression) extends Expression
  case class LogicalXOrExp(exp1: Expression, exp2: Expression) extends Expression
  case class LogicalAndExp2(exp1: Expression, exp2: Expression) extends Expression
  case class AssignmentExp(e: Boolean) extends Expression
  case class TernaryExp(cond: Expression, first: Option[Expression], second: Expression) extends Expression
  case class SimpleAssignmentExp(byRef: Boolean, variable: Variable, exp: Expression) extends Expression
  case class CoalesceExp(exp1: Expression, exp2: Expression) extends Expression
  case class CompoundAssignmentExp(op: String, variable: Variable, exp: Expression) extends Expression
  case class LogicalOrExp(exp1: Expression, exp2: Expression) extends Expression
  case class LogicalAndExp(exp1: Expression, exp2: Expression) extends Expression
  case class BitwiseOrExp(exp1: Expression, exp2: Expression) extends Expression
  case class BitwiseXOrExp(exp1: Expression, exp2: Expression) extends Expression
  case class BitwiseAndExp(exp1: Expression, exp2: Expression) extends Expression
  case class EqualityExp(op: String, exp1: Expression, exp: Expression) extends Expression
  case class RelationalExp(op: String, exp1: Expression, exp2: Expression) extends Expression
  case class LShiftExp(exp1: Expression, exp2: Expression) extends Expression
  case class RShiftExp(exp1: Expression, exp2: Expression) extends Expression
  case class AddExp(exp1: Expression, exp2: Expression) extends Expression
  case class SubExp(exp1: Expression, exp2: Expression) extends Expression
  case class ConcatExp(exp1: Expression, exp2: Expression) extends Expression
  case class MulExp(exp1: Expression, exp2: Expression) extends Expression
  case class DivExp(exp1: Expression, exp2: Expression) extends Expression
  case class ModExp(exp1: Expression, exp2: Expression) extends Expression
  case class ExponentiationExp(exp1: Expression, exp2: Expression) extends Expression
  case class InstanceOfExp(exp: Expression, of: Either[Expression, QualifiedName]) extends Expression
  case class PrefixIncrementExp(va: Variable) extends Expression
  case class PrefixDecrementExp(va: Variable) extends Expression
  case class UnaryOpExp(op: String, exp: Expression) extends Expression
  case class ErrorControlExp(exp: Expression) extends Expression
  case class ShellCommandExp(content: String) extends Expression
  case class CastExp(cast: CastType.Value, exp: Expression) extends Expression

  case object CastType extends Enumeration {
    val ARRAY, BINARY, BOOL, BOOLEAN, DOUBLE, INT, INTEGER, FLOAT, OBJECT, REAL, STRING, UNSET = Value
  }

  case class ObjectCreationExp(designator: Expression) extends Expression //TODO
  case class PostfixIncrementExp(va: Variable) extends Expression

  case class PostfixDecrementExp(va: Variable) extends Expression

  case class CloneExp(exp: Expression) extends Expression

  case class ClassConstAccessExp()

  sealed abstract class Variable extends Expression

  sealed abstract class SimpleVar extends Variable
  case class SimpleNameVar(name: Name) extends SimpleVar
  case class SimpleAccessVar(acc: SimpleVar) extends SimpleVar
  case class SimpleExpVar(exp: Expression) extends SimpleVar
  case class ScopeAccessVar(scope: ScopeType.Value) extends Variable
  case object ScopeType extends Enumeration {
    val SELF, PARENT, STATIC = Value
  }

  case class QualifiedName(nType: NamespaceType.Value, namespace: Seq[Name], name: Name)

  case object NamespaceType extends Enumeration {
    val RELATIVE, LOCAL, GLOBAL = Value
  }

  case class QualifiedNameVar(name: QualifiedName) extends Variable
  case class ExpressionVar(exp: Expression) extends Variable
  case class StringLiteralVar(literal: StringLiteral) extends Variable

  sealed abstract class MemberName

  case class NameMember(name: Name) extends MemberName
  case class SimpleVarMember(s: SimpleVar) extends MemberName
  case class ExpMember(exp: Expression) extends MemberName

  sealed abstract class Accessor(from: Variable) extends Variable

  sealed abstract class StaticAccessor(from: Variable) extends Accessor(from)
  case class MemberCallStaticAcc(from: Variable, member: MemberName, args: Seq[ArgumentExpression]) extends StaticAccessor(from)
  case class SimpleVarStaticAcc(from: Variable, s: SimpleVar) extends StaticAccessor(from)

  sealed abstract class PropertyAccessor(from: Variable) extends Accessor(from)
  case class MemberPropertyAcc(from: Variable, member: MemberName) extends PropertyAccessor(from: Variable)
  case class MemberCallPropertyAcc(from: Variable, member: MemberName, args: Seq[ArgumentExpression]) extends PropertyAccessor(from: Variable)

  case class CallAccessor(from: Variable, args: Seq[ArgumentExpression]) extends Accessor(from)
  case class ArrayAcc(from: Variable, exp: Option[Expression]) extends Accessor(from)
  case class BlockAcc(from: Variable, exp: Expression) extends Accessor(from)

  case class ArrayCreationVar(elems: Seq[ArrayElement]) extends Variable
  case class ArrayElement(key: Option[Expression], value: Expression, designateVar: Boolean)
  case class ArgumentExpression(isVariadic: Boolean, exp: Expression)

  case class ArrayElementInitializer(key: Option[Expression], value: Expression, designateVar: Boolean)

  sealed abstract class Literal extends Expression

  sealed abstract class StringLiteral extends Literal
  case class DQStringLiteral(prefix: Option[String], sequence: String) extends StringLiteral
  case class SQStringLiteral(prefix: Option[String], sequence: String) extends StringLiteral

  sealed abstract class IntegerLiteral extends Literal
  case class DecimalLiteral(value: String) extends IntegerLiteral
  case class OctalLiteral(value: String) extends IntegerLiteral
  case class HexadecimalLiteral(value: String) extends IntegerLiteral
  case class BinaryLiteral(value: String) extends IntegerLiteral

  case class FloatingLiteral(digits: String, fracDigits: String, exponent: Option[(Boolean, String)]) extends Literal

}