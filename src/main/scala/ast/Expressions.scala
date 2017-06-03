package ast

import ast.Basic._
import ast.Statements.{ClassDecl, CompoundStmnt, FuncHeader}

object Expressions {

  abstract class Expression

  case class IncludeExp(exp: Expression) extends Expression
  case class IncludeOnceExp(exp: Expression) extends Expression
  case class RequireExp(exp: Expression) extends Expression
  case class RequireOnceExp(exp: Expression) extends Expression
  case class YieldExp(a: ArrayElement) extends Expression
  case class YieldFromExp(exp: Expression) extends Expression
  case class LogicalOrExp2(exp1: Expression, exp2: Expression) extends Expression
  case class LogicalXOrExp(exp1: Expression, exp2: Expression) extends Expression
  case class LogicalAndExp2(exp1: Expression, exp2: Expression) extends Expression
  case class AssignmentExp(e: Boolean) extends Expression
  case class TernaryExp(cond: Expression, first: Option[Expression], second: Expression) extends Expression
  case class SimpleAssignmentExp(byRef: Boolean, variable: Variable, exp: Expression) extends Expression
  case class ListAssignmentExp(list: ListIntrinsic, exp: Expression) extends Expression
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
  case class AnonymousFunctionCreationExp(isStatic: Boolean, header: FuncHeader, useClause: Seq[(Boolean, SimpleNameVar)], body: CompoundStmnt) extends Expression
  case class ShellCommandExp(sequence: Seq[StringElement]) extends Expression
  case class CastExp(cast: CastType.Value, exp: Expression) extends Expression

  case object CastType extends Enumeration {
    val ARRAY, BINARY, BOOL, BOOLEAN, DOUBLE, INT, INTEGER, FLOAT, OBJECT, REAL, STRING, UNSET = Value
  }

  sealed abstract class Intrinsic extends Expression
  case class EchoIntrinsic(exps: Seq[Expression]) extends Intrinsic
  case class UnsetIntrinsic(vars: Seq[Variable]) extends Intrinsic
  case class EmptyIntrinsic(exp: Expression) extends Intrinsic
  case class EvalIntrinsic(exp: Expression) extends Intrinsic
  case class ExitIntrinsic(exp: Option[Expression]) extends Intrinsic
  case class IssetIntrinsic(vars: Seq[Variable]) extends Intrinsic
  case class PrintIntrinsic(exp: Expression) extends Intrinsic
  case class ListIntrinsic(expList: Either[Seq[Expression], Seq[(Expression, Expression)]]) extends Intrinsic

  case class InstanceCreationExp(designator: Expression, arguments: Option[Seq[ArgumentExpression]]) extends Expression
  case class AnonymousClassCreationExp(decl: ClassDecl, arguments: Option[Seq[ArgumentExpression]]) extends Expression

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

  case class QualifiedNameVar(name: QualifiedName) extends Variable
  case class EnclosedExp(exp: Expression) extends Variable
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

  case class ClassConstAcc(from: Variable, name: Name) extends Accessor(from)

  //unused
  case class SpecialExp() extends Expression
}
