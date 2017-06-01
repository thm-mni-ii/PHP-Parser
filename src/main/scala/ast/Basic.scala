package ast

import ast.Expressions.{Expression, SimpleNameVar, Variable}
import ast.Statements.Statement

object Basic {

  case class Script(leadingText: Text, s: Seq[Statement])

  case class Text(text: String)

  trait EndTagElement {
    val text: Option[Text]
  }

  case class Name(name: String)

  case class QualifiedName(nType: NamespaceType.Value, namespace: Seq[Name], name: Name)
  case object NamespaceType extends Enumeration {
    val RELATIVE, LOCAL, GLOBAL = Value
  }

  sealed abstract class Literal extends Expression

  sealed abstract class StringLiteral extends Literal
  case class SQStringLiteral(prefix: Option[String], sequence: String) extends StringLiteral

  case class DQStringLiteral(prefix: Option[String], sequence: Seq[DQElement]) extends StringLiteral
  sealed abstract class DQElement
  case class DQStringElement(s: String) extends DQElement
  case class WrappedUnicodeDQElement(value: Either[Seq[Char], Variable]) extends DQElement
  case class OctalDQElement(digits: Seq[Char]) extends DQElement
  case class HexDQElement(digits: Seq[Char]) extends DQElement
  case class ExpressionDQElement(exp: Expression) extends DQElement
  case class VarDQElement(variable: SimpleNameVar, access: Option[DQVarAccess]) extends DQElement
  sealed abstract class DQVarAccess
  case class PropertyDQVarAcc(name: Name) extends DQVarAccess
  case class NameOffsetDQVarAcc(name: Name) extends DQVarAccess
  case class VarOffsetDQVarAcc(variable: SimpleNameVar) extends DQVarAccess
  case class IntOffsetDQVarAcc(integerLiteral: IntegerLiteral) extends DQVarAccess


  sealed abstract class IntegerLiteral extends Literal
  case class DecimalLiteral(value: String) extends IntegerLiteral
  case class OctalLiteral(value: String) extends IntegerLiteral
  case class HexadecimalLiteral(value: String) extends IntegerLiteral
  case class BinaryLiteral(value: String) extends IntegerLiteral

  case class FloatingLiteral(digits: String, fracDigits: String, exponent: Option[(Boolean, String)]) extends Literal
}
