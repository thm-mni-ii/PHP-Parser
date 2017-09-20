package de.thm.ii.phpparser.ast

import de.thm.ii.phpparser.ast.Expressions.{Expression, SimpleNameVar, Variable}
import de.thm.ii.phpparser.ast.Statements.Statement

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

  sealed abstract class StringElement
  case class UnicodeStringElement(value: Either[Seq[Char], Variable]) extends StringElement
  case class OctalStringElement(digits: Seq[Char]) extends StringElement
  case class HexStringElement(digits: Seq[Char]) extends StringElement
  case class ExpressionStringElement(exp: Expression) extends StringElement
  case class VarStringElement(variable: SimpleNameVar, access: Option[StringVarAccess]) extends StringElement

  sealed abstract class StringVarAccess
  case class PropertyStringVarAcc(name: Name) extends StringVarAccess
  case class NameOffsetStringVarAcc(name: Name) extends StringVarAccess
  case class VarOffsetStringVarAcc(variable: SimpleNameVar) extends StringVarAccess
  case class IntOffsetStringVarAcc(integerLiteral: IntegerLiteral) extends StringVarAccess

  case class SQStringLiteral(prefix: Option[String], sequence: String) extends StringLiteral

  case class DQStringLiteral(prefix: Option[String], sequence: Seq[StringElement]) extends StringLiteral
  case class DQStringElement(s: String) extends StringElement

  case class HeredocStringLiteral(prefix: Option[String], hdIdentifier: Name, sequence: Seq[StringElement]) extends StringLiteral
  case class HDStringElement(s: String) extends StringElement
  case object HDNewLine extends StringElement

  case class NowdocStringLiteral(prefix: Option[String], ndIdentifier: Name, sequence: Seq[StringElement]) extends StringLiteral
  case class NDStringElement(s: String) extends StringElement
  case object NDNewLine extends StringElement


  sealed abstract class IntegerLiteral extends Literal
  case class DecimalLiteral(value: String) extends IntegerLiteral
  case class OctalLiteral(value: String) extends IntegerLiteral
  case class HexadecimalLiteral(value: String) extends IntegerLiteral
  case class BinaryLiteral(value: String) extends IntegerLiteral

  case class FloatingLiteral(digits: String, fracDigits: String, exponent: Option[(Boolean, String)]) extends Literal
}
