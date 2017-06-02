package parser.literals

import ast.Basic._
import ast.Expressions.SimpleNameVar
import fastparse.all._
import parser.literals.Keywords.allKeywords
import parser.literals.Lexical.{whitespace, wsChars, newline}
import parser.ExpressionParser.{expression}
import parser.expressions.VariableParser.variable

/**
  * Created by tobias on 27.05.17.
  */
object Literals {

  val nonDigitSeq = ('a' to 'z') ++ ('A' to 'Z') ++ ('\u0080' to '\u00ff') :+ '_'
  val nonDigit = P(CharIn(nonDigitSeq).!)

  val nameWithKeyword : P[Name] = P(nonDigit ~ (nonDigit | digit).rep).map(t => Name(t._1 + t._2.mkString))
  val name : P[Name] = P(!keyword ~ nameWithKeyword)

  val keyword = P(StringInIgnoreCase(allKeywords:_*) ~ !nonDigit)

  val variableName : P[SimpleNameVar] = P("$" ~ nameWithKeyword).map(SimpleNameVar)

  val stringLiteral : P[StringLiteral] = P(sqStringLiteral | dqStringLiteral | hdStringLiteral | ndStringLiteral)

  val digit = P(CharIn("0123456789").!)
  val nonZeroDigit = P(CharIn("123456789").!)
  val octalDigit = P(CharIn("01234567").!)
  val hexadecimalDigit = P(CharIn("0123456789ABCDEFabcdef").!)
  val binaryDigit = P(CharIn("01").!)

  val decimalLiteral = P(nonZeroDigit ~ digit.rep).map(t => DecimalLiteral(t._1 + t._2.mkString))
  val octalLiteral = P("0" ~ octalDigit.rep).map(t => OctalLiteral(t.mkString))
  val hexadecimalLiteral = P(("0x" | "0X") ~ hexadecimalDigit.rep).map(t => HexadecimalLiteral(t.mkString))
  val binaryLiteral = P(("0b" | "0B")  ~ binaryDigit.rep).map(t => BinaryLiteral(t.mkString))

  val integerLiteral : P[IntegerLiteral] = P(decimalLiteral | binaryLiteral | hexadecimalLiteral | octalLiteral)

  val exponentPart : P[(Boolean, String)] = P(("e" | "E") ~ ("+".!.map(_ => true) | "-".!.map(_ => false)).? ~ digit.rep(1)).map(t => (if(t._1.isDefined) t._1.get else true, t._2.mkString))
  val floatingLiteral : P[FloatingLiteral] = P(("." ~ digit.rep(1) ~ exponentPart.?).map(t => FloatingLiteral("", t._1.mkString, t._2)) |
    (digit.rep(1) ~ (exponentPart.map(e => (Some(e), "")) | ("." ~ digit.rep ~ exponentPart.?).map(t => (t._2, t._1.mkString)))).map(t => FloatingLiteral(t._1.mkString, t._2._2, t._2._1)))

  val assignmentOp = P(StringIn("**", "*", "/", "+", "-", ".", "<<", ">>", "&", "^", "|").!)
  val equalityOp = P(StringIn("===", "==", "!==", "!=", "<>").!)
  val relationalOp = P(StringIn("<=>", "<=", ">=", "<", ">").!)
  val unaryOp = P(CharIn("+-!~").!)


  val octalStringElement = P("\\" ~ octalDigit.rep(min = 1, max = 3)).map(t => OctalStringElement(t.map(_(0))))
  val hexStringElement = P("\\" ~ IgnoreCase("x") ~ hexadecimalDigit.rep(min = 1, max = 2)).map(t => HexStringElement(t.map(_(0))))
  val unicodeStringElement = P("\\u{" ~ (
    hexadecimalDigit.rep(min = 1).map(t => Left(t.map(_(0)))) |
      variable.map(Right(_)) ~ whitespace
    ) ~ "}").map(UnicodeStringElement)
  val varStringElement = P(variableName ~ ((
    "->" ~ name).map(PropertyStringVarAcc) | ("[" ~ (
    name.map(NameOffsetStringVarAcc) |
      variableName.map(VarOffsetStringVarAcc) |
      integerLiteral.map(IntOffsetStringVarAcc)
    ) ~ "]")).?).map(t => VarStringElement(t._1, t._2))
  val expressionStringElement = P("${" ~ expression ~ "}").map(ExpressionStringElement)


  val sqEscapeSequence = P("\\".! ~ AnyChar.!).map(t => t._1 + t._2)
  val sqUnescapedSequence = P(CharsWhile(!"\\'".contains(_)).!)
  val sqCharSequence = P((sqEscapeSequence | sqUnescapedSequence).rep).map(_.mkString)
  val sqStringLiteral = P(CharIn("bB").!.? ~ "'" ~ sqCharSequence ~ "'").map(t => SQStringLiteral(t._1, t._2))

  val dqNormalEscapeSequence = P("\\".! ~ !(CharIn("xX01234567") | "u{") ~ AnyChar.!).map(t => t._1 + t._2)
  val dqUnescapedSequence = P(CharsWhile(!"\\\"$".contains(_)).!)
  val dQStringElement = P((dqNormalEscapeSequence | dqUnescapedSequence).rep(1)).map(t => DQStringElement(t.mkString))
  val dqCharSequence = P((dQStringElement | octalStringElement | hexStringElement | unicodeStringElement | varStringElement | expressionStringElement).rep)
  val dqStringLiteral = P(CharIn("bB").!.? ~ "\"" ~ dqCharSequence ~ "\"").map(t => DQStringLiteral(t._1, t._2))

  val hdNormalEscapeSequence = P("\\".! ~ !(CharIn("xX01234567") | "u{") ~ AnyChar.!).map(t => t._1 + t._2)
  val hdUnescapedSequence = P(CharsWhile(!"\\\n\r$".contains(_)).!)
  val hdStringElement = P((hdNormalEscapeSequence | hdUnescapedSequence).rep(1)).map(t => HDStringElement(t.mkString))
  val hdCharSequence = P((hdStringElement | octalStringElement | hexStringElement | unicodeStringElement | varStringElement | expressionStringElement).rep)
  def hdRest(identifier: Name) : P[(Name, Seq[StringElement])] = P(hdCharSequence ~
    (newline ~ !(identifier.name ~ ";".? ~ newline) ~ hdCharSequence).rep ~
    newline ~ identifier.name ~ &(";".? ~ newline)
  ).map(t => (identifier, t._2.foldLeft(t._1)(_ ++ Seq(HDNewLine) ++ _)))
  val hdStringLiteral = P(CharIn("bB").!.? ~ whitespace ~ "<<<" ~ wsChars.rep ~
    ((("\"" ~ name ~ "\"") | name) ~ wsChars.rep ~ newline)
      .flatMap(hdRest))
    .map(t => HeredocStringLiteral(t._1, t._2._1, t._2._2))

  val ndNormalEscapeSequence = P("\\".! ~ AnyChar.!).map(t => t._1 + t._2)
  val ndUnescapedSequence = P(CharsWhile(!"\\\n\r".contains(_)).!)
  val ndStringElement = P((ndNormalEscapeSequence | ndUnescapedSequence).rep(1)).map(t => NDStringElement(t.mkString))
  def ndRest(identifier: Name) : P[(Name, Seq[StringElement])] = P(ndStringElement ~
    (newline ~ !(identifier.name ~ ";".? ~ newline) ~ ndStringElement).rep ~
    newline ~ identifier.name ~ &(";".? ~ newline)
  ).map(t => (identifier, t._2.foldLeft(Seq[StringElement](t._1))(_ ++ Seq(NDNewLine, _))))
  val ndStringLiteral = P(CharIn("bB").!.? ~ whitespace ~ "<<<" ~ wsChars.rep ~
    ("\'" ~ name ~ "\'" ~ wsChars.rep ~ newline)
      .flatMap(ndRest))
    .map(t => HeredocStringLiteral(t._1, t._2._1, t._2._2))

  val literal : P[Literal] = P(integerLiteral | floatingLiteral | stringLiteral)
}
