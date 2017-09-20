package de.thm.ii.phpparser.parser.literals

import de.thm.ii.phpparser.ast.{Basic => BAst}
import de.thm.ii.phpparser.ast.Expressions.SimpleNameVar

import fastparse.all._

import de.thm.ii.phpparser.parser.literals.Keywords.AllKeywords
import de.thm.ii.phpparser.parser.literals.Lexical.{Whitespace, WsChars, Newline}
import de.thm.ii.phpparser.parser.expressions.ExpressionParser.{Expression}
import de.thm.ii.phpparser.parser.expressions.VariableParser.Variable

/**
  * Created by tobias on 27.05.17.
  */
object Literals {

  val NonDigitSeq = ('a' to 'z') ++ ('A' to 'Z') ++ ('\u0080' to '\u00ff') :+ '_'
  val NonDigit = P(CharIn(NonDigitSeq).!)

  val NameWithKeyword : P[BAst.Name] = P(NonDigit ~ (NonDigit | Digit).rep).map(t => BAst.Name(t._1 + t._2.mkString))
  val Name : P[BAst.Name] = P(!Keyword ~ NameWithKeyword)

  val Keyword = P(StringInIgnoreCase(AllKeywords:_*) ~ !(NonDigit | Digit))

  val VariableName : P[SimpleNameVar] = P("$" ~ NameWithKeyword).map(SimpleNameVar)

  val StringLiteral : P[BAst.StringLiteral] = P(SqStringLiteral | DqStringLiteral | HdStringLiteral | NdStringLiteral)

  val Digit = P(CharIn("0123456789").!)
  val NonZeroDigit = P(CharIn("123456789").!)
  val OctalDigit = P(CharIn("01234567").!)
  val HexadecimalDigit = P(CharIn("0123456789ABCDEFabcdef").!)
  val BinaryDigit = P(CharIn("01").!)

  val DecimalLiteral = P(NonZeroDigit ~ Digit.rep).map(t => BAst.DecimalLiteral(t._1 + t._2.mkString))
  val OctalLiteral = P("0" ~ OctalDigit.rep).map(t => BAst.OctalLiteral(t.mkString))
  val HexadecimalLiteral = P(("0x" | "0X") ~ HexadecimalDigit.rep).map(t => BAst.HexadecimalLiteral(t.mkString))
  val BinaryLiteral = P(("0b" | "0B")  ~ BinaryDigit.rep).map(t => BAst.BinaryLiteral(t.mkString))

  val IntegerLiteral : P[BAst.IntegerLiteral] = P(DecimalLiteral | BinaryLiteral | HexadecimalLiteral | OctalLiteral)

  val ExponentPart : P[(Boolean, String)] = P(("e" | "E") ~ ("+".!.map(_ => true) | "-".!.map(_ => false)).? ~ Digit.rep(1)).map(t => (if(t._1.isDefined) t._1.get else true, t._2.mkString))
  val FloatingLiteral : P[BAst.FloatingLiteral] = P(("." ~ Digit.rep(1) ~ ExponentPart.?).map(t => BAst.FloatingLiteral("", t._1.mkString, t._2)) |
    (Digit.rep(1) ~ (ExponentPart.map(e => (Some(e), "")) | ("." ~ Digit.rep ~ ExponentPart.?).map(t => (t._2, t._1.mkString)))).map(t => BAst.FloatingLiteral(t._1.mkString, t._2._2, t._2._1)))

  val AssignmentOp = P(StringIn("**", "*", "/", "+", "-", ".", "<<", ">>", "&", "^", "|", "%").!)
  val EqualityOp = P(StringIn("===", "==", "!==", "!=", "<>").!)
  val RelationalOp = P(StringIn("<=>", "<=", ">=", "<", ">").!)
  val UnaryOp = P(CharIn("+-!~").!)


  val OctalStringElement = P("\\" ~ OctalDigit.rep(min = 1, max = 3)).map(t => BAst.OctalStringElement(t.map(_(0))))
  val HexStringElement = P("\\" ~ IgnoreCase("x") ~ (HexadecimalDigit.rep(min = 1, max = 2) | "{" ~ HexadecimalDigit.rep ~"}")).map(t => BAst.HexStringElement(t.map(_(0))))
  val UnicodeStringElement = P("\\u{" ~ (
    HexadecimalDigit.rep(min = 1).map(t => Left(t.map(_(0)))) |
      Variable.map(Right(_)) ~ Whitespace
    ) ~ "}").map(BAst.UnicodeStringElement)
  val VarStringElement = P(VariableName ~ ((
    "->" ~ Name).map(BAst.PropertyStringVarAcc) | ("[" ~ (
    Name.map(BAst.NameOffsetStringVarAcc) |
      VariableName.map(BAst.VarOffsetStringVarAcc) |
      IntegerLiteral.map(BAst.IntOffsetStringVarAcc)
    ) ~ "]")).?).map(t => BAst.VarStringElement(t._1, t._2))
  val ExpressionStringElement = P("${" ~ Expression ~ "}").map(BAst.ExpressionStringElement)


  val SqEscapeSequence = P("\\".! ~ AnyChar.!).map(t => t._1 + t._2)
  val SqUnescapedSequence = P(CharsWhile(!"\\'".contains(_)).!)
  val SqCharSequence = P((SqEscapeSequence | SqUnescapedSequence).rep).map(_.mkString)
  val SqStringLiteral = P(CharIn("bB").!.? ~ "'" ~/ SqCharSequence ~ "'").map(t => BAst.SQStringLiteral(t._1, t._2))

  val DqNormalEscapeSequence = P((("\\".! ~ !(CharIn("xX01234567") | "u{")) | &("$" ~ !NonDigit)) ~ AnyChar.!).map(t => t._1 + t._2)
  val DqUnescapedSequence = P(CharsWhile(!"\\\"$".contains(_)).!)
  val DqStringElement = P((DqNormalEscapeSequence | DqUnescapedSequence).rep(1)).map(t => BAst.DQStringElement(t.mkString))
  val DqCharSequence = P((DqStringElement | OctalStringElement | HexStringElement | UnicodeStringElement | VarStringElement | ExpressionStringElement).rep)
  val DqStringLiteral = P(CharIn("bB").!.? ~ "\"" ~/ DqCharSequence ~ "\"").map(t => BAst.DQStringLiteral(t._1, t._2))

  val DqCommandUnescapedSequence = P(CharsWhile(!"\\`$".contains(_)).!)
  val DqCommandStringElement = P((DqNormalEscapeSequence | DqCommandUnescapedSequence).rep(1)).map(t => BAst.DQStringElement(t.mkString))
  val DqCommandCharSequence = P((DqCommandStringElement | OctalStringElement | HexStringElement | UnicodeStringElement | VarStringElement | ExpressionStringElement).rep)

  val HdUnescapedSequence = P(CharsWhile(!"\\\n\r$".contains(_)).!)
  val HdStringElement = P((DqNormalEscapeSequence | HdUnescapedSequence).rep(1)).map(t => BAst.HDStringElement(t.mkString))
  val HdCharSequence = P((HdStringElement | OctalStringElement | HexStringElement | UnicodeStringElement | VarStringElement | ExpressionStringElement).rep)
  def HdRest(identifier: BAst.Name) : P[(BAst.Name, Seq[BAst.StringElement])] = P(HdCharSequence ~
    (Newline ~ !(identifier.name ~ ";".? ~ Newline) ~ HdCharSequence).rep ~
    Newline ~ identifier.name ~ &(";".? ~ Newline)
  ).map(t => (identifier, t._2.foldLeft(t._1)(_ ++ Seq(BAst.HDNewLine) ++ _)))
  val HdStringLiteral = P(CharIn("bB").!.? ~ Whitespace ~ "<<<" ~ WsChars.rep ~
    ((("\"" ~/ Name ~ "\"") | Name) ~/ WsChars.rep ~ Newline)
      .flatMap(HdRest))
    .map(t => BAst.HeredocStringLiteral(t._1, t._2._1, t._2._2))

  val NdNormalEscapeSequence = P("\\".! ~ AnyChar.!).map(t => t._1 + t._2)
  val NdUnescapedSequence = P(CharsWhile(!"\\\n\r".contains(_)).!)
  val NdStringElement = P((NdNormalEscapeSequence | NdUnescapedSequence).rep(1)).map(t => BAst.NDStringElement(t.mkString))
  def NdRest(identifier: BAst.Name) : P[(BAst.Name, Seq[BAst.StringElement])] = P(NdStringElement ~
    (Newline ~ !(identifier.name ~ ";".? ~ Newline) ~ NdStringElement).rep ~
    Newline ~ identifier.name ~ &(";".? ~ Newline)
  ).map(t => (identifier, t._2.foldLeft(Seq[BAst.StringElement](t._1))(_ ++ Seq(BAst.NDNewLine, _))))
  val NdStringLiteral = P(CharIn("bB").!.? ~ Whitespace ~ "<<<" ~ WsChars.rep ~
    ("\'" ~/ Name ~ "\'" ~ WsChars.rep ~ Newline)
      .flatMap(NdRest))
    .map(t => BAst.HeredocStringLiteral(t._1, t._2._1, t._2._2))

  val Literal : P[BAst.Literal] = P(FloatingLiteral | IntegerLiteral | StringLiteral)
}
