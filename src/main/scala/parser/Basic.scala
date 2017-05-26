package parser

import ast.Basic._
import ast.Expressions.{ScopeType, SimpleNameVar}
import ast.Statements._

import parser.StatementParser.statement
import parser.PHPParser._

import fastparse.noApi._
import parser.WsAPI._

object Basic {

  def script : P[Script] = {
    isTagProcessed = true
    P(text ~ normalStartTag.? ~ statement.rep ~ End).map(t => {
      isTagProcessed = t._2.isDefined
      Script(t._1, t._3)
    })
  }

  val text : P[Text] = P((!startTag ~ AnyChar.!).rep).map(t => Text(t.mkString))

  def semicolonFactory : P[Option[Text]] = P(";".!.map(_ => None) |
    ("?>" ~ text ~ normalStartTag.?).map(t => {
      isTagProcessed = t._2.isDefined
      Some(t._1)
    })
  )

  val normalStartTag = P("<?php".!)
  val echoStartTag = P("<?=".!)
  val endTag = P("?>")
  val startTag = P(normalStartTag | echoStartTag)

  val nonDigitSeq = ('a' to 'z') ++ ('A' to 'Z') ++ ('\u0080' to '\u00ff') :+ '_'
  val nonDigit = P(CharIn(nonDigitSeq).!)

  val nameWithKeyword : P[Name] = P(nonDigit ~~ (nonDigit | digit).repX).map(t => Name(t._1 + t._2.mkString))
  val name : P[Name] = P(!keyword ~~ nameWithKeyword)

  val keyword = P(StringIn(allKeywords:_*) ~~ !nonDigit)

  val allKeywords = Seq(
    "abstract", "and", "array", "as", "break", "callable", "case", "catch",
    "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else",
    "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile",
    "eval", "exit", "extends", "final", "finally", "for", "foreach", "function", "global", "goto",
    "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset",
    "list", "namespace", "new", "or", "print", "private", "protected", "public", "require",
    "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use",
    "var", "while", "xor", "yield")

  val variableName : P[SimpleNameVar] = P("$" ~~ name).map(SimpleNameVar)

  def qualifiedName : P[QualifiedName] = P(("namespace" ~ "\\" ~ (name ~ "\\").rep ~ name).map(t => QualifiedName(NamespaceType.LOCAL, t._1, t._2)) |
    ("\\".!.? ~ (name ~ "\\").rep ~ name).map(t =>
      if(t._1.isDefined) QualifiedName(NamespaceType.GLOBAL, t._2, t._3)
      else QualifiedName(NamespaceType.RELATIVE, t._2, t._3)
    ))

  val arrayType = P("array").map(_ => ArrayType)
  val callableType = P("callable").map(_ => CallableType)
  val iterableType = P("iterable").map(_ => IterableType)
  val boolType = P("bool").map(_ => BoolType)
  val floatType = P("float").map(_ => FloatType)
  val intType = P("int").map(_ => IntType)
  val stringType = P("string").map(_ => StringType)
  val voidType = P("void").map(_ => VoidType)

  val assignmentOp = P(StringIn("**", "*", "/", "+", "-", ".", "<<", ">>", "&", "^", "|").!)
  val equalityOp = P(StringIn("===", "==", "!==", "!=", "<>").!)
  val relationalOp = P(StringIn("<=>", "<=", ">=", "<", ">").!)
  val unaryOp = P(CharIn("+-!~").!)

  val selfScope = P("self").map(_ => ScopeType.SELF)
  val parentScope = P("parent").map(_ => ScopeType.PARENT)
  val staticScope = P("static").map(_ => ScopeType.STATIC)

  val sqEscapeSequence = P("\\".! ~~ AnyChar.!).map(t => t._1 + t._2)
  val sqUnescapedSequence = P(CharsWhile(!"\\'".contains(_)).!)
  val sqCharSequence = P((sqEscapeSequence | sqUnescapedSequence).repX).map(_.mkString)
  val sqStringLiteral = P(CharIn("bB").!.? ~ "'" ~~ sqCharSequence ~~ "'").map(t => SQStringLiteral(t._1, t._2))

  val dqEscapeSequence = P("\\".! ~~ AnyChar.!).map(t => t._1 + t._2)
  val dqUnescapedSequence = P(CharsWhile(!"\\\"".contains(_)).!)
  val dqCharSequence = P((dqEscapeSequence | dqUnescapedSequence).repX).map(_.mkString)
  val dqStringLiteral = P(CharIn("bB").!.? ~ "\"" ~~ dqCharSequence ~~ "\"").map(t => DQStringLiteral(t._1, t._2))
  //TODO dq's are wrong

  val stringLiteral : P[StringLiteral] = P(sqStringLiteral | dqStringLiteral)

  val digit = P(CharIn("0123456789").!)
  val nonZeroDigit = P(CharIn("123456789").!)
  val octalDigit = P(CharIn("01234567").!)
  val hexadecimalDigit = P(CharIn("0123456789ABCDEFabcdef").!)
  val binaryDigit = P(CharIn("01").!)

  val decimalLiteral = P(nonZeroDigit ~~ digit.repX).map(t => DecimalLiteral(t._1 + t._2.mkString))
  val octalLiteral = P("0" ~~ octalDigit.repX).map(t => OctalLiteral(t.mkString))
  val hexadecimalLiteral = P(("0x" | "0X") ~~ hexadecimalDigit.repX).map(t => HexadecimalLiteral(t.mkString))
  val binaryLiteral = P(("0b" | "0B")  ~~ binaryDigit.repX).map(t => BinaryLiteral(t.mkString))

  val integerLiteral : P[IntegerLiteral] = P(decimalLiteral | binaryLiteral | hexadecimalLiteral | octalLiteral)

  val exponentPart : P[(Boolean, String)] = P(("e" | "E") ~~ ("+".!.map(_ => true) | "-".!.map(_ => false)).? ~~ digit.repX(1)).map(t => (if(t._1.isDefined) t._1.get else true, t._2.mkString))
  val floatingLiteral : P[FloatingLiteral] = P(("." ~~ digit.repX(1) ~~ exponentPart.?).map(t => FloatingLiteral("", t._1.mkString, t._2)) |
    (digit.repX(1) ~~ (exponentPart.map(e => (Some(e), "")) | ("." ~~ digit.repX ~~ exponentPart.?).map(t => (t._2, t._1.mkString)))).map(t => FloatingLiteral(t._1.mkString, t._2._2, t._2._1)))

  val literal : P[Literal] = P(integerLiteral | floatingLiteral | stringLiteral)
}
