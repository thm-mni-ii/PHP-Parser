package parser

import fastparse.noApi._
import WsAPI._
import ast.Ast._
import StatementParser._

object PHPParser {

  def parser = scriptTag ~ statement.rep ~ End

  //def script : P[Script] = P((scriptSection | textSection).rep)

  //def scriptSection : P[ScriptSection] =

  val scriptTag : Parser[Boolean] = "<?php".!.map(_ => false) | "<?=".!.map(_ => true)

  val textSection : Parser[Text] = P((!scriptTag ~ AnyChar.!).rep).map(t => Text(t.mkString))

  def parse(toParse: String) = parser.parse(toParse)

  val nonDigit = P(CharIn(('a' to 'z') ++ ('A' to 'Z') ++ ('\u0080' to '\u00ff') :+ '_').!)
  val name : P[Name] = P(nonDigit ~~ (nonDigit | digit).repX).map(t => Name(t._1 + t._2.mkString))

  val variableName : P[SimpleNameVar] = P("$" ~~ name).map(SimpleNameVar)

  def qualifiedName : P[QualifiedName] = P(("namespace" ~ "\\" ~ (name ~ "\\").rep ~ name).map(t => QualifiedName(NamespaceType.LOCAL, t._1, t._2)) |
    ("\\".!.? ~ (name ~ "\\").rep ~ name).map(t =>
      if(t._1.isDefined) QualifiedName(NamespaceType.GLOBAL, t._2, t._3)
      else QualifiedName(NamespaceType.RELATIVE, t._2, t._3)
    ))

  def possibleType : P[PossibleTypes] = P("void").map(_ => VoidType) | typeDecl

  def arrayType = P("array").map(_ => ArrayType)
  def callableType = P("callable").map(_ => CallableType)
  def iterableType = P("iterable").map(_ => IterableType)
  def boolType = P("bool").map(_ => BoolType)
  def floatType = P("float").map(_ => FloatType)
  def intType = P("int").map(_ => IntType)
  def stringType = P("string").map(_ => StringType)

  def typeDecl : P[TypeDecl] = arrayType | callableType |iterableType | boolType | floatType | intType | stringType | qualifiedName.map(QualifiedType)

  //def expression : P[Expression] = exp1.map(_ => SpecialExp())

  def charSeqInside = "(){}[]:"
  def charSeqOutside = "(){}[]:,;"

  def random = P(CharsWhile(charSeqInside.indexOf(_) == -1, 1))
  def randomOutside = P(CharsWhile(charSeqOutside.indexOf(_) == -1, 1))

  val assignmentOp = P(StringIn("**", "*", "/", "+", "-", ".", "<<", ">>", "&", "^", "|").!)
  val equalityOp = P(StringIn("===", "==", "!==", "!=", "<>").!).log()
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

  val stringLiteral : P[StringLiteral] = P(sqStringLiteral | dqStringLiteral).log()

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

  //val random1 : P[Expression] = """[^\(\)\{\}\[\]:]+""".r ^^^ SpecialExp()

  //val randomOutside1 : P[Expression] = """[^\(\)\{\}\[\]:,;]+""".r ^^^ SpecialExp()

  def exp1 : P[Expression] = P((randomOutside.? ~ "(" ~ exp2.? ~ ")" ~ exp1.?).map(_ => SpecialExp()) |
    (randomOutside.? ~ "{" ~ exp2.? ~ "}" ~  exp1.?) .map(_ => SpecialExp()) |
    (randomOutside.? ~ "[" ~ exp2.? ~ "]" ~ exp1.?) .map(_ => SpecialExp()) |
    (randomOutside ~ (":".rep(1) ~ exp1).?) .map(_ => SpecialExp()))

  def exp2 : P[Expression] = P((random.? ~ "(" ~ exp2.? ~ ")" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "{" ~ exp2.? ~ "}" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "[" ~ exp2.? ~ "]" ~ exp2.?) .map(_ => SpecialExp()) |
    (random ~ (":".rep(1) ~ exp2).?) .map(_ => SpecialExp()))
}
