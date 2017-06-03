package parser.expressions

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.ws

import ast.Expressions._

import parser.literals.Keywords._
import parser.literals.Literals._

import parser.Basic.wsExp
import parser.statements.StatementParser.{compoundStmnt, anonymousFuncHeader}
import parser.expressions.OperatorParser.logicalOrExpr2
import parser.expressions.VariableParser.{arrayElement, variable}


object ExpressionParser {

  val expression : P[Expression] = P(yieldExp | requireOnceExp | requireExp | includeOnceExp | includeExp | logicalOrExpr2)

  val requireExp : P[RequireExp] = P(REQUIRE ~~ &(wsExp) ~/ expression).map(RequireExp)
  val requireOnceExp : P[RequireOnceExp] = P(REQUIRE_ONCE ~~ &(wsExp) ~/ expression).map(RequireOnceExp)
  val includeExp : P[IncludeExp] = P(INCLUDE ~~ &(wsExp) ~/ expression).map(IncludeExp)
  val includeOnceExp : P[IncludeOnceExp] = P(INCLUDE_ONCE ~~ &(wsExp) ~/ expression).map(IncludeOnceExp)
  val yieldExp : P[Expression] = P(
    (YIELD ~~ ws ~ FROM ~~ &(wsExp) ~/ expression).map(YieldFromExp)
      | (YIELD ~~ &(wsExp) ~/ arrayElement).map(YieldExp))

  val cloneExp : P[Expression] = P(CLONE ~~ &(wsExp) ~/ expression).map(CloneExp)

  val primaryExpWithoutVariable : P[Expression] = P(constAccExp |
    literal | intrinsic | anonymousFuncExp)

  //unnecessary
  val constAccExp : P[Expression] = P(Fail)

  val listIntrinsic : P[ListIntrinsic] = P(
    LIST ~ "(" ~/ (
      (",".rep ~ NoCut(expression).rep(sep=",".rep(1))).map(Left(_))
        | ((expression ~ "=>" ~ expression).rep(sep=",") ~ ",".?).map(Right(_)))
      ~ ")").map(ListIntrinsic)
  val echoIntrinsic : P[EchoIntrinsic] = P(ECHO ~~ &(wsExp) ~/ expression.rep(min=1, sep=",")).map(EchoIntrinsic)
  val unsetIntrinsic : P[UnsetIntrinsic] = P(UNSET ~ "(" ~ variable.rep(1, sep=",") ~ ")").map(UnsetIntrinsic)

  val emptyIntrinsic : P[EmptyIntrinsic] = P(EMPTY ~ "(" ~/ expression ~ ")").map(EmptyIntrinsic)
  val evalIntrinsic : P[EvalIntrinsic] = P(EVAL ~ "(" ~/ expression ~ ")").map(EvalIntrinsic)
  val exitIntrinsic : P[ExitIntrinsic] = P(EXIT ~ ("(" ~/ expression.? ~ ")").? |
    (DIE ~ ("(" ~/ expression.? ~ ")").?)).map(t => ExitIntrinsic(t.getOrElse(None)))
  val issetIntrinsic : P[IssetIntrinsic] = P(ISSET ~ "(" ~/ variable.rep(1, sep=",") ~ ")").map(IssetIntrinsic)
  val printIntrinsic : P[PrintIntrinsic] = P(PRINT ~~ &(wsExp) ~/ expression).map(PrintIntrinsic)

  val intrinsicConstruct : P[Expression] = P(echoIntrinsic | unsetIntrinsic | listIntrinsic)
  val intrinsicOperator : P[Expression] = P(emptyIntrinsic | evalIntrinsic | exitIntrinsic | issetIntrinsic | printIntrinsic)
  val intrinsic : P[Expression] = P(intrinsicOperator | intrinsicConstruct)

  val anonymousFuncExp : P[Expression] = P(STATIC.!.?.map(_.isDefined) ~ anonymousFuncHeader ~ compoundStmnt)
    .map(t => AnonymousFunctionCreationExp(t._1, t._2._1, t._2._2, t._3))

  val singleExpression : P[Expression] = P(yieldExp | requireOnceExp | requireExp | includeOnceExp | includeExp)

  //unused part

  //val expression : P[Expression] = P(exp1).map(_ => SpecialExp())

  def exp1 : P[Expression] = P((randomOutside.? ~ "(" ~ exp2.? ~ ")" ~ exp1.?).map(_ => SpecialExp()) |
    (randomOutside.? ~ "{" ~ exp2.? ~ "}" ~  exp1.?) .map(_ => SpecialExp()) |
    (randomOutside.? ~ "[" ~ exp2.? ~ "]" ~ exp1.?) .map(_ => SpecialExp()) |
    (randomOutside ~ (":".rep(1) ~ exp1).?) .map(_ => SpecialExp()))

  def exp2 : P[Expression] = P((random.? ~ "(" ~ exp2.? ~ ")" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "{" ~ exp2.? ~ "}" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "[" ~ exp2.? ~ "]" ~ exp2.?) .map(_ => SpecialExp()) |
    (random ~ (":".rep(1) ~ exp2).?) .map(_ => SpecialExp()))

  //val random1 : P[Expression] = """[^\(\)\{\}\[\]:]+""".r ^^^ SpecialExp()

  //val randomOutside1 : P[Expression] = """[^\(\)\{\}\[\]:,;]+""".r ^^^ SpecialExp()

  def charSeqInside = "(){}[]:"
  def charSeqOutside = "(){}[]:,;"

  def random = P(CharsWhile(charSeqInside.indexOf(_) == -1, 1))
  def randomOutside = P(CharsWhile(charSeqOutside.indexOf(_) == -1, 1))

  //end unused part
}
