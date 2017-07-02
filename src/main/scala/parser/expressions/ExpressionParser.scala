package parser.expressions

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Expressions => EAst}

import parser.literals.Keywords._
import parser.literals.Literals._

import parser.Basic.WsExp
import parser.statements.StatementParser.{CompoundStmnt, AnonymousFuncHeader}
import parser.expressions.OperatorParser.{LogicalOrExpr2, CondExp}
import parser.expressions.VariableParser.{ArrayElement, Variable}


object ExpressionParser {

  val Expression : P[EAst.Expression] = P(YieldExp | RequireOnceExp | RequireExp | IncludeOnceExp | IncludeExp | LogicalOrExpr2)

  val RequireExp : P[EAst.RequireExp] = P(REQUIRE ~~ &(WsExp) ~/ Expression).map(EAst.RequireExp)
  val RequireOnceExp : P[EAst.RequireOnceExp] = P(REQUIRE_ONCE ~~ &(WsExp) ~/ Expression).map(EAst.RequireOnceExp)
  val IncludeExp : P[EAst.IncludeExp] = P(INCLUDE ~~ &(WsExp) ~/ Expression).map(EAst.IncludeExp)
  val IncludeOnceExp : P[EAst.IncludeOnceExp] = P(INCLUDE_ONCE ~~ &(WsExp) ~/ Expression).map(EAst.IncludeOnceExp)
  val YieldExp : P[EAst.Expression] = P(
    (YIELD ~~ Ws ~ FROM ~~ &(WsExp) ~/ Expression).map(EAst.YieldFromExp)
      | (YIELD ~~ &(WsExp) ~/ ArrayElement).map(EAst.YieldExp))

  val CloneExp : P[EAst.Expression] = P(CLONE ~~ &(WsExp) ~/ Expression).map(EAst.CloneExp)

  val PrimaryExpWithoutVariable : P[EAst.Expression] = P(ConstAccExp |
    Literal | Intrinsic | AnonymousFuncExp)

  //unnecessary
  val ConstAccExp : P[EAst.Expression] = P(Fail)

  val ListIntrinsic : P[EAst.ListIntrinsic] = P(
    LIST ~ "(" ~/ (
      (",".rep ~ NoCut(Expression).rep(sep=",".rep(1))).map(Left(_))
        | ((Expression ~ "=>" ~ Expression).rep(sep=",") ~ ",".?).map(Right(_)))
      ~ ")").map(EAst.ListIntrinsic)
  val EchoIntrinsic : P[EAst.EchoIntrinsic] = P(ECHO ~~ &(WsExp) ~/ Expression.rep(min=1, sep=",")).map(EAst.EchoIntrinsic)
  val UnsetIntrinsic : P[EAst.UnsetIntrinsic] = P(UNSET ~ "(" ~ Variable.rep(1, sep=",") ~ ")").map(EAst.UnsetIntrinsic)

  val EmptyIntrinsic : P[EAst.EmptyIntrinsic] = P(EMPTY ~ "(" ~/ Expression ~ ")").map(EAst.EmptyIntrinsic)
  val EvalIntrinsic : P[EAst.EvalIntrinsic] = P(EVAL ~ "(" ~/ Expression ~ ")").map(EAst.EvalIntrinsic)
  val ExitIntrinsic : P[EAst.ExitIntrinsic] = P(EXIT ~ ("(" ~/ Expression.? ~ ")").? |
    (DIE ~ ("(" ~/ Expression.? ~ ")").?)).map(t => EAst.ExitIntrinsic(t.getOrElse(None)))
  val IssetIntrinsic : P[EAst.IssetIntrinsic] = P(ISSET ~ "(" ~/ Variable.rep(1, sep=",") ~ ")").map(EAst.IssetIntrinsic)
  val PrintIntrinsic : P[EAst.PrintIntrinsic] = P(PRINT ~~ &(WsExp) ~/ Expression).map(EAst.PrintIntrinsic)

  val IntrinsicConstruct : P[EAst.Expression] = P(EchoIntrinsic | UnsetIntrinsic | ListIntrinsic)
  val IntrinsicOperator : P[EAst.Expression] = P(EmptyIntrinsic | EvalIntrinsic | ExitIntrinsic | IssetIntrinsic | PrintIntrinsic)
  val Intrinsic : P[EAst.Expression] = P(IntrinsicOperator | IntrinsicConstruct)

  val AnonymousFuncExp : P[EAst.Expression] = P(STATIC.!.?.map(_.isDefined) ~ AnonymousFuncHeader ~ CompoundStmnt)
    .map(t => EAst.AnonymousFunctionCreationExp(t._1, t._2._1, t._2._2, t._3))

  val SingleExpression : P[EAst.Expression] = P(YieldExp | RequireOnceExp | RequireExp | IncludeOnceExp | IncludeExp)

  val ArgumentExpressionList : P[Seq[EAst.ArgumentExpression]] = {
    val ArgumentExp: P[EAst.ArgumentExpression] = P(
      "...".!.? ~ CondExp).map(t => EAst.ArgumentExpression(t._1.isDefined, t._2))

    P(ArgumentExp.rep(sep=","))
  }

  //unused part

  //val Expression : P[Expression] = P(exp1).map(_ => SpecialExp())

  def exp1 : P[EAst.Expression] = P((randomOutside.? ~ "(" ~ exp2.? ~ ")" ~ exp1.?).map(_ => EAst.SpecialExp()) |
    (randomOutside.? ~ "{" ~ exp2.? ~ "}" ~  exp1.?) .map(_ => EAst.SpecialExp()) |
    (randomOutside.? ~ "[" ~ exp2.? ~ "]" ~ exp1.?) .map(_ => EAst.SpecialExp()) |
    (randomOutside ~ (":".rep(1) ~ exp1).?) .map(_ => EAst.SpecialExp()))

  def exp2 : P[EAst.Expression] = P((random.? ~ "(" ~ exp2.? ~ ")" ~ exp2.?) .map(_ => EAst.SpecialExp()) |
    (random.? ~ "{" ~ exp2.? ~ "}" ~ exp2.?) .map(_ => EAst.SpecialExp()) |
    (random.? ~ "[" ~ exp2.? ~ "]" ~ exp2.?) .map(_ => EAst.SpecialExp()) |
    (random ~ (":".rep(1) ~ exp2).?) .map(_ => EAst.SpecialExp()))

  //val random1 : P[Expression] = """[^\(\)\{\}\[\]:]+""".r ^^^ SpecialExp()

  //val randomOutside1 : P[Expression] = """[^\(\)\{\}\[\]:,;]+""".r ^^^ SpecialExp()

  def charSeqInside = "(){}[]:"
  def charSeqOutside = "(){}[]:,;"

  def random = P(CharsWhile(charSeqInside.indexOf(_) == -1, 1))
  def randomOutside = P(CharsWhile(charSeqOutside.indexOf(_) == -1, 1))

  //end unused part
}
