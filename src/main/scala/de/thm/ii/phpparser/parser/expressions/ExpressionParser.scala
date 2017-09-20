package de.thm.ii.phpparser.parser.expressions

import fastparse.noApi._
import de.thm.ii.phpparser.parser.literals.WsAPI._
import de.thm.ii.phpparser.parser.literals.Lexical.Ws

import de.thm.ii.phpparser.ast.{Expressions => EAst}

import de.thm.ii.phpparser.parser.literals.Keywords._
import de.thm.ii.phpparser.parser.literals.Literals._

import de.thm.ii.phpparser.parser.Basic.WsExp
import de.thm.ii.phpparser.parser.statements.StatementParser.{CompoundStmnt, AnonymousFuncHeader}
import de.thm.ii.phpparser.parser.expressions.OperatorParser.{LogicalOrExpr2, CondExp}
import de.thm.ii.phpparser.parser.expressions.VariableParser.{ArrayElement, Variable}


object ExpressionParser {

  val Expression : P[EAst.Expression] = P(YieldExp | RequireOnceExp | RequireExp | IncludeOnceExp | IncludeExp | LogicalOrExpr2)

  val RequireExp = P(REQUIRE ~~ &(WsExp) ~/ Expression).map(EAst.RequireExp)
  val RequireOnceExp = P(REQUIRE_ONCE ~~ &(WsExp) ~/ Expression).map(EAst.RequireOnceExp)
  val IncludeExp = P(INCLUDE ~~ &(WsExp) ~/ Expression).map(EAst.IncludeExp)
  val IncludeOnceExp = P(INCLUDE_ONCE ~~ &(WsExp) ~/ Expression).map(EAst.IncludeOnceExp)

  val YieldExp : P[EAst.Expression] = P(
    (YIELD ~~ Ws ~ FROM ~~ &(WsExp) ~/ Expression).map(EAst.YieldFromExp)
      | (YIELD ~~ &(WsExp) ~/ ArrayElement).map(EAst.YieldExp))

  val CloneExp = P(CLONE ~~ &(WsExp) ~/ Expression).map(EAst.CloneExp)

  val PrimaryExpWithoutVariable : P[EAst.Expression] =
    P(Literal | Intrinsic | AnonymousFuncExp)

  val ListIntrinsic = P(
    LIST ~ "(" ~/ (
      (",".rep ~ NoCut(Expression).rep(sep=",".rep(1)) ~ ",".rep).map(Left(_))
        | ((Expression ~ "=>" ~ Expression).rep(sep=",") ~ ",".?).map(Right(_)))
      ~ ")").map(EAst.ListIntrinsic)
  val EchoIntrinsic = P(ECHO ~~ &(WsExp) ~/ Expression.rep(min=1, sep=",")).map(EAst.EchoIntrinsic)
  val UnsetIntrinsic = P(UNSET ~ "(" ~ Variable.rep(1, sep=",") ~ ")").map(EAst.UnsetIntrinsic)

  val EmptyIntrinsic = P(EMPTY ~ "(" ~/ Expression ~ ")").map(EAst.EmptyIntrinsic)
  val EvalIntrinsic = P(EVAL ~ "(" ~/ Expression ~ ")").map(EAst.EvalIntrinsic)
  val ExitIntrinsic = P((EXIT|DIE) ~ ("(" ~/ Expression.? ~ ")").?).map(t => EAst.ExitIntrinsic(t.getOrElse(None)))
  val IssetIntrinsic = P(ISSET ~ "(" ~/ Variable.rep(1, sep=",") ~ ")").map(EAst.IssetIntrinsic)
  val PrintIntrinsic = P(PRINT ~~ &(WsExp) ~/ Expression).map(EAst.PrintIntrinsic)

  val IntrinsicConstruct : P[EAst.Intrinsic] = P(EchoIntrinsic | UnsetIntrinsic | ListIntrinsic)
  val IntrinsicOperator : P[EAst.Intrinsic] = P(EmptyIntrinsic | EvalIntrinsic | ExitIntrinsic | IssetIntrinsic | PrintIntrinsic)
  val Intrinsic : P[EAst.Intrinsic] = P(IntrinsicOperator | IntrinsicConstruct)

  val AnonymousFuncExp = P(STATIC.!.?.map(_.isDefined) ~ AnonymousFuncHeader ~ CompoundStmnt)
    .map(t => EAst.AnonymousFunctionCreationExp(t._1, t._2._1, t._2._2, t._3))

  val SingleExpression : P[EAst.Expression] = P(YieldExp | RequireOnceExp | RequireExp | IncludeOnceExp | IncludeExp)

  val ArgumentExpressionList : P[Seq[EAst.ArgumentExpression]] = {
    val ArgumentExp = P(
      "...".!.? ~ (CondExp | SingleExpression)).map(t => EAst.ArgumentExpression(t._1.isDefined, t._2))

    P(ArgumentExp.rep(sep=","))
  }
}
