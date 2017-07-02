package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Basic => BAst}
import ast.{Expressions => EAst}
import ast.{Statements => SAst}

import parser.literals.Keywords._

import parser.Basic.{SemicolonFactory, WsExp, Semicolon}
import parser.literals.Literals.{IntegerLiteral, Name}
import parser.expressions.ExpressionParser.Expression
import parser.statements.StatementParser.{EmptyStmnt, Statement, Statements}

/**
  * This object contains all statements, which manipulate the sequential control flow.
  */
object ControlFlowParser {

  // selection statements

  val IfStmnt = {
    val IfBody: P[(Seq[ast.Statements.Statement], Seq[(EAst.Expression, Seq[SAst.Statement])], Option[Seq[SAst.Statement]], Option[BAst.Text])] = P((
      ":" ~/ Statements
        ~ (ELSEIF ~ "(" ~/ Expression ~ ")" ~/ ":" ~/ Statements).rep
        ~ (ELSE ~ ":" ~/ Statements).? ~ ENDIF ~/ SemicolonFactory) |
      (Statement
        ~/ (ELSEIF ~/ "(" ~ Expression ~ ")" ~/ Statement).rep
        ~ (ELSE ~~ &(WsExp | Semicolon | "{") ~/ Statement).?
        ).map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)), None))
    )

    P(IF ~ "(" ~/ Expression ~ ")" ~/ IfBody)
      .map(t => SAst.IfStmnt(t._1, t._2._1, t._2._2, t._2._3, t._2._4))
  }

  val SwitchStmnt = {
    val CaseBlock = P(CASE ~~ &(WsExp) ~/ Expression ~ ((":" ~/ Statements) | Statement.rep(1)))
      .map(t => SAst.CaseBlock(t._1, t._2))

    val DefaultBlock = P(DEFAULT ~ (":" ~/ Statements | Statement.rep(1)))
      .map(SAst.DefaultBlock)

    val SwitchBody: P[(Seq[SAst.SwitchBlock], Option[BAst.Text])] = P(
      (":" ~/ (CaseBlock | DefaultBlock).rep ~ ENDSWITCH ~ SemicolonFactory)
        | ("{" ~/ (CaseBlock | DefaultBlock).rep ~ "}").map(t => (t, None)))

    P(SWITCH ~ "(" ~/ Expression ~ ")" ~/ SwitchBody)
      .map(t => SAst.SwitchStmnt(t._1, t._2._1, t._2._2))
  }

  val SelectionStmnt: P[SAst.SelectionStmnt] = P(IfStmnt | SwitchStmnt)


  // iteration statements

  val WhileStmnt = {
    val WhileBody: P[(Seq[SAst.Statement], Option[BAst.Text])] = P(
      (":" ~/ Statements ~ ENDWHILE ~ SemicolonFactory)
        | Statement.map(t => (Seq(t), None))
    )

    P(WHILE ~ "(" ~/ Expression ~ ")" ~/ WhileBody)
      .map(t => SAst.WhileStmnt(t._1, t._2._1, t._2._2))
  }

  val DoStmnt =
    P(DO ~~ &(WsExp | Semicolon) ~/ Statement ~/ WHILE ~/ "(" ~/ Expression ~ ")" ~/ SemicolonFactory)
      .map(t => SAst.DoStmnt(t._2, t._1, t._3))

  val ForStmnt = {
    val ForBody: P[(Seq[SAst.Statement], Option[BAst.Text])] = P(
      (":" ~/ Statements ~ ENDFOR ~ SemicolonFactory)
        | Statement.map(t => (Seq(t), None))
    )

    val ForExpressionList = P(Expression.rep(sep = ",".~/) ~ SemicolonFactory)
      .map(t => SAst.ForExpressionList(t._1, t._2))

    P(FOR ~ "(" ~/ ForExpressionList ~/ ForExpressionList ~/ Expression.rep(sep = ",".~/) ~ ")" ~/ ForBody)
      .map(t => SAst.ForStmnt(t._1, t._2, SAst.ForExpressionList(t._3, None), t._4._1, t._4._2))
  }

  val ForeachStmnt = {
    val ForeachBody: P[(Seq[SAst.Statement], Option[BAst.Text])] = P(
      (":" ~/ Statements ~ ENDFOREACH ~ SemicolonFactory)
        | Statement.map(t => (Seq(t), None))
    )

    P(FOREACH ~ "(" ~/ Expression ~~ &(Ws) ~/ AS ~~ WsExp ~/ ((
      "&" ~ NoCut(Expression)).map(t => (None, true, t)) |
      (Expression ~ ("=>" ~ "&".!.? ~ Expression).?)
        .map(t => if (t._2.isDefined) (Some(t._1), t._2.get._1.isDefined, t._2.get._2) else (None, false, t._1))
      ) ~ ")" ~/ ForeachBody
    ).map(t => SAst.ForeachStmnt(t._1, t._2._1, t._2._2, t._2._3, t._3._1, t._3._2))
  }

  val IterationStmnt: P[SAst.IterationStmnt] = P(WhileStmnt | DoStmnt | ForeachStmnt | ForStmnt)


  // jump statements

  val JumpStmnt: P[SAst.JumpStmnt] = P(
    (GOTO ~/ Name ~ SemicolonFactory).map(t => SAst.GotoStmnt(t._1, t._2))
      | (CONTINUE ~~ &(Ws | Semicolon) ~/ IntegerLiteral.? ~ SemicolonFactory).map(t => SAst.ContinueStmnt(t._1, t._2))
      | (BREAK ~~ &(Ws | Semicolon) ~/ IntegerLiteral.? ~ SemicolonFactory).map(t => SAst.BreakStmnt(t._1, t._2))
      | (RETURN ~~ &(WsExp | Semicolon) ~/ Expression.? ~ SemicolonFactory).map(t => SAst.ReturnStmnt(t._1, t._2))
      | (THROW ~~ &(WsExp) ~/ Expression ~ SemicolonFactory).map(t => SAst.ThrowStmnt(t._1, t._2)))
}
