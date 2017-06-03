package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.ws

import ast.Basic.Text
import ast.Expressions.Expression
import ast.Statements._

import parser.literals.Keywords._

import parser.Basic.{semicolonFactory, wsExp, semicolon}
import parser.literals.Literals.{integerLiteral, name}
import parser.expressions.ExpressionParser.expression
import parser.statements.StatementParser.{emptyStmnt, statement, statements}

/**
  * This object contains all statements, which manipulate the sequential control flow.
  */
object ControlFlowParser {

  // selection statements

  private val ifBody : P[(Seq[Statement], Seq[(Expression, Seq[Statement])], Option[Seq[Statement]], Option[Text])] = P((
    ":" ~/ statements ~
      (ELSEIF ~ "(" ~/ expression ~ ")" ~/ ":" ~/ statements).rep ~
      (ELSE ~ ":" ~/ statements).? ~ ENDIF ~/ semicolonFactory) |
    (statement ~/
      (ELSEIF ~/ "(" ~ expression ~ ")" ~/ statement).rep ~
      (ELSE ~~ &(wsExp | semicolon | "{") ~/ statement).?)
      .map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)), None))
  )

  val ifStmnt : P[IfStmnt] =
    P(IF ~ "(" ~/ expression ~ ")" ~/ ifBody)
      .map(t => IfStmnt(t._1, t._2._1, t._2._2, t._2._3, t._2._4))


  private val caseBlock : P[CaseBlock] =
    P(CASE ~~ &(wsExp) ~/ expression ~ ((":" ~/ statements) | statement.rep(1)))
      .map(t => CaseBlock(t._1, t._2))

  private val defaultBlock : P[DefaultBlock] =
    P(DEFAULT ~ (":" ~/ statements | statement.rep(1))).map(DefaultBlock)

  private val switchBody : P[(Seq[SwitchBlock], Option[Text])] =
    P((":" ~/ (caseBlock | defaultBlock).rep ~ ENDSWITCH ~ semicolonFactory) |
      ("{" ~/ (caseBlock | defaultBlock).rep ~ "}").map(t => (t, None)))

  val switchStmnt : P[SwitchStmnt] =
    P(SWITCH ~ "(" ~/ expression ~ ")" ~/ switchBody)
      .map(t => SwitchStmnt(t._1, t._2._1, t._2._2))


  val selectionStmnt : P[SelectionStmnt] = P(ifStmnt | switchStmnt)


  // iteration statements

  private val whileBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statements ~ ENDWHILE ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  val whileStmnt : P[WhileStmnt] =
    P(WHILE ~ "(" ~/ expression ~ ")" ~/ whileBody)
      .map(t => WhileStmnt(t._1, t._2._1, t._2._2))


  val doStmnt : P[DoStmnt] =
    P(DO ~~ &(wsExp | semicolon) ~/ statement ~/ WHILE ~/ "(" ~/ expression ~ ")" ~/ semicolonFactory)
      .map(t => DoStmnt(t._2, t._1, t._3))


  private val forBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statements ~ ENDFOR ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  private val forExpressionList : P[ForExpressionList] =
    P(expression.rep(sep=",".~/) ~ semicolonFactory)
      .map(t => ForExpressionList(t._1, t._2))

  val forStmnt : P[ForStmnt] =
    P(FOR ~ "(" ~/ forExpressionList ~/ forExpressionList ~/ expression.rep(sep=",".~/) ~ ")" ~/ forBody)
      .map(t => ForStmnt(t._1, t._2, ForExpressionList(t._3, None), t._4._1, t._4._2))


  private val foreachBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statements ~ ENDFOREACH ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  val foreachStmnt : P[ForeachStmnt] =
    P(FOREACH ~ "(" ~/ expression ~~ &(ws) ~/ AS ~~ wsExp ~/ ((
      "&" ~ expression).map(t => (None, true, t)) |
      (expression ~ ("=>" ~ "&".!.? ~ expression).?)
        .map(t => if(t._2.isDefined) (Some(t._1), t._2.get._1.isDefined, t._2.get._2) else (None, false, t._1))
      ) ~ ")" ~/ foreachBody
    ).map(t => ForeachStmnt(t._1, t._2._1, t._2._2, t._2._3, t._3._1, t._3._2))


  val iterationStmnt : P[IterationStmnt] = P(whileStmnt | doStmnt | foreachStmnt | forStmnt)


  // jump statements

  val jumpStmnt : P[JumpStmnt] = P((GOTO ~/ name ~ semicolonFactory).map(t => GotoStmnt(t._1, t._2)) |
    (CONTINUE ~~ &(ws | semicolon) ~/ integerLiteral.? ~ semicolonFactory).map(t => ContinueStmnt(t._1, t._2)) |
    (BREAK ~~ &(ws | semicolon) ~/ integerLiteral.? ~ semicolonFactory).map(t => BreakStmnt(t._1, t._2)) |
    (RETURN ~~ &(wsExp | semicolon) ~/ expression.? ~ semicolonFactory).map(t => ReturnStmnt(t._1, t._2)) |
    (THROW ~~ &(wsExp) ~/ expression ~ semicolonFactory).map(t => ThrowStmnt(t._1, t._2)))
}
