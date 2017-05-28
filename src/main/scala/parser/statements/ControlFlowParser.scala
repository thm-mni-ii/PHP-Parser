package parser.statements

import ast.Basic.Text
import ast.Expressions.Expression
import ast.Statements._
import fastparse.noApi._
import parser.literals.WsAPI._
import parser.ExpressionParser.expression
import parser.literals.Keywords._
import parser.literals.Lexical.ws
import parser.literals.Literals.integerLiteral
import parser.literals.Literals.name
import parser.statements.StatementParser.{emptyStmnt, statement}
import parser.Basic._

/**
  * Created by tobias on 27.05.17.
  */
object ControlFlowParser {

  // selection statements

  private val ifBody : P[(Seq[Statement], Seq[(Expression, Seq[Statement])], Option[Seq[Statement]], Option[Text])] = P((
    ":" ~/ statement.rep(1) ~
      (ELSEIF ~/ "(" ~ expression ~ ")" ~/ ":" ~/ statement.rep(1)).rep ~
      (ELSE ~/ ":" ~/ statement.rep).? ~ ENDIF ~/ semicolonFactory) |
    (statement ~/
      (ELSEIF ~/ "(" ~ expression ~ ")" ~/ statement).rep ~
      (ELSE ~/ statement).?)
      .map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)), None))
  )

  val ifStmnt : P[IfStmnt] =
    P(IF ~/ "(" ~ expression ~ ")" ~ ifBody)
      .map(t => IfStmnt(t._1, t._2._1, t._2._2, t._2._3, t._2._4))


  private val caseBlock : P[CaseBlock] =
    P(CASE ~/ expression ~ (":".!.map(_ => Seq()) | emptyStmnt.map(Seq(_))) ~ statement.rep)
      .map(t => CaseBlock(t._1, t._2 ++ t._3))

  private val defaultBlock : P[DefaultBlock] =
    P(DEFAULT ~/ (":".!.map(_ => Seq()) | emptyStmnt.map(Seq(_))) ~ statement.rep)
      .map(t => DefaultBlock(t._1 ++ t._2))

  private val switchBody : P[(Seq[SwitchBlock], Option[Text])] =
    P((":" ~ (caseBlock | defaultBlock).rep ~ ENDSWITCH ~ semicolonFactory) |
      ("{" ~ (caseBlock | defaultBlock).rep ~ "}").map(t => (t, None)))

  val switchStmnt : P[SwitchStmnt] =
    P(SWITCH ~/ "(" ~ expression ~ ")" ~ switchBody)
      .map(t => SwitchStmnt(t._1, t._2._1, t._2._2))


  val selectionStmnt : P[SelectionStmnt] = P(ifStmnt | switchStmnt)


  // iteration statements

  private val whileBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statement.rep ~ ENDWHILE ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  val whileStmnt : P[WhileStmnt] =
    P(WHILE ~/ "(" ~ expression ~ ")" ~ whileBody)
      .map(t => WhileStmnt(t._1, t._2._1, t._2._2))


  val doStmnt : P[DoStmnt] =
    P(DO ~/ statement ~ WHILE ~/ "(" ~ expression ~ ")" ~ semicolonFactory)
      .map(t => DoStmnt(t._2, t._1, t._3))


  private val forBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statement.rep ~ ENDFOR ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  private val forExpressionList : P[ForExpressionList] =
    P(expression.rep(sep=",") ~ semicolonFactory)
      .map(t => ForExpressionList(t._1, t._2))

  val forStmnt : P[ForStmnt] =
    P(FOR ~/ "(" ~ forExpressionList ~ forExpressionList ~ expression.rep(sep=",") ~ ")" ~ forBody)
      .map(t => ForStmnt(t._1, t._2, ForExpressionList(t._3, None), t._4._1, t._4._2))


  private val foreachBody : P[(Seq[Statement], Option[Text])] = P((
    ":" ~/ statement.rep ~ ENDFOREACH ~ semicolonFactory) |
    statement.map(t => (Seq(t), None))
  )

  val foreachStmnt : P[ForeachStmnt] =
    P(FOREACH ~/ "(" ~ expression ~ AS ~ ((
      "&" ~ expression).map(t => (None, true, t)) |
      (expression ~ ("=>" ~ "&".!.? ~ expression).?)
        .map(t => if(t._2.isDefined) (Some(t._1), t._2.get._1.isDefined, t._2.get._2) else (None, false, t._1))
      ) ~ ")" ~/ foreachBody
    ).map(t => ForeachStmnt(t._1, t._2._1, t._2._2, t._2._3, t._3._1, t._3._2))


  val iterationStmnt : P[IterationStmnt] = P(whileStmnt | doStmnt | foreachStmnt | forStmnt)


  // jump statements

  val jumpStmnt : P[JumpStmnt] = P((GOTO ~/ name ~ semicolonFactory).map(t => GotoStmnt(t._1, t._2)) |
    (CONTINUE ~~ &(";" | "?>" | ws) ~/ integerLiteral.? ~ semicolonFactory).map(t => ContinueStmnt(t._1, t._2)) |
    (BREAK ~~ &(";" | "?>" | ws) ~/ integerLiteral.? ~ semicolonFactory).map(t => BreakStmnt(t._1, t._2)) |
    (RETURN ~~ &(";" | "?>" | ws) ~/ expression.? ~ semicolonFactory).map(t => ReturnStmnt(t._1, t._2)) |
    (THROW ~~ &(ws) ~/ expression ~ semicolonFactory).map(t => ThrowStmnt(t._1, t._2)))
}
