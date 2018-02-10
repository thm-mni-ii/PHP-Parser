package de.thm.mni.ii.phpparser.parser.statements

import fastparse.noApi._
import de.thm.mni.ii.phpparser.parser.literals.WsAPI._
import de.thm.mni.ii.phpparser.parser.literals.Lexical.Ws

import de.thm.mni.ii.phpparser.ast.{Basic => BAst, Expressions => EAst, Statements => SAst}

import de.thm.mni.ii.phpparser.parser.literals.Keywords._

import de.thm.mni.ii.phpparser.parser.Basic.{SemicolonFactory, WsExp, Semicolon}
import de.thm.mni.ii.phpparser.parser.literals.Literals.{IntegerLiteral, Name}
import de.thm.mni.ii.phpparser.parser.expressions.ExpressionParser.Expression
import de.thm.mni.ii.phpparser.parser.statements.StatementParser.{Statement, Statements, wrap}

/**
  * This object contains all statements, which manipulate the sequential control flow.
  */
object ControlFlowParser {

  // selection statements

  val IfStmnt = {
    val IfBody: P[(Seq[de.thm.mni.ii.phpparser.ast.Statements.Statement], Seq[(EAst.Expression, Seq[SAst.Statement])], Option[Seq[SAst.Statement]], Option[BAst.Text])] = P((
      ":" ~/ Statements
        ~ (ELSEIF ~ "(" ~/ Expression ~ ")" ~/ ":" ~/ Statements).rep
        ~ (ELSE ~ ":" ~/ Statements).? ~ ENDIF ~/ SemicolonFactory) |
      (Statement
        ~/ (ELSEIF ~/ "(" ~ Expression ~ ")" ~/ Statement).rep
        ~ (ELSE ~~ &(WsExp | Semicolon | "{") ~/ Statement).?
        ).map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)), None))
    )

    P(IF ~ "(" ~/ Expression ~ ")" ~/ IfBody).map {
      case (exp, (stmnts, elseifs, elseStmnts, text)) => wrap(SAst.IfStmnt(exp, stmnts, elseifs, elseStmnts), text)
    }
  }

  val SwitchStmnt = {
    val CaseBlock = P(CASE ~~ &(WsExp) ~/ Expression ~ ((":" ~/ Statements) | Statement.rep(1)))
      .map(t => SAst.CaseBlock(t._1, t._2))

    val DefaultBlock = P(DEFAULT ~ (":" ~/ Statements | Statement.rep(1)))
      .map(SAst.DefaultBlock)

    val SwitchBody: P[(Seq[SAst.SwitchBlock], Option[BAst.Text])] = P(
      (":" ~/ (CaseBlock | DefaultBlock).rep ~ ENDSWITCH ~ SemicolonFactory)
        | ("{" ~/ (CaseBlock | DefaultBlock).rep ~ "}").map(t => (t, None)))

    P(SWITCH ~ "(" ~/ Expression ~ ")" ~/ SwitchBody).map {
      case (exp, (blocks, text)) => wrap(SAst.SwitchStmnt(exp, blocks), text)
    }
  }

  val SelectionStmnt: P[SAst.Statement] = P(IfStmnt | SwitchStmnt)


  // iteration statements

  val WhileStmnt = {
    val WhileBody: P[(Seq[SAst.Statement], Option[BAst.Text])] = P(
      (":" ~/ Statements ~ ENDWHILE ~ SemicolonFactory)
        | Statement.map(t => (Seq(t), None))
    )

    P(WHILE ~ "(" ~/ Expression ~ ")" ~/ WhileBody).map {
      case (exp, (stmnt, text)) => wrap(SAst.WhileStmnt(exp, stmnt), text)
    }
  }

  val DoStmnt =
    P(DO ~~ &(WsExp | Semicolon | "{") ~/ Statement ~/ WHILE ~/ "(" ~/ Expression ~ ")" ~/ SemicolonFactory).map {
      case (stmnt, exp, text) => wrap(SAst.DoStmnt(exp, stmnt), text)
    }

  val ForStmnt = {
    val ForBody: P[(Seq[SAst.Statement], Option[BAst.Text])] = P(
      (":" ~/ Statements ~ ENDFOR ~ SemicolonFactory)
        | Statement.map(t => (Seq(t), None))
    )

    val ForExpressionList = P(Expression.rep(sep = ",".~/) ~ SemicolonFactory)
      .map(t => SAst.ForExpressionList(t._1, t._2))

    P(FOR ~ "(" ~/ ForExpressionList ~/ ForExpressionList ~/ Expression.rep(sep = ",".~/) ~ ")" ~/ ForBody).map {
      case (init, control, exps, (body, text)) => wrap(SAst.ForStmnt(init, control, SAst.ForExpressionList(exps, None), body), text)
    }
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
    ).map {
      case (exp, (key, valueDesVar, value), (body, text)) => wrap(SAst.ForeachStmnt(exp, key, valueDesVar, value, body), text)
    }
  }

  val IterationStmnt: P[SAst.Statement] = P(WhileStmnt | DoStmnt | ForeachStmnt | ForStmnt)


  // jump statements

  val JumpStmnt: P[SAst.Statement] = P(
    (GOTO ~~ Ws ~/ Name ~ SemicolonFactory).map(t => wrap(SAst.GotoStmnt(t._1), t._2))
      | (CONTINUE ~~ &(Ws | Semicolon) ~/ IntegerLiteral.? ~ SemicolonFactory).map(t => wrap(SAst.ContinueStmnt(t._1), t._2))
      | (BREAK ~~ &(Ws | Semicolon) ~/ IntegerLiteral.? ~ SemicolonFactory).map(t => wrap(SAst.BreakStmnt(t._1), t._2))
      | (RETURN ~~ &(WsExp | Semicolon) ~/ Expression.? ~ SemicolonFactory).map(t => wrap(SAst.ReturnStmnt(t._1), t._2))
      | (THROW ~~ &(WsExp) ~/ Expression ~ SemicolonFactory).map(t => wrap(SAst.ThrowStmnt(t._1), t._2)))
}
