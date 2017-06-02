package parser.expressions

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.ws

import ast.Expressions._
import ast.Statements.ClassDecl

import parser.literals.Keywords._
import parser.literals.KeywordConversions._
import parser.literals.Literals._

import parser.Basic._
import parser.statements.DeclarationParser.classDeclBody
import parser.expressions.VariableParser.variable
import parser.expressions.ExpressionParser.{expression, listIntrinsic, primaryExpWithoutVariable, cloneExp, singleExpression}

/**
  * Created by tobias on 02.06.17.
  */
object OperatorParser {

  val space = P(&(ws | "("))

  val logicalOrExpr2 : P[Expression] = P(logicalXOrExp.rep(sep=OR.~/, min=1)).map(_.reduceLeft(LogicalOrExp2))
  val logicalXOrExp : P[Expression] = P(logicalAndExp2.rep(sep=XOR.~/, min=1)).map(_.reduceLeft(LogicalXOrExp))
  val logicalAndExp2 : P[Expression] = P(condExp.rep(sep=AND.~/, min=1)).map(_.reduceLeft(LogicalAndExp2))

  val conditionalExpFactory : P[Expression => Expression] = P(
    ("??" ~/ expression).map(e => (x: Expression) => CoalesceExp(x,e))
      | ("?" ~~ !">" ~/ expression.? ~ ":" ~/ condExp).map(e => (x) => TernaryExp(x,e._1, e._2)))
  val condExp : P[Expression] = P((logicalOrExp ~ conditionalExpFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))

  val logicalOrExp: P[Expression] = P(logicalAndExp.rep(sep="||".~/, min=1)).map(_.reduceLeft(LogicalOrExp))
  val logicalAndExp: P[Expression] = P(bitwiseOrExp.rep(sep="&&".~/, min=1)).map(_.reduceLeft(LogicalAndExp))
  val bitwiseOrExp: P[Expression] = P(bitwiseXOrExp.rep(sep=("|" ~~ !"|").~/, min=1)).map(_.reduceLeft(BitwiseOrExp))
  val bitwiseXOrExp: P[Expression] = P(bitwiseAndExp.rep(sep="^".~/, min=1)).map(_.reduceLeft(BitwiseXOrExp))
  val bitwiseAndExp: P[Expression] = P(equalityExp.rep(sep=("&" ~~ !"&").~/, min=1)).map(_.reduceLeft(BitwiseAndExp))

  val equalityExp: P[Expression] = P(relationalExp ~ (equalityOp ~/ relationalExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => EqualityExp(op._1, exp, op._2)))
  val relationalExp: P[Expression] = P(shiftExp ~ (relationalOp ~ shiftExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => RelationalExp(op._1, exp, op._2)))

  val shiftFactory : P[Expression => Expression] = P(
    ("<<" ~~ !"=" ~~ !"<" ~/ additiveExp).map(e => (x: Expression) => LShiftExp(x, e))
      | (">>" ~~ !"=" ~/ additiveExp).map(e => (x: Expression) => RShiftExp(x, e)))
  val shiftExp: P[Expression] = P(additiveExp ~ shiftFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))

  val additiveFactory : P[Expression => Expression] = P(
    ("+" ~/ multExp).map(e => (x: Expression) => AddExp(x, e))
      | ("-" ~/ multExp).map(e => (x: Expression) => SubExp(x, e))
      | ("." ~/ multExp).map(e => (x) => SubExp(x, e)))
  val additiveExp: P[Expression] = P(multExp ~ additiveFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))

  val multFactory : P[Expression => Expression] = P(
    ("*" ~/ exponentiationExp).map(e => (x: Expression) => MulExp(x, e))
      | ("/" ~/ exponentiationExp).map(e => (x: Expression) => DivExp(x, e))
      | ("%" ~/ exponentiationExp).map(e => (x) => ModExp(x, e)))
  val multExp: P[Expression] = P(exponentiationExp ~ multFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))

  val exponentiationExp: P[Expression] = P(instanceOfExp ~ ("**" ~/ expression).?)
    .map(t => if(t._2.isDefined) ExponentiationExp(t._1, t._2.get) else t._1)

  val instanceOfExp : P[Expression] = P(unaryExp ~~
    (ws ~ INSTANCEOF ~~ space ~/ (qualifiedName.map(Right(_)) | expression.map(Left(_)))).?
  ).map(t => if(t._2.isDefined) InstanceOfExp(t._1, t._2.get) else t._1)

  val prefixIncrementExp : P[Expression] = P("++" ~/ variable).map(PrefixIncrementExp)
  val prefixDecrementExp : P[Expression] = P("--" ~/ variable).map(PrefixDecrementExp)
  val unaryOpExp : P[Expression] = P(unaryOp ~ unaryExp).map(t => UnaryOpExp(t._1,t._2))
  val errorControlExp : P[Expression] = P("@" ~/ expression).map(ErrorControlExp)
  val shellCommandExp : P[Expression] = P("`" ~~ dqCharSequence ~~ "`").map(ShellCommandExp)

  val castType : P[CastType.Value] = P(arrayCastType | binaryCastType | booleanCastType | boolCastType |
    doubleCastType | integerCastType | intCastType | floatCastType | objectCastType |
    realCastType | stringCastType | unsetCastType)
  val castExp : P[Expression] = P("(" ~ castType ~ ")" ~/ expression).map(t => CastExp(t._1, t._2))

  val unaryExp : P[Expression] = P(
    prefixIncrementExp | prefixDecrementExp | unaryOpExp
      | errorControlExp | shellCommandExp | castExp | postfixExp)

  val listAssignment : P[Expression] = P(NoCut(listIntrinsic) ~ "=" ~/ condExp).map(t => ListAssignmentExp(t._1, t._2))
  val assignmentFactory : P[Variable => Expression] = P(
    ("=" ~ "&".!.? ~ condExp).map(e => (x: Variable) => SimpleAssignmentExp(e._1.isDefined, x, e._2))
      | (assignmentOp ~~ "=" ~/ condExp).map(e => (x: Variable) => CompoundAssignmentExp(e._1, x, e._2)))

  val postfixOperatorFactory : P[Variable => Expression] = P(
    "++".!.map(_ => (x: Variable) => PostfixIncrementExp(x))
      | "--".!.map(_ => (x: Variable) => PostfixDecrementExp(x))
      | "::" ~ name.map(n => (x: Variable) => ClassConstAcc(x, n))
      | assignmentFactory)

  val objectCreationExp : P[Expression] = P(
    NEW ~~ ws ~/ ((
      CLASS ~~ &("(" | "{" | ws) ~/ ("(" ~/ argumentExpressionList ~ ")").? ~ classDeclBody)
        .map(t => AnonymousClassCreationExp(ClassDecl(None, None, t._2._1, t._2._2, t._2._3), t._1))
      | ((qualifiedName.map(Left(_)) | expression.map(Right(_))) ~ ("(" ~/ argumentExpressionList ~ ")").?)
        .map(t => InstanceCreationExp(t._1, t._2)))
  )

  val argumentExpressionList : P[Seq[ArgumentExpression]] = P(
    argumentExp.rep(sep=","))

  val argumentExp: P[ArgumentExpression] = P(
    "...".!.? ~ condExp).map(t => ArgumentExpression(t._1.isDefined, t._2))

  val postfixExp : P[Expression] = P(listAssignment | primaryExpWithoutVariable | cloneExp | objectCreationExp |
    (variable ~/ postfixOperatorFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))
}
