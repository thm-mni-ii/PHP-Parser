package parser.expressions

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Expressions => EAst}
import ast.Statements.ClassDecl

import parser.literals.Keywords._
import parser.literals.KeywordConversions._
import parser.literals.Literals._

import parser.Basic._
import parser.statements.DeclarationParser.ClassDeclBody
import parser.expressions.VariableParser.Variable
import parser.expressions.ExpressionParser.{Expression, ArgumentExpressionList, ListIntrinsic, PrimaryExpWithoutVariable, CloneExp, SingleExpression}

/**
  * Created by tobias on 02.06.17.
  */
object OperatorParser {

  val LogicalOrExpr2 : P[EAst.Expression] = P(LogicalXOrExp.rep(sep=OR.~/, min=1)).map(_.reduceLeft(EAst.LogicalOrExp2))
  val LogicalXOrExp : P[EAst.Expression] = P(LogicalAndExp2.rep(sep=XOR.~/, min=1)).map(_.reduceLeft(EAst.LogicalXOrExp))
  val LogicalAndExp2 : P[EAst.Expression] = P(CondExp.rep(sep=AND.~/, min=1)).map(_.reduceLeft(EAst.LogicalAndExp2))

  val CondExp : P[EAst.Expression] = {
    val ConditionalExpFactory : P[EAst.Expression => EAst.Expression] = P(
      ("??" ~/ Expression).map(e => (x: EAst.Expression) => EAst.CoalesceExp(x,e))
        | ("?" ~~ !">" ~/ Expression.? ~ ":" ~/ CondExp).map(e => (x) => EAst.TernaryExp(x,e._1, e._2)))

    P((LogicalOrExp ~ ConditionalExpFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))
  }

  val LogicalOrExp: P[EAst.Expression] = P(LogicalAndExp.rep(sep="||".~/, min=1)).map(_.reduceLeft(EAst.LogicalOrExp))
  val LogicalAndExp: P[EAst.Expression] = P(BitwiseOrExp.rep(sep="&&".~/, min=1)).map(_.reduceLeft(EAst.LogicalAndExp))
  val BitwiseOrExp: P[EAst.Expression] = P(BitwiseXOrExp.rep(sep=("|" ~~ !"|").~/, min=1)).map(_.reduceLeft(EAst.BitwiseOrExp))
  val BitwiseXOrExp: P[EAst.Expression] = P(BitwiseAndExp.rep(sep="^".~/, min=1)).map(_.reduceLeft(EAst.BitwiseXOrExp))
  val BitwiseAndExp: P[EAst.Expression] = P(EqualityExp.rep(sep=("&" ~~ !"&").~/, min=1)).map(_.reduceLeft(EAst.BitwiseAndExp))

  val EqualityExp: P[EAst.Expression] = P(RelationalExp ~ (EqualityOp ~/ RelationalExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => EAst.EqualityExp(op._1, exp, op._2)))
  val RelationalExp: P[EAst.Expression] = P(ShiftExp ~ (RelationalOp ~ ShiftExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => EAst.RelationalExp(op._1, exp, op._2)))

  val ShiftExp: P[EAst.Expression] = {
    val ShiftFactory : P[EAst.Expression => EAst.Expression] = P(
      ("<<" ~~ !"=" ~~ !"<" ~/ AdditiveExp).map(e => (x: EAst.Expression) => EAst.LShiftExp(x, e))
        | (">>" ~~ !"=" ~/ AdditiveExp).map(e => (x: EAst.Expression) => EAst.RShiftExp(x, e)))

    P(AdditiveExp ~ ShiftFactory.rep)
      .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  }

  val AdditiveExp: P[EAst.Expression] = {
    val AdditiveFactory : P[EAst.Expression => EAst.Expression] = P(
      ("+" ~/ MultExp).map(e => (x: EAst.Expression) => EAst.AddExp(x, e))
        | ("-" ~/ MultExp).map(e => (x: EAst.Expression) => EAst.SubExp(x, e))
        | ("." ~/ MultExp).map(e => (x) => EAst.SubExp(x, e)))

    P(MultExp ~ AdditiveFactory.rep)
      .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  }

  val MultExp: P[EAst.Expression] = {
    val MultFactory : P[EAst.Expression => EAst.Expression] = P(
      ("*" ~/ ExponentiationExp).map(e => (x: EAst.Expression) => EAst.MulExp(x, e))
        | ("/" ~/ ExponentiationExp).map(e => (x: EAst.Expression) => EAst.DivExp(x, e))
        | ("%" ~/ ExponentiationExp).map(e => (x) => EAst.ModExp(x, e)))

    P(ExponentiationExp ~ MultFactory.rep)
      .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  }

  val ExponentiationExp: P[EAst.Expression] = P(InstanceOfExp ~ ("**" ~/ Expression).?)
    .map(t => if(t._2.isDefined) EAst.ExponentiationExp(t._1, t._2.get) else t._1)

  val InstanceOfExp : P[EAst.Expression] = P(UnaryExp ~~
    (Ws ~ INSTANCEOF ~~ &(WsExp) ~/ (QualifiedName.map(Right(_)) | Expression.map(Left(_)))).?
  ).map(t => if(t._2.isDefined) EAst.InstanceOfExp(t._1, t._2.get) else t._1)

  val PrefixIncrementExp : P[EAst.Expression] = P("++" ~/ Variable).map(EAst.PrefixIncrementExp)
  val PrefixDecrementExp : P[EAst.Expression] = P("--" ~/ Variable).map(EAst.PrefixDecrementExp)
  val UnaryOpExp : P[EAst.Expression] = P(UnaryOp ~ (SingleExpression | UnaryExp)).map(t => EAst.UnaryOpExp(t._1,t._2))
  val ErrorControlExp : P[EAst.Expression] = P("@" ~/ Expression).map(EAst.ErrorControlExp)
  val ShellCommandExp : P[EAst.Expression] = P("`" ~~ DqCommandCharSequence ~~ "`").map(EAst.ShellCommandExp)

  val CastExp : P[EAst.Expression] = {
    val CastType : P[EAst.CastType.Value] = P(ArrayCastType | BinaryCastType | BooleanCastType | BoolCastType |
      DoubleCastType | IntegerCastType | IntCastType | FloatCastType | ObjectCastType |
      RealCastType | StringCastType | UnsetCastType)

    P("(" ~ CastType ~ ")" ~/ Expression).map(t => EAst.CastExp(t._1, t._2))
  }

  val UnaryExp : P[EAst.Expression] = P(
    PrefixIncrementExp | PrefixDecrementExp | UnaryOpExp
      | ErrorControlExp | ShellCommandExp | CastExp | PostfixExp)

  val ListAssignment : P[EAst.Expression] = P(NoCut(ListIntrinsic) ~ "=" ~/ (CondExp | SingleExpression))
    .map(t => EAst.ListAssignmentExp(t._1, t._2))

  val ObjectCreationExp : P[EAst.Expression] = P(
    NEW ~~ &(WsExp) ~/ ((
      CLASS ~~ &("(" | "{" | Ws) ~/ ("(" ~/ ArgumentExpressionList ~ ")").? ~ ClassDeclBody)
        .map(t => EAst.AnonymousClassCreationExp(ClassDecl(None, None, t._2._1, t._2._2, t._2._3), t._1))
      | (Expression ~ ("(" ~/ ArgumentExpressionList ~ ")").?)
        .map(t => EAst.InstanceCreationExp(t._1, t._2)))
  )

  val PostfixExp : P[EAst.Expression] = {
    val PostfixOperatorFactory : P[EAst.Variable => EAst.Expression] = P(
      "++".!.map(_ => (x: EAst.Variable) => EAst.PostfixIncrementExp(x))
        | "--".!.map(_ => (x: EAst.Variable) => EAst.PostfixDecrementExp(x))
        | "::" ~ NameWithKeyword.map(n => (x: EAst.Variable) => EAst.ClassConstAcc(x, n))
        | ("=" ~ "&".!.? ~ (CondExp | SingleExpression)).map(e => (x: EAst.Variable) => EAst.SimpleAssignmentExp(e._1.isDefined, x, e._2))
        | (AssignmentOp ~~ "=" ~/ (CondExp | SingleExpression)).map(e => (x: EAst.Variable) => EAst.CompoundAssignmentExp(e._1, x, e._2)))

    P(ListAssignment | PrimaryExpWithoutVariable | CloneExp | ObjectCreationExp
      | (Variable ~/ PostfixOperatorFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))
  }
}
