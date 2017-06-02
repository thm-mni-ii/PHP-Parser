package parser.expressions

import ast.Expressions._
import ast.Statements.ClassDecl
import parser.Basic._
import parser.literals.Keywords._
import ExpressionParser.expression
import parser.expressions.OperatorParser.{assignmentCondExp, argumentExpressionList}
import parser.literals.Lexical.ws
import parser.literals.KeywordConversions._
import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Literals._
import parser.statements.DeclarationParser.classDeclBody

/**
  * Created by tobias on 02.06.17.
  */
object VariableParser {

  val variableName : P[SimpleNameVar] = P(
    "$" ~ nameWithKeyword).map(SimpleNameVar)

  val simpleVariable : P[SimpleVar] = P(
    ("$" ~ simpleVariable).map(SimpleAccessVar)
      | ("$" ~ "{" ~/ expression ~ "}").map(SimpleExpVar)
      | variableName)

  val scopeAccVar : P[ScopeAccessVar] = P(
    selfScope | parentScope | staticScope).map(ScopeAccessVar)

  val qualifiedNameVar : P[QualifiedNameVar] = P(
    qualifiedName).map(QualifiedNameVar)

  val memberName : P[MemberName] = P(
    nameWithKeyword.map(NameMember)
      | simpleVariable.map(SimpleVarMember)
      | ("{" ~/ expression ~ "}").map(ExpMember))

  val stringLiteralVar : P[Variable] = P(
    stringLiteral).map(StringLiteralVar)

  val expressionVar : P[ExpressionVar] = P(
    "(" ~ expression ~ ")").map(ExpressionVar)

  val arrayElement : P[ArrayElement] = P(
    ("&" ~ NoCut(expression)).map(ArrayElement(None, _, true))
      | (expression ~ ("=>" ~ "&".!.? ~ expression).?).map(t =>
        if(t._2.isDefined) ArrayElement(Some(t._1), t._2.get._2, t._2.get._1.isDefined)
        else ArrayElement(None, t._1, false))
  )

  val arrayCreationVar : P[ArrayCreationVar] = P(
    ARRAY ~ "(" ~/ !"," ~ arrayElement.rep(sep=("," ~ !")").~/) ~ ",".? ~ ")"
      | "[" ~/ !"," ~ arrayElement.rep(sep=("," ~ !"]").~/) ~ ",".? ~ "]"
  ).map(ArrayCreationVar)

  val memberCallStaticAccFactory : P[Variable => Variable] = P(
    "::" ~ memberName ~ "(" ~/ argumentExpressionList ~ ")"
  ).map(t => (x: Variable) => MemberCallStaticAcc(x, t._1, t._2))

  val simpleVarStaticAccFactory : P[Variable => Variable] = P(
    "::" ~ simpleVariable).map(t => (x: Variable) => SimpleVarStaticAcc(x, t))

  val arrayAccFactory : P[Variable => Variable] = P(
    "[" ~/ expression.? ~ "]").map(t => (x: Variable) => ArrayAcc(x, t))

  val blockAccFactory : P[Variable => Variable] = P(
    "{" ~/ expression ~ "}").map(t => (x: Variable) => BlockAcc(x, t))

  val memberPropertyAccFactory : P[Variable => Variable] = P(
    "->" ~/ memberName ~ ("(" ~/ argumentExpressionList ~ ")").?).map(t =>
      if(t._2.isDefined) (x) => MemberCallPropertyAcc(x, t._1, t._2.get)
      else (x) => MemberPropertyAcc(x, t._1)
  )

  val directCallAccFactory : P[Variable => Variable] = P(
    "(" ~/ argumentExpressionList ~ ")").map(t => (x: Variable) => CallAccessor(x, t))

  val qualifiedNameVarWithCall : P[Variable] = P(
    qualifiedNameVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)

  val expressionVarWithCall : P[Variable] = P(
    expressionVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)

  val singleVariable : P[Variable] = P(
    simpleVariable | arrayCreationVar | stringLiteralVar
      | scopeAccVar | qualifiedNameVarWithCall | expressionVarWithCall)

  val variable : P[Variable] = P(
    singleVariable
      ~ (memberCallStaticAccFactory | simpleVarStaticAccFactory | arrayAccFactory | blockAccFactory | memberPropertyAccFactory).rep
  ).map(t => t._2.foldLeft(t._1)((a,b) => b(a)))

}
