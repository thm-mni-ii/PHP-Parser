package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Basic => BAst, Expressions => EAst, Statements => SAst}

import parser.literals.KeywordConversions._
import parser.literals.Keywords._
import parser.literals.Literals._

import parser.PHPParser.isTagProcessed
import parser.Basic.{EchoStartTag, QualifiedName, SemicolonFactory}
import parser.expressions.ExpressionParser.Expression
import parser.statements.ControlFlowParser._
import parser.statements.DeclarationParser._

/**
  * This object contains all basic statements
  */
object StatementParser {

  def Statement: P[SAst.Statement] = if (isTagProcessed) PossibleStatements else EchoTagStmnt

  val Statements: P[Seq[SAst.Statement]] = P(Statement.rep)

  def EchoTagStmnt: P[SAst.EchoTagStmnt] = {
    isTagProcessed = true
    P(EchoStartTag ~ Expression.rep(min = 1, sep = ",") ~ SemicolonFactory).map(t => SAst.EchoTagStmnt(t._2, t._3))
  }

  val EmptyStmnt = P(SemicolonFactory).map(SAst.EmptyStmnt)
  val CompoundStmnt = P("{" ~/ Statements ~ "}").map(SAst.CompoundStmnt)
  val NamedLabelStmnt = P(Name ~ ":" ~ Statement).map(t => SAst.NamedLabelStmnt(t._1, t._2))
  val ExpStmnt = P(Expression ~ SemicolonFactory).map(t => SAst.ExpressionStmnt(t._1, t._2))

  val TryStmnt = {
    val CatchClause = P(CATCH ~ "(" ~/ QualifiedName ~ VariableName ~ ")" ~/ CompoundStmnt)
      .map(t => SAst.CatchClause(t._1, t._2, t._3))

    P(TRY ~ &("{") ~/ CompoundStmnt ~/ CatchClause.rep() ~ (FINALLY ~ &("{") ~/ CompoundStmnt).?)
      .map(t => SAst.TryStmnt(t._1, t._2, t._3))
  }

  val DeclareStmnt = {
    val DeclareDeclarative: P[SAst.DeclareDeclarative.Value] =
      P(TicksDeclarative | EncodingDeclarative | StrictTypesDeclarative)

    val DeclareBody: P[(Seq[SAst.Statement], Option[BAst.Text])] =
      P(":" ~/ Statements ~ ENDDECLARE ~ SemicolonFactory |
        Statement.map(t => (Seq(t), None)))

    P(DECLARE ~ "(" ~/ DeclareDeclarative ~ "=" ~ Literal ~ ")" ~/ DeclareBody)
      .map(t => SAst.DeclareStmnt(t._1, t._2, t._3._1, t._3._2))
  }


  val TypeDecl: P[SAst.TypeDecl] = P(ArrayType | CallableType | IterableType |
    BoolType | FloatType | IntType | StringType | QualifiedName.map(SAst.QualifiedType))
  val PossibleFunctionType: P[SAst.PossibleTypes] = P(VoidType | TypeDecl)

  private val ParamType: P[(Option[SAst.TypeDecl], Boolean)] = P(TypeDecl.? ~ "&".!.?)
    .map(t => (t._1, t._2.isDefined))
  private val ParameterDecl: P[(Option[SAst.TypeDecl], Boolean) => SAst.ParameterDecl] =
    P(("..." ~ VariableName).map(name => (a: Option[SAst.TypeDecl], b: Boolean) => SAst.VariadicParam(a, b, name))
      | (VariableName ~ ("=" ~ Expression).?).map(t => (a: Option[SAst.TypeDecl], b: Boolean) => SAst.SimpleParam(a, b, t._1, t._2)))

  private[statements] val FuncHeader: P[SAst.FuncHeader] =
    P(FUNCTION ~~ &(Ws) ~ "&".!.? ~ Name ~/ "(" ~/ (ParamType ~ ParameterDecl).rep(sep = ",".~/) ~ ")" ~/ (":" ~/ PossibleFunctionType).?)
      .map(t => SAst.FuncHeader(t._1.isDefined, Some(t._2), t._3.map(g => g._3(g._1, g._2)), t._4))

  private[parser] val AnonymousFuncHeader: P[(SAst.FuncHeader, Seq[(Boolean, EAst.SimpleNameVar)])] = P(
    FUNCTION ~~ &(Ws | "(") ~ "&".!.?
      ~ "(" ~/ (ParamType ~ ParameterDecl).rep(sep = ",".~/) ~ ")"
      ~/ (USE ~ "(" ~ ("&".!.?.map(_.isDefined) ~ VariableName).rep(1, sep = ",") ~ ")").?
      ~ (":" ~/ PossibleFunctionType).?)
    .map(t => (SAst.FuncHeader(t._1.isDefined, None, t._2.map(g => g._3(g._1, g._2)), t._4), t._3.getOrElse(Seq())))

  val FunctionDefStmnt = P(FuncHeader ~ &("{") ~/ CompoundStmnt)
    .map(t => SAst.FuncDef(t._1, t._2))

  val NamespaceDefStmnt: P[SAst.NamespaceDef] = P(
    (NAMESPACE ~~ &(Ws) ~ QualifiedName ~ SemicolonFactory).map(t => SAst.NamespaceDef(Some(t._1), None, t._2))
      | (NAMESPACE ~~ &(Ws) ~/ QualifiedName.? ~ CompoundStmnt).map(t => SAst.NamespaceDef(t._1, Some(t._2), None)))


  private val PossibleStatements: P[SAst.Statement] = P(CompoundStmnt | NamedLabelStmnt | SelectionStmnt | IterationStmnt | JumpStmnt | TryStmnt
    | DeclareStmnt | ConstDeclStmnt | FunctionDefStmnt | ClassDeclStmnt | InterfaceDeclStmnt | TraitDeclStmnt
    | NamespaceDefStmnt | NamespaceUseDeclStmnt | GlobalDeclStmnt | FunctionStaticDeclStmnt | EmptyStmnt | ExpStmnt)
}
