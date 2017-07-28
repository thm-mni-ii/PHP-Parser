package phpparser.parser.statements

import fastparse.noApi._
import phpparser.parser.literals.WsAPI._
import phpparser.parser.literals.Lexical.Ws

import phpparser.ast.{Basic => BAst, Expressions => EAst, Statements => SAst}

import phpparser.parser.literals.KeywordConversions._
import phpparser.parser.literals.Keywords._
import phpparser.parser.literals.Literals._

import phpparser.parser.Basic.{EchoStartTag, QualifiedName, SemicolonFactory}
import phpparser.parser.expressions.ExpressionParser.Expression
import phpparser.parser.statements.ControlFlowParser._
import phpparser.parser.statements.DeclarationParser._

/**
  * This object contains all basic statements
  */
object StatementParser {

  val Statement: P[SAst.Statement] = P(CompoundStmnt | NamedLabelStmnt | SelectionStmnt | IterationStmnt | JumpStmnt | TryStmnt
    | DeclareStmnt | ConstDeclStmnt | FunctionDefStmnt | ClassDeclStmnt | InterfaceDeclStmnt | TraitDeclStmnt
    | NamespaceDefStmnt | NamespaceUseDeclStmnt | GlobalDeclStmnt | FunctionStaticDeclStmnt | EmptyStmnt | ExpStmnt)

  val Statements: P[Seq[SAst.Statement]] = P(Statement.? ~ AvailableStatement.rep).map(t => t._1 match {
    case Some(stmnt) => stmnt +: t._2
    case None => t._2
  })

  val AvailableStatement: P[SAst.Statement] = P(EchoTagStmnt | Statement)

  val EchoTagStmnt = P(EchoStartTag ~ Expression.rep(min = 1, sep = ",") ~ SemicolonFactory).map {
    case (_, expressions, text) => wrap(SAst.EchoTagStmnt(expressions), text)
  }

  private[statements] def wrap(stmnt: SAst.Statement, text: Option[BAst.Text]): SAst.Statement = text match {
    case Some(_) => SAst.EndTagStmnt(stmnt, text)
    case None => stmnt
  }

  val EmptyStmnt = P(SemicolonFactory).map(wrap(SAst.EmptyStmnt(), _))
  val CompoundStmnt = P("{" ~/ Statements ~ "}").map(SAst.CompoundStmnt)
  val NamedLabelStmnt = P(Name ~ ":" ~ Statement).map {
    case (name, stmnt) => SAst.NamedLabelStmnt(name, stmnt)
  }
  val ExpStmnt = P(Expression ~ SemicolonFactory).map {
    case (exp, text) => wrap(SAst.ExpressionStmnt(exp), text)
  }

  val TryStmnt = {
    val CatchClause = P(CATCH ~ "(" ~/ QualifiedName ~~ Ws ~ VariableName ~ ")" ~/ CompoundStmnt).map {
      case (qName, varName, stmnt) => SAst.CatchClause(qName, varName, stmnt)
    }

    P(TRY ~ &("{") ~/ CompoundStmnt ~/ CatchClause.rep() ~ (FINALLY ~ &("{") ~/ CompoundStmnt).?).map {
      case (stmnt, clauses, finallyStmnt) => SAst.TryStmnt(stmnt, clauses, finallyStmnt)
    }
  }

  val DeclareStmnt = {
    val DeclareDeclarative: P[SAst.DeclareDeclarative.Value] =
      P(TicksDeclarative | EncodingDeclarative | StrictTypesDeclarative)

    val DeclareBody: P[(Seq[SAst.Statement], Option[BAst.Text])] =
      P(":" ~/ Statements ~ ENDDECLARE ~ SemicolonFactory |
        Statement.map(stmnt => (Seq(stmnt), None)))

    P(DECLARE ~ "(" ~/ DeclareDeclarative ~ "=" ~ Literal ~ ")" ~/ DeclareBody).map {
      case (decl, literal, (stmnt, text)) => wrap(SAst.DeclareStmnt(decl, literal, stmnt), text)
    }
  }


  val TypeDecl: P[SAst.TypeDecl] = P(ArrayType | CallableType | IterableType |
    BoolType | FloatType | IntType | StringType | QualifiedName.map(SAst.QualifiedType))
  val PossibleFunctionType: P[SAst.PossibleTypes] = P(VoidType | TypeDecl)

  private val ParamType: P[(Option[SAst.TypeDecl], Boolean)] = P(TypeDecl.? ~ "&".!.?.map(_.isDefined))
  private val ParameterDecl: P[SAst.ParameterDecl] = {
    val ParameterDeclFactory: P[(Option[SAst.TypeDecl], Boolean) => SAst.ParameterDecl] = P(
      ("..." ~ VariableName).map(name => (a: Option[SAst.TypeDecl], b: Boolean) => SAst.VariadicParam(a, b, name))
        | (VariableName ~ ("=" ~ Expression).?).map(t => (a: Option[SAst.TypeDecl], b: Boolean) => SAst.SimpleParam(a, b, t._1, t._2)))

    P(ParamType ~ ParameterDeclFactory).map {
      case (typeDecl, isRef, toParamDecl) => toParamDecl(typeDecl, isRef)
    }
  }

  private[statements] val FuncHeader: P[SAst.FuncHeader] =
    P(FUNCTION ~~ &(Ws) ~ "&".!.? ~ Name ~/ "("
      ~/ ParameterDecl.rep(sep = ",".~/) ~ ")" ~/ (":" ~/ PossibleFunctionType).?
    ).map {
      case (returnRef, name, params, returnValue) => SAst.FuncHeader(returnRef.isDefined, Some(name), params, returnValue)
    }

  private[parser] val AnonymousFuncHeader: P[(SAst.FuncHeader, Seq[(Boolean, EAst.SimpleNameVar)])] =
    P(FUNCTION ~~ &(Ws | "(") ~ "&".!.?
      ~ "(" ~/ ParameterDecl.rep(sep = ",".~/) ~ ")"
      ~/ (USE ~ "(" ~ ("&".!.?.map(_.isDefined) ~ VariableName).rep(1, sep = ",") ~ ")").?
      ~ (":" ~/ PossibleFunctionType).?
    ).map {
      case (returnRef, params, uses, returnValue) => (SAst.FuncHeader(returnRef.isDefined, None, params, returnValue), uses.getOrElse(Seq()))
    }

  val FunctionDefStmnt = P(FuncHeader ~ &("{") ~/ CompoundStmnt).map {
    case (header, body) => SAst.FuncDef(header, body)
  }

  val NamespaceDefStmnt: P[SAst.Statement] = P(
    (NAMESPACE ~~ &(Ws) ~ QualifiedName ~ SemicolonFactory).map {
      case (name, text) => wrap(SAst.NamespaceDef(Some(name), None), text)
    } | (NAMESPACE ~~ &(Ws) ~/ QualifiedName.? ~ CompoundStmnt).map{
      case (name, stmnt) => SAst.NamespaceDef(name, Some(stmnt))
    })
}
