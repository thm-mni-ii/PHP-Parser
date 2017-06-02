package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.ws

import ast.Basic.{QualifiedName, Text}
import ast.Statements._

import parser.literals.KeywordConversions._
import parser.literals.Keywords._
import parser.literals.Literals.{name, variableName}

import parser.Basic.{qualifiedName, semicolonFactory, namespaceName}
import parser.expressions.ExpressionParser.{expression}
import parser.expressions.VariableParser.simpleVariable
import parser.statements.StatementParser.{compoundStmnt, funcHeader}


/**
  * This object contains all declaration statements
  */
object DeclarationParser {

  // declaration statements

  private val constElem: P[ConstElement] = P(name ~ "=" ~ expression)
    .map(t => ConstElement(t._1, t._2))

  val constDeclStmnt: P[ConstDecl] = P(CONST ~~ &(ws) ~/ constElem.rep ~ semicolonFactory)
    .map(t => ConstDecl(t._1, t._2))


  val globalDeclStmnt : P[GlobalDecl] = P(GLOBAL ~~ &(ws) ~/ simpleVariable.rep(sep=",".~/) ~ semicolonFactory)
    .map(t => GlobalDecl(t._1, t._2))


  private val staticVarElement : P[StaticVarElement] = P(variableName ~ ("=" ~/ expression).?)
    .map(t => StaticVarElement(t._1, t._2))

  val functionStaticDeclStmnt : P[FuncStaticDecl] = P(STATIC ~~ &(ws) ~/ staticVarElement.rep(sep=",".~/) ~ semicolonFactory)
    .map(t => FuncStaticDecl(t._1, t._2))


  private val namespaceUseType: P[NamespaceUseType.Value] = P(functionUseType | constUseType)

  val namespaceUseDeclStmnt = P(USE ~~ &(ws) ~/ (
    (namespaceUseType ~~ &(ws) ~ "\\".? ~ namespaceName ~ "\\" ~ "{" ~ (namespaceName ~ (AS ~ name).?)
      .map(t => NamespaceUseClause(None, Right(t._1), t._2)).rep(min = 1, sep = ",") ~ "}")
      .map(t => NamespaceUseDecl(Some(t._1), Some(t._2), t._3, None)) |
      ((namespaceUseType ~~ &(ws)).? ~ (qualifiedName ~ (AS ~ name).?)
        .map(t => NamespaceUseClause(None, Left(t._1), t._2)).rep(min = 1, sep = ",") ~ semicolonFactory)
        .map(t => NamespaceUseDecl(t._1, None, t._2, t._3)) |
      ("\\".? ~ namespaceName ~ "\\" ~ "{" ~ (namespaceUseType.? ~ namespaceName ~ (AS ~ name).?)
        .map(t => NamespaceUseClause(t._1, Right(t._2), t._3)).rep(min = 1, sep = ",") ~ "}")
        .map(t => NamespaceUseDecl(None, Some(t._1), t._2, None)))
  )


  // available modifiers

  private val visibilityMod: P[VisibilityModifier] = P(publicMod | privateMod | protectedMod)

  private val classMod: P[ClassModifier] = P(abstractMod | finalMod)

  private val propertyMod: P[PropertyModifier] = P(noMod |
    (visibilityMod ~ staticMod.?).map(t => CombinedMod(t._2, Some(t._1))) |
    (staticMod ~ visibilityMod.?).map(t => CombinedMod(Some(t._1), t._2)))

  private val methodMod: P[MethodModifier] = P(visibilityMod | staticMod | classMod)


  // member declarations

  val classConstDecl: P[ClassConstDecl] = P((visibilityMod ~~ &(ws)).? ~ CONST ~~ &(ws) ~/ constElem.rep ~ semicolonFactory)
    .map(t => ClassConstDecl(t._1, t._2, t._3))


  private val propertyElem: P[PropertyElement] = P(variableName ~ ("=" ~ expression).?)
    .map(t => PropertyElement(t._1, t._2))

  val propertyDecl: P[PropertyDecl] = P(propertyMod ~~ &(ws) ~ propertyElem.rep ~ semicolonFactory)
    .map(t => PropertyDecl(t._1, t._2, t._3))


  private val bodyOrEnd: P[(Option[CompoundStmnt], Option[Text])] =
    P(semicolonFactory.map((None, _)) | compoundStmnt.map(t => (Some(t), None)))

  val methodDecl: P[MethodDecl] = P((methodMod ~~ &(ws)).rep ~ funcHeader ~ bodyOrEnd)
    .map(t => MethodDecl(t._1, t._2, t._3._1, t._3._2))


  private val traitUseSpec: P[TraitUseSpec] =
    P((name ~~ &(ws) ~ INSTEADOF ~~ &(ws) ~ name).map(t => SelectInsteadofClause(t._1, t._2)) |
      (name ~~ &(ws) ~ AS ~~ &(ws) ~ (visibilityMod ~~ &(ws)).? ~ name).map(t => TraitAliasClause(t._1, t._2, Some(t._3))) |
      (name ~~ &(ws) ~ AS ~~ &(ws) ~ visibilityMod ~~ &(ws) ~ name.?).map(t => TraitAliasClause(t._1, Some(t._2), t._3)))

  private val traitUseSpecs: P[(Seq[TraitUseSpec], Option[Text])] = P(semicolonFactory.map((Seq(), _)) |
      ("{" ~ traitUseSpec.rep(sep = ",") ~ "}").map((_, None)))

  val traitUseClause: P[TraitUseClause] = P(USE ~~ &(ws) ~/ qualifiedName.rep(sep = ",") ~/ traitUseSpecs)
    .map(t => TraitUseClause(t._1, t._2._1, t._2._2))


  // class declarations

  private val classMemberDecl: P[MemberDecl] = P(classConstDecl | propertyDecl | methodDecl | traitUseClause)

  private[parser] val classDeclBody: P[(Option[QualifiedName], Option[Seq[QualifiedName]], Seq[MemberDecl])] = P(
    (&(ws) ~ EXTENDS ~~ &(ws) ~ qualifiedName).? ~~
    (&(ws) ~ IMPLEMENTS ~~ &(ws) ~ qualifiedName.rep(sep = ",".~/)).? ~
      "{" ~/ classMemberDecl.rep ~ "}")

  val classDeclStmnt: P[ClassDecl] = P(classMod.? ~ CLASS ~~ &(ws) ~/ name ~~ classDeclBody)
    .map(t => ClassDecl(t._1, Some(t._2), t._3._1, t._3._2, t._3._3))


  // interface declarations

  private val interfaceMemberDecl: P[MemberDecl] = P(classConstDecl | methodDecl)

  val interfaceDeclStmnt: P[InterfaceDecl] =
    P(INTERFACE ~~ &(ws) ~/ name ~~ (&(ws) ~ EXTENDS ~~ &(ws) ~ qualifiedName.rep(sep = ",".~/)).? ~ "{" ~/ interfaceMemberDecl.rep ~ "}")
      .map(t => InterfaceDecl(t._1, t._2, t._3))


  // trait declarations

  private val traitMemberDecl: P[MemberDecl] = P(propertyDecl | methodDecl | traitUseClause)

  val traitDeclStmnt: P[TraitDecl] =
    P(TRAIT ~~ &(ws) ~/ name ~ "(" ~/ interfaceMemberDecl.rep ~ ")")
      .map(t => TraitDecl(t._1, t._2))
}
