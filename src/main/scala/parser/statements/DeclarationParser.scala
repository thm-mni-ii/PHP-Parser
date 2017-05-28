package parser.statements

import ast.Basic.QualifiedName
import ast.Statements._
import fastparse.noApi._
import parser.literals.WsAPI._
import parser.Basic.qualifiedName
import parser.ExpressionParser.expression
import parser.literals.KeywordConversions._
import parser.literals.Keywords._
import parser.statements.StatementParser.{compoundStmnt, funcHeader}
import parser.literals.Literals._
import parser.Basic._


/**
  * Created by tobias on 27.05.17.
  */
object DeclarationParser {

  private val constElem: P[ConstElement] = P(name ~ "=" ~ expression)
    .map(t => ConstElement(t._1, t._2))

  val constDeclStmnt: P[ConstDecl] = P(CONST ~ constElem.rep ~ ";")
    .map(ConstDecl)


  val globalDeclStmnt : P[GlobalDecl] = P(GLOBAL ~ variableName.rep(sep=",") ~ ";")
    .map(GlobalDecl)


  private val staticVarElement : P[StaticVarElement] = P(variableName ~ ("=" ~ expression).?)
    .map(t => StaticVarElement(t._1, t._2))

  val functionStaticDeclStmnt : P[FuncStaticDecl] = P(STATIC ~ staticVarElement.rep(sep=",") ~ ";")
    .map(FuncStaticDecl)


  private val namespaceUseType: P[NamespaceUseType.Value] = P(functionUseType | constUseType)

  val namespaceUseDeclStmnt = P(USE ~/ (
    (namespaceUseType ~ "\\".? ~ namespaceName ~ "\\" ~ "{" ~ (namespaceName ~ (AS ~ name).?)
      .map(t => NamespaceUseClause(None, Right(t._1), t._2)).rep(min = 1, sep = ",") ~ "}")
      .map(t => NamespaceUseDecl(Some(t._1), Some(t._2), t._3, None)) |
      (namespaceUseType.? ~ (qualifiedName ~ (AS ~ name).?)
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

  val classConstDecl: P[ClassConstDecl] = P(visibilityMod.? ~ CONST ~ constElem.rep ~ ";")
    .map(t => ClassConstDecl(t._1, t._2))


  private val propertyElem: P[PropertyElement] = P(variableName ~ ("=" ~ expression).?)
    .map(t => PropertyElement(t._1, t._2))

  val propertyDecl: P[PropertyDecl] = P(propertyMod ~ propertyElem.rep ~ ";")
    .map(t => PropertyDecl(t._1, t._2))


  private val bodyOrEnd: P[Option[CompoundStmnt]] = P(";".!.map(_ => None) | compoundStmnt.map(Some(_)))

  val methodDecl: P[MethodDecl] = P(methodMod.rep ~ funcHeader ~ bodyOrEnd)
    .map(t => MethodDecl(t._1, t._2, t._3))


  private val traitUseSpec: P[TraitUseSpec] =
    P((name ~ INSTEADOF ~ name).map(t => SelectInsteadofClause(t._1, t._2)) |
      (name ~ AS ~ visibilityMod.? ~ name).map(t => TraitAliasClause(t._1, t._2, Some(t._3))) |
      (name ~ AS ~ visibilityMod ~ name.?).map(t => TraitAliasClause(t._1, Some(t._2), t._3)))

  private val traitUseSpecs: P[Seq[TraitUseSpec]] = P(";".!.map(_ => Seq()) |
    ("{" ~ traitUseSpec.rep(sep = ",") ~ "}"))

  val traitUseClause: P[TraitUseClause] = P(USE ~/ qualifiedName.rep(sep = ",") ~ traitUseSpecs)
    .map(t => TraitUseClause(t._1, t._2))


  // class declarations

  private val classMemberDecl: P[MemberDecl] = P(classConstDecl | propertyDecl | methodDecl | traitUseClause)

  private[parser] val classDeclBody: P[(Option[QualifiedName], Option[Seq[QualifiedName]], Seq[MemberDecl])] = P((EXTENDS ~ qualifiedName).? ~
    (IMPLEMENTS ~ qualifiedName.rep(sep = ",")).? ~ "{" ~ classMemberDecl.rep ~ "}")

  val classDeclStmnt: P[ClassDecl] = P(classMod.? ~ CLASS ~/ name ~ classDeclBody)
    .map(t => ClassDecl(t._1, Some(t._2), t._3._1, t._3._2, t._3._3))


  // interface declarations

  private val interfaceMemberDecl: P[MemberDecl] = P(classConstDecl | methodDecl)

  val interfaceDeclStmnt: P[InterfaceDecl] =
    P(INTERFACE ~/ name ~ (EXTENDS ~ qualifiedName.rep(sep = ",")).? ~ "(" ~ interfaceMemberDecl.rep ~ ")")
      .map(t => InterfaceDecl(t._1, t._2, t._3))


  // trait declarations

  private val traitMemberDecl: P[MemberDecl] = P(propertyDecl | methodDecl | traitUseClause)

  val traitDeclStmnt: P[TraitDecl] =
    P(TRAIT ~/ name ~ "(" ~ interfaceMemberDecl.rep ~ ")")
      .map(t => TraitDecl(t._1, t._2))
}
