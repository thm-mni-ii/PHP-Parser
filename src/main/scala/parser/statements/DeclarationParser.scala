package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Basic => BAst, Statements => SAst}

import parser.literals.KeywordConversions._
import parser.literals.Keywords._
import parser.literals.Literals.{Name, VariableName}

import parser.Basic.{QualifiedName, SemicolonFactory, NamespaceName}
import parser.expressions.ExpressionParser.{Expression}
import parser.expressions.VariableParser.SimpleVariable
import parser.statements.StatementParser.{CompoundStmnt, FuncHeader}


/**
  * This object contains all declaration statements
  */
object DeclarationParser {

  // declaration statements

  private val ConstElem = P(Name ~ "=" ~ Expression)
    .map(t => SAst.ConstElement(t._1, t._2))

  val ConstDeclStmnt = P(CONST ~~ &(Ws) ~/ ConstElem.rep ~ SemicolonFactory)
      .map(t => SAst.ConstDecl(t._1, t._2))

  val GlobalDeclStmnt = P(GLOBAL ~~ &(Ws) ~/ SimpleVariable.rep(sep = ",".~/) ~ SemicolonFactory)
    .map(t => SAst.GlobalDecl(t._1, t._2))


  val FunctionStaticDeclStmnt = {
    val StaticVarElement = P(VariableName ~ ("=" ~/ Expression).?)
      .map(t => SAst.StaticVarElement(t._1, t._2))

    P(STATIC ~~ &(Ws) ~/ StaticVarElement.rep(sep = ",".~/) ~ SemicolonFactory)
      .map(t => SAst.FuncStaticDecl(t._1, t._2))
  }

  val NamespaceUseDeclStmnt = {
    val NamespaceUseType: P[SAst.NamespaceUseType.Value] = P(FunctionUseType | ConstUseType)

    P(USE ~~ &(Ws) ~/ (
      (NamespaceUseType ~~ &(Ws) ~ "\\".? ~ NamespaceName ~ "\\" ~ "{" ~ (NamespaceName ~ (AS ~ Name).?)
        .map(t => SAst.NamespaceUseClause(None, Right(t._1), t._2)).rep(min = 1, sep = ",") ~ "}")
        .map(t => SAst.NamespaceUseDecl(Some(t._1), Some(t._2), t._3, None)) |
        ((NamespaceUseType ~~ &(Ws)).? ~ (QualifiedName ~ (AS ~ Name).?)
          .map(t => SAst.NamespaceUseClause(None, Left(t._1), t._2)).rep(min = 1, sep = ",") ~ SemicolonFactory)
          .map(t => SAst.NamespaceUseDecl(t._1, None, t._2, t._3)) |
        ("\\".? ~ NamespaceName ~ "\\" ~ "{" ~ (NamespaceUseType.? ~ NamespaceName ~ (AS ~ Name).?)
          .map(t => SAst.NamespaceUseClause(t._1, Right(t._2), t._3)).rep(min = 1, sep = ",") ~ "}")
          .map(t => SAst.NamespaceUseDecl(None, Some(t._1), t._2, None)))
    )
  }


  // available modifiers

  private val VisibilityMod: P[SAst.VisibilityModifier] = P(PublicMod | PrivateMod | ProtectedMod)
  private val ClassMod: P[SAst.ClassModifier] = P(AbstractMod | FinalMod)
  private val MethodMod: P[SAst.MethodModifier] = P(VisibilityMod | StaticMod | ClassMod)
  private val PropertyMod: P[SAst.PropertyModifier] = P(NoMod
    | (VisibilityMod ~ StaticMod.?).map(t => SAst.CombinedMod(t._2, Some(t._1)))
    | (StaticMod ~ VisibilityMod.?).map(t => SAst.CombinedMod(Some(t._1), t._2)))

  // member declarations

  val ClassConstDecl = P((VisibilityMod ~~ &(Ws)).? ~ CONST ~~ &(Ws) ~/ ConstElem.rep ~ SemicolonFactory)
    .map(t => SAst.ClassConstDecl(t._1, t._2, t._3))

  val PropertyDecl = {
    val propertyElem: P[SAst.PropertyElement] = P(VariableName ~ ("=" ~ Expression).?)
      .map(t => SAst.PropertyElement(t._1, t._2))

    P(PropertyMod ~~ &(Ws) ~ propertyElem.rep ~ SemicolonFactory)
      .map(t => SAst.PropertyDecl(t._1, t._2, t._3))
  }

  val MethodDecl = {
    val BodyOrEnd: P[(Option[SAst.CompoundStmnt], Option[BAst.Text])] =
      P(SemicolonFactory.map((None, _)) | CompoundStmnt.map(t => (Some(t), None)))

    P((MethodMod ~~ &(Ws)).rep ~ FuncHeader ~ BodyOrEnd)
      .map(t => SAst.MethodDecl(t._1, t._2, t._3._1, t._3._2))
  }

  val TraitUseClause = {
    val TraitUseSpec: P[SAst.TraitUseSpec] =
      P((Name ~~ &(Ws) ~ INSTEADOF ~~ &(Ws) ~ Name).map(t => SAst.SelectInsteadofClause(t._1, t._2))
        | (Name ~~ &(Ws) ~ AS ~~ &(Ws) ~ (VisibilityMod ~~ &(Ws)).? ~ Name).map(t => SAst.TraitAliasClause(t._1, t._2, Some(t._3)))
        | (Name ~~ &(Ws) ~ AS ~~ &(Ws) ~ VisibilityMod ~~ &(Ws) ~ Name.?).map(t => SAst.TraitAliasClause(t._1, Some(t._2), t._3)))

    val TraitUseSpecs: P[(Seq[SAst.TraitUseSpec], Option[BAst.Text])] = P(SemicolonFactory.map((Seq(), _)) |
      ("{" ~ TraitUseSpec.rep(sep = ",") ~ "}").map((_, None)))

    P(USE ~~ &(Ws) ~/ QualifiedName.rep(sep = ",") ~/ TraitUseSpecs)
      .map(t => SAst.TraitUseClause(t._1, t._2._1, t._2._2))
  }


  // class declarations

  private[parser] val ClassDeclBody: P[(Option[BAst.QualifiedName], Option[Seq[BAst.QualifiedName]], Seq[SAst.MemberDecl])] = {
    val ClassMemberDecl: P[SAst.MemberDecl] = P(ClassConstDecl | PropertyDecl | MethodDecl | TraitUseClause)

    P((&(Ws) ~ EXTENDS ~~ &(Ws) ~ QualifiedName).?
      ~~ (&(Ws) ~ IMPLEMENTS ~~ &(Ws) ~ QualifiedName.rep(sep = ",".~/)).?
      ~ "{" ~/ ClassMemberDecl.rep ~ "}")
  }

  val ClassDeclStmnt = P(ClassMod.? ~ CLASS ~~ &(Ws) ~/ Name ~~ ClassDeclBody)
    .map(t => SAst.ClassDecl(t._1, Some(t._2), t._3._1, t._3._2, t._3._3))


  // interface declarations

  val InterfaceDeclStmnt = {
    val InterfaceMemberDecl: P[SAst.MemberDecl] = P(ClassConstDecl | MethodDecl)

    P(INTERFACE ~~ &(Ws) ~/ Name ~~ (&(Ws)
      ~ EXTENDS ~~ &(Ws) ~ QualifiedName.rep(sep = ",".~/)).?
      ~ "{" ~/ InterfaceMemberDecl.rep ~ "}"
    ).map(t => SAst.InterfaceDecl(t._1, t._2, t._3))
  }


  // trait declarations

  val TraitDeclStmnt = {
    val TraitMemberDecl: P[SAst.MemberDecl] = P(PropertyDecl | MethodDecl | TraitUseClause)

    P(TRAIT ~~ &(Ws) ~/ Name
      ~ "{" ~/ TraitMemberDecl.rep ~ "}"
    ).map(t => SAst.TraitDecl(t._1, t._2))
  }
}
