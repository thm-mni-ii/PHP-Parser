package parser.statements

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import ast.{Basic => BAst, Statements => SAst}
import parser.literals.KeywordConversions._
import parser.literals.Keywords._
import parser.literals.Literals.{Name, VariableName}

import parser.Basic.{NamespaceName, QualifiedName, SemicolonFactory}
import parser.expressions.ExpressionParser.Expression
import parser.expressions.VariableParser.SimpleVariable
import parser.statements.StatementParser.{CompoundStmnt, FuncHeader, wrap}


/**
  * This object contains all declaration statements
  */
object DeclarationParser {

  // declaration statements

  private val ConstElem = P(Name ~ "=" ~ Expression).map {
    case (name, exp) => SAst.ConstElement(name, exp)
  }

  val ConstDeclStmnt = P(CONST ~~ &(Ws) ~/ ConstElem.rep ~ SemicolonFactory).map {
    case (constElems, text) => wrap(SAst.ConstDecl(constElems), text)
  }

  val GlobalDeclStmnt = P(GLOBAL ~~ &(Ws) ~/ SimpleVariable.rep(sep = ",".~/) ~ SemicolonFactory).map {
    case (simpleVar, text) => wrap(SAst.GlobalDecl(simpleVar), text)
  }


  val FunctionStaticDeclStmnt = {
    val StaticVarElement = P(VariableName ~ ("=" ~/ Expression).?).map {
      case (varName, exp) => SAst.StaticVarElement(varName, exp)
    }

    P(STATIC ~~ &(Ws) ~/ StaticVarElement.rep(sep = ",".~/) ~ SemicolonFactory).map {
      case (varElements, text) => wrap(SAst.FuncStaticDecl(varElements), text)
    }
  }

  val NamespaceUseDeclStmnt = {
    val NamespaceUseType: P[SAst.NamespaceUseType.Value] = P(FunctionUseType | ConstUseType)

    val NameSpaceUseClause1 = P(NamespaceName ~ (AS ~ Name).?).map {
      case (namespaceName, asName) => SAst.NamespaceUseClause(None, Right(namespaceName), asName)
    }
    val NameSpaceUseClause2 = P(QualifiedName ~ (AS ~ Name).?).map {
      case (qName, asName) => SAst.NamespaceUseClause(None, Left(qName), asName)
    }
    val NameSpaceUseClause3 = P(NamespaceUseType.? ~ NamespaceName ~ (AS ~ Name).?).map {
      case (useType, name, asName) => SAst.NamespaceUseClause(useType, Right(name), asName)
    }

    P(USE ~~ &(Ws) ~/ (
      (NamespaceUseType ~~ &(Ws) ~ "\\".? ~ NamespaceName ~ "\\" ~ "{" ~ NameSpaceUseClause1.rep(min = 1, sep = ",") ~ "}").map(t => SAst.NamespaceUseDecl(Some(t._1), Some(t._2), t._3))
        | ((NamespaceUseType ~~ &(Ws)).? ~ NameSpaceUseClause2.rep(min = 1, sep = ",") ~ SemicolonFactory).map(t => wrap(SAst.NamespaceUseDecl(t._1, None, t._2), t._3))
        | ("\\".? ~ NamespaceName ~ "\\" ~ "{" ~ NameSpaceUseClause3.rep(min = 1, sep = ",") ~ "}").map(t => SAst.NamespaceUseDecl(None, Some(t._1), t._2)))
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

  private def wrapMemberDecl(mDecl: SAst.MemberDecl, text: Option[BAst.Text]): SAst.MemberDecl = text match {
    case Some(_) => SAst.EndTagMemberDecl(mDecl, text)
    case None => mDecl
  }

  val EmptyMemberDecl = P(SemicolonFactory).map(wrapMemberDecl(SAst.EmptyMemberDecl(),_))

  val ClassConstDecl = P((VisibilityMod ~~ &(Ws)).? ~ CONST ~~ &(Ws) ~/ ConstElem.rep ~ SemicolonFactory).map {
    case (mod, constElems, text) => wrapMemberDecl(SAst.ClassConstDecl(mod, constElems), text)
  }

  val PropertyDecl = {
    val propertyElem: P[SAst.PropertyElement] = P(VariableName ~ ("=" ~ Expression).?).map{
      case (varName, exp) => SAst.PropertyElement(varName, exp)
    }

    P(PropertyMod ~~ &(Ws) ~ propertyElem.rep(sep=",") ~ SemicolonFactory).map {
      case (mod, pElems, text) => wrapMemberDecl(SAst.PropertyDecl(mod, pElems), text)
    }
  }

  val MethodDecl = {
    val BodyOrEnd: P[(Option[SAst.CompoundStmnt], Option[BAst.Text])] =
      P(SemicolonFactory.map((None, _)) | CompoundStmnt.map(stmnt => (Some(stmnt), None)))

    P((MethodMod ~~ &(Ws)).rep ~ FuncHeader ~ BodyOrEnd).map {
      case (mod, header, (body, text)) => wrapMemberDecl(SAst.MethodDecl(mod, header, body), text)
    }
  }

  val TraitUseClause = {
    val TraitUseSpec: P[SAst.TraitUseSpec] =
      P((Name ~~ &(Ws) ~ INSTEADOF ~~ &(Ws) ~ Name).map(t => SAst.SelectInsteadofClause(t._1, t._2))
        | (Name ~~ &(Ws) ~ AS ~~ &(Ws) ~ (VisibilityMod ~~ &(Ws)).? ~ Name).map(t => SAst.TraitAliasClause(t._1, t._2, Some(t._3)))
        | (Name ~~ &(Ws) ~ AS ~~ &(Ws) ~ VisibilityMod ~~ &(Ws) ~ Name.?).map(t => SAst.TraitAliasClause(t._1, Some(t._2), t._3)))

    val TraitUseSpecs: P[(Seq[SAst.TraitUseSpec], Option[BAst.Text])] = P(SemicolonFactory.map((Seq(), _)) |
      ("{" ~ TraitUseSpec.rep(sep = ",") ~ "}").map((_, None)))

    P(USE ~~ &(Ws) ~/ QualifiedName.rep(sep = ",") ~/ TraitUseSpecs).map {
      case (qName, (specs, text)) => wrapMemberDecl(SAst.TraitUseClause(qName, specs), text)
    }
  }


  // class declarations

  private[parser] val ClassDeclBody: P[(Option[BAst.QualifiedName], Option[Seq[BAst.QualifiedName]], Seq[SAst.MemberDecl])] = {
    val ClassMemberDecl: P[SAst.MemberDecl] = P(EmptyMemberDecl | ClassConstDecl | PropertyDecl | MethodDecl | TraitUseClause)

    P((&(Ws) ~ EXTENDS ~~ &(Ws) ~ QualifiedName).?
      ~~ (&(Ws) ~ IMPLEMENTS ~~ &(Ws) ~ QualifiedName.rep(sep = ",".~/)).?
      ~ "{" ~/ ClassMemberDecl.rep ~ "}")
  }

  val ClassDeclStmnt = P(ClassMod.? ~ CLASS ~~ &(Ws) ~/ Name ~~ ClassDeclBody).map {
      case (mod, name, (extendsName, interfaces, members)) => SAst.ClassDecl(mod, Some(name), extendsName, interfaces, members)
  }


  // interface declarations

  val InterfaceDeclStmnt = {
    val InterfaceMemberDecl: P[SAst.MemberDecl] = P(EmptyMemberDecl | ClassConstDecl | MethodDecl)

    P(INTERFACE ~~ &(Ws) ~/ Name ~~ (&(Ws)
      ~ EXTENDS ~~ &(Ws) ~ QualifiedName.rep(sep = ",".~/)).?
      ~ "{" ~/ InterfaceMemberDecl.rep ~ "}"
    ).map {
      case (name, extendsNames, members) => SAst.InterfaceDecl(name, extendsNames, members)
    }
  }


  // trait declarations

  val TraitDeclStmnt = {
    val TraitMemberDecl: P[SAst.MemberDecl] = P(EmptyMemberDecl | PropertyDecl | MethodDecl | TraitUseClause)

    P(TRAIT ~~ &(Ws) ~/ Name
      ~ "{" ~/ TraitMemberDecl.rep ~ "}"
    ).map{
      case (name, members) => SAst.TraitDecl(name, members)
    }
  }
}
