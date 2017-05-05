package parser

import fastparse.noApi._
import WsAPI._

import ExpressionParser.expression
import PHPParser._

import ast.Ast._

object StatementParser {
  val statement : P[Statement] = P(compoundStmnt | namedLabelStmnt | selectionStmnt | selectionStmnt | iterationStmnt | jumpStmnt | tryStmnt |
      declareStmnt | constDeclStmnt | functionDefStmnt | classDeclStmnt | interfaceDeclStmnt | traitDeclStmnt |
      namespaceDefStmnt | namespaceUseDeclStmnt | globalDeclStmnt | functionStaticDeclStmnt | expStmnt).log()

  val compoundStmnt : P[CompoundStmnt] = P("{" ~/ statement.rep ~ "}").map(CompoundStmnt)

  val namedLabelStmnt : P[NamedLabelStmnt] = P(name ~ ":" ~ statement).map(t => NamedLabelStmnt(t._1, t._2))

  val expStmnt : P[ExpressionStmnt] = P(expression.? ~ ";").map(ExpressionStmnt)

  val selectionStmnt : P[SelectionStmnt] = P(ifStmnt | switchStmnt)

  val ifBody : P[(Seq[Statement], Seq[(Expression, Seq[Statement])], Option[Seq[Statement]])] =
    P((":" ~ (statement).rep(1) ~ ("elseif" ~ "(" ~ expression ~ ")" ~ ":" ~ statement.rep(1)).rep ~ ("else" ~ ":" ~ statement.rep).?) |
      (statement ~ ("elseif" ~ "(" ~ expression ~ ")" ~ statement).rep ~ ("else" ~ statement).?).map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)))))

  val ifStmnt : P[IfStmnt] =
    P("if" ~/ "(" ~ expression ~ ")" ~ ifBody).map(t => IfStmnt(t._1, t._2._1, t._2._2, t._2._3))

  val switchStmnt : P[SwitchStmnt] =
    P("switch" ~/ "(" ~ expression ~ ")" ~ (("{" ~ (caseBlock | defaultBlock).rep ~ "}") | (":" ~ (caseBlock | defaultBlock).rep ~ "endswitch" ~ ";"))).map(t => SwitchStmnt(t._1, t._2))

  val caseBlock : P[CaseBlock] =
    P("case" ~/ expression ~ (":" | ";") ~ statement.rep).map(t => CaseBlock(t._1, t._2))

  val defaultBlock : P[DefaultBlock] =
    P("default" ~/ (":" | ";") ~ statement.rep).map(DefaultBlock)

  val iterationStmnt : P[IterationStmnt] = P(whileStmnt | doStmnt | forStmnt | foreachStmnt)

  val whileBody : P[Seq[Statement]] =
    P((":" ~ statement.rep ~ "endwhile" ~ ";") | statement.map(Seq(_)))

  val whileStmnt : P[WhileStmnt] =
    P("while" ~/ "(" ~ expression ~ ")" ~ whileBody).map(t => WhileStmnt(t._1, t._2))

  val doStmnt : P[DoStmnt] =
    P("do" ~/ statement ~ "while" ~/ "(" ~ expression ~ ")").map(t => DoStmnt(t._2, t._1))

  val forBody : P[Seq[Statement]] = P(":" ~ statement.rep ~ "endfor" ~ ";" | statement.map(Seq(_)))

  val forStmnt : P[ForStmnt] = P("for" ~/ "(" ~ expression.rep(sep=",") ~ ";" ~ expression.rep(sep=",") ~ ";" ~ expression.rep(sep=",") ~ ")" ~ forBody).map(t => ForStmnt(t._1, t._2, t._3, t._4))

  val foreachBody : P[Seq[Statement]] = P((":" ~ statement.rep ~ "foreach" ~ ";") | statement.map(Seq(_)))

  val foreachStmnt : P[ForeachStmnt] = P("----").map(_ => ForeachStmnt())
  //TODO missing foreach

  val jumpStmnt : P[JumpStmnt] = P(("goto" ~/ name ~ ";").map(GotoStmnt) |
    ("continue" ~/ integerLiteral ~ ";").map(ContinueStmnt) |
    ("break" ~/ integerLiteral ~ ";").map(BreakStmnt) |
    ("return" ~/ expression.? ~ ";").map(ReturnStmnt) |
    ("throw" ~/ expression ~ ";").map(ThrowStmnt))

  val catchClause : P[CatchClause] = P("catch" ~/ "(" ~ qualifiedName ~ variableName ~ ")" ~ compoundStmnt)
    .map(t => CatchClause(t._1, t._2, t._3))

  val tryStmnt : P[TryStmnt] = P("try" ~ compoundStmnt ~ catchClause.rep ~ ("finally" ~/ compoundStmnt).?)
    .map(t => TryStmnt(t._1, t._2, t._3))

  val declareStmnt : P[DeclareStmnt] = P("----").map(_ => DeclareStmnt())
  //TODO declareStmnt

  val constElem : P[ConstElement] = P(name ~ "=" ~ expression)
    .map(t => ConstElement(t._1, t._2))

  val constDeclStmnt : P[ConstDecl] = P("const" ~ constElem.rep ~ ";")
    .map(ConstDecl)

  val paramType : P[(Option[TypeDecl], Boolean)] = P(typeDecl.? ~ "&".!.?)
    .map(t => (t._1, t._2.isDefined))

  val parameterDecl : P[(Option[TypeDecl], Boolean) => ParameterDecl] = P(("..." ~ variableName).map(name => (a: Option[TypeDecl], b: Boolean) =>  VariadicParam(a, b, name)) |
    (variableName ~ ("=" ~ expression).?).map(t => (a: Option[TypeDecl], b: Boolean) => SimpleParam(a, b, t._1, t._2)))

  val funcHeader : P[FuncHeader] = P("function" ~/ "&".!.? ~ name ~ "(" ~ (paramType ~ parameterDecl).rep(sep=",") ~ ")" ~ (":" ~ possibleType).?)
    .map(t => FuncHeader(t._1.isDefined, t._2, t._3.map(g => g._3(g._1, g._2)), t._4))

  val functionDefStmnt : P[FuncDef] = P(funcHeader ~ compoundStmnt)
    .map(t => FuncDef(t._1, t._2))

  val classConstDecl : P[ClassConstDecl] = P(visibilityMod.? ~ "const" ~ constElem.rep ~ ";")
    .map(t => ClassConstDecl(t._1, t._2))

  val propertyElem : P[PropertyElement] = P(variableName ~ ("=" ~ expression).?)
    .map(t => PropertyElement(t._1, t._2))

  val propertyDecl : P[PropertyDecl] = P(propertyMod ~ propertyElem.rep ~ ";")
    .map(t => PropertyDecl(t._1, t._2))

  val bodyOrEnd: P[Option[CompoundStmnt]] = P(";".!.map(_ => None) | compoundStmnt.map(Some(_)))

  val methodDecl : P[MethodDecl] = P(methodMod.rep ~ funcHeader ~ bodyOrEnd)
    .map(t => MethodDecl(t._1, t._2, t._3))

  val traitUseSpec : P[TraitUseSpec] =
    P((name ~ "insteadof" ~ name).map(t => SelectInsteadofClause(t._1, t._2)) |
      (name ~ "as" ~ visibilityMod.? ~ name).map(t => TraitAliasClause(t._1, t._2, Some(t._3))) |
      (name ~ "as" ~ visibilityMod ~ name.?).map(t => TraitAliasClause(t._1, Some(t._2), t._3)))

  val traitUseSpecs : P[Seq[TraitUseSpec]] = P(";".!.map(_ => Seq()) |
    ("{" ~ traitUseSpec.rep(sep=",") ~ "}"))

  val traitUseClause : P[TraitUseClause] = P("use" ~/ qualifiedName.rep(sep=",") ~ traitUseSpecs)
     .map(t => TraitUseClause(t._1, t._2))

  val classMemberDecl : P[MemberDecl] = classConstDecl | propertyDecl | methodDecl | traitUseClause

  val classDeclStmnt : P[ClassDecl] = P(classMod.? ~ "class" ~/ name ~ ("extends" ~ qualifiedName).? ~
      ("implements" ~ qualifiedName.rep(sep=",")).? ~ "{" ~/ classMemberDecl.rep ~ "}")
      .map(t => ClassDecl(t._1, t._2, t._3, t._4, t._5))

  val interfaceMemberDecl : P[MemberDecl] = classConstDecl | methodDecl

  val interfaceDeclStmnt : P[InterfaceDecl] =
    P("interface" ~/ name ~ ("extends" ~ qualifiedName.rep(sep=",")).? ~ "(" ~ interfaceMemberDecl.rep ~ ")")
      .map(t => InterfaceDecl(t._1, t._2, t._3))

  val traitMemberDecl : P[MemberDecl] = propertyDecl | methodDecl | traitUseClause

  val traitDeclStmnt : P[TraitDecl] =
    P("trait" ~/ name ~ "(" ~ interfaceMemberDecl.rep ~ ")")
      .map(t => TraitDecl(t._1, t._2))

  val namespaceDefStmnt : P[NamespaceDef] = P(("namespace" ~ name ~ ";").map(t => NamespaceDef(Some(t), None)) |
    ("namespace" ~ name.? ~ compoundStmnt).map(t => NamespaceDef(t._1, Some(t._2))))

  val namespaceUseDeclStmnt : P[NamespaceUseDecl] = P("----").map(_ => NamespaceUseDecl())
  //TODO namespace

  val globalDeclStmnt : P[GlobalDecl] = P("global" ~ variableName.rep(sep=",") ~ ";")
    .map(GlobalDecl)

  val staticVarElement : P[StaticVarElement] = P(variableName ~ ("=" ~ expression).?)
    .map(t => StaticVarElement(t._1, t._2))

  val functionStaticDeclStmnt : P[FuncStaticDecl] = P("static" ~ staticVarElement.rep(sep=",") ~ ";")
    .map(FuncStaticDecl)

  val classMod : P[ClassModifier] = P("abstract".!.map(_ => AbstractMod) |
    "final".!.map(_ => FinalMod))

  val visibilityMod : P[VisibilityModifier] = P("public".!.map(_ => PublicMod) |
    "private".!.map(_ => PrivateMod) | "protected".!.map(_ => ProtectedMod))

  val propertyMod : P[PropertyModifier] = P("var".!.map(_ => NoMod) |
    (visibilityMod ~ staticMod.?).map(t => CombinedMod(t._2, Some(t._1))) |
    (staticMod ~ visibilityMod.?).map(t => CombinedMod(Some(t._1), t._2)))

  val staticMod : P[StaticModifier] = P("static".!).map(_ => StaticMod)

  val methodMod : P[MethodModifier] = P(visibilityMod | staticMod | classMod)

}
