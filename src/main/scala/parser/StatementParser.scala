package parser

import fastparse.noApi._
import WsAPI._
import ExpressionParser.expression
import PHPParser._
import ast.Ast._

import scala.collection.immutable.Stream.Empty

object StatementParser {

  def statement : P[Statement] = if(isTagProcessed) possibleStatements else echoTagStmnt

  val possibleStatements : P[Statement] = P(compoundStmnt | namedLabelStmnt | selectionStmnt | selectionStmnt | iterationStmnt | jumpStmnt | tryStmnt |
      declareStmnt | constDeclStmnt | functionDefStmnt | classDeclStmnt | interfaceDeclStmnt | traitDeclStmnt |
      namespaceDefStmnt | namespaceUseDeclStmnt | globalDeclStmnt | functionStaticDeclStmnt | emptyStmnt | expStmnt).log()

  val emptyStmnt : P[EmptyStmnt] = P(semicolonFactory).map(EmptyStmnt)

  val compoundStmnt : P[CompoundStmnt] = P("{" ~/ statement.rep ~ "}").map(CompoundStmnt)

  def echoTagStmnt : P[EchoTagStmnt] = {
    isTagProcessed = true
    P(echoStartTag ~ expression.rep(min=1, sep=",") ~ semicolonFactory).map(t => EchoTagStmnt(t._2, t._3))
  }

  val namedLabelStmnt : P[NamedLabelStmnt] = P(name ~ ":" ~ statement).map(t => NamedLabelStmnt(t._1, t._2))

  val expStmnt : P[ExpressionStmnt] = P(expression ~ semicolonFactory).map(t => ExpressionStmnt(t._1, t._2))

  val selectionStmnt : P[SelectionStmnt] = P(ifStmnt | switchStmnt)

  val ifBody : P[(Seq[Statement], Seq[(Expression, Seq[Statement])], Option[Seq[Statement]], Option[Text])] =
    P((":" ~ (statement).rep(1) ~ ("elseif" ~ "(" ~ expression ~ ")" ~ ":" ~ statement.rep(1)).rep ~ ("else" ~ ":" ~ statement.rep).? ~ "endif" ~ semicolonFactory) |
      (statement ~ ("elseif" ~ "(" ~ expression ~ ")" ~ statement).rep ~ ("else" ~ statement).?).map(t => (Seq(t._1), t._2.map(e => (e._1, Seq(e._2))), t._3.map(Seq(_)), None)))

  val ifStmnt : P[IfStmnt] =
    P("if" ~/ "(" ~ expression ~ ")" ~ ifBody).map(t => IfStmnt(t._1, t._2._1, t._2._2, t._2._3, t._2._4))

  val switchBody : P[(Seq[SwitchBlock], Option[Text])] =
    P((":" ~ (caseBlock | defaultBlock).rep ~ "endswitch" ~ semicolonFactory) |
      ("{" ~ (caseBlock | defaultBlock).rep ~ "}").map(t => (t, None)))

  val switchStmnt : P[SwitchStmnt] =
    P("switch" ~/ "(" ~ expression ~ ")" ~ switchBody).map(t => SwitchStmnt(t._1, t._2._1, t._2._2))

  val caseBlock : P[CaseBlock] =
    P("case" ~/ expression ~ (":".!.map(_ => Seq()) | emptyStmnt.map(Seq(_))) ~ statement.rep).map(t => CaseBlock(t._1, t._2 ++ t._3))

  val defaultBlock : P[DefaultBlock] =
    P("default" ~/ (":".!.map(_ => Seq()) | emptyStmnt.map(Seq(_))) ~ statement.rep).map(t => DefaultBlock(t._1 ++ t._2))

  val iterationStmnt : P[IterationStmnt] = P(whileStmnt | doStmnt | forStmnt | foreachStmnt)

  val whileBody : P[(Seq[Statement], Option[Text])] =
    P((":" ~ statement.rep ~ "endwhile" ~ semicolonFactory) |
      statement.map(t => (Seq(t), None)))

  val whileStmnt : P[WhileStmnt] =
    P("while" ~/ "(" ~ expression ~ ")" ~ whileBody).map(t => WhileStmnt(t._1, t._2._1, t._2._2))

  val doStmnt : P[DoStmnt] =
    P("do" ~/ statement ~ "while" ~/ "(" ~ expression ~ ")" ~ semicolonFactory).map(t => DoStmnt(t._2, t._1, t._3))

  val forBody : P[(Seq[Statement], Option[Text])] =
    P(":" ~ statement.rep ~ "endfor" ~ semicolonFactory | statement.map(t => (Seq(t), None)))

  val forExpressionList : P[ForExpressionList] = P(expression.rep(sep=",") ~ semicolonFactory).map(t => ForExpressionList(t._1, t._2))

  val forStmnt : P[ForStmnt] = P("for" ~/ "(" ~ forExpressionList ~ forExpressionList ~ expression.rep(sep=",") ~ ")" ~ forBody).map(t => ForStmnt(t._1, t._2, ForExpressionList(t._3, None), t._4._1, t._4._2))

  val foreachBody : P[Seq[Statement]] = P((":" ~ statement.rep ~ "foreach" ~ ";") | statement.map(Seq(_)))

  val foreachStmnt : P[ForeachStmnt] = P("----").map(_ => ForeachStmnt())
  //TODO missing foreach

  val jumpStmnt : P[JumpStmnt] = P(("goto" ~/ name ~ semicolonFactory).map(t => GotoStmnt(t._1, t._2)) |
    ("continue" ~/ integerLiteral ~ semicolonFactory).map(t => ContinueStmnt(t._1, t._2)) |
    ("break" ~/ integerLiteral ~ semicolonFactory).map(t => BreakStmnt(t._1, t._2)) |
    ("return" ~/ expression.? ~ semicolonFactory).map(t => ReturnStmnt(t._1, t._2)) |
    ("throw" ~/ expression ~ semicolonFactory).map(t => ThrowStmnt(t._1, t._2)))

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

  val classMemberDecl : P[MemberDecl] = P(classConstDecl | propertyDecl | methodDecl | traitUseClause)

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
