package parser

import ast.Expressions._
import ast.Statements.ClassDecl
import Basic._
import parser.literals.Keywords._
import parser.literals.Lexical.ws
import parser.literals.KeywordConversions._
import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Literals._
import parser.statements.DeclarationParser.classDeclBody
  import parser.expressions.VariableParser.{variable, arrayElement}

object ExpressionParser {
  val expression : P[Expression] = P(yieldExp | requireOnceExp | requireExp | includeOnceExp | includeExp | logicalOrExpr2)

  val space = P(&(ws | "("))

  val requireExp : P[RequireExp] = P(REQUIRE ~~ space ~/ expression).map(RequireExp)
  val requireOnceExp : P[RequireOnceExp] = P(REQUIRE_ONCE ~~ space ~/ expression).map(RequireOnceExp)
  val includeExp : P[IncludeExp] = P(INCLUDE ~~ space ~/ expression).map(IncludeExp)
  val includeOnceExp : P[IncludeOnceExp] = P(INCLUDE_ONCE ~~ space ~/ expression).map(IncludeOnceExp)
  val yieldExp : P[Expression] = P(
    (YIELD ~~ ws ~ FROM ~~ space ~/ expression).map(YieldFromExp)
      | (YIELD ~~ space ~/ arrayElement).map(YieldExp))

  val logicalOrExpr2 : P[Expression] = P(logicalXOrExp.rep(sep=OR.~/, min=1)).map(_.reduceLeft(LogicalOrExp2))
  val logicalXOrExp : P[Expression] = P(logicalAndExp2.rep(sep=XOR.~/, min=1)).map(_.reduceLeft(LogicalXOrExp))
  val logicalAndExp2 : P[Expression] = P(assignmentCondExp.rep(sep=AND.~/, min=1)).map(_.reduceLeft(LogicalAndExp2))

  val conditionalExpFactory : P[Expression => Expression] = P(
    ("??" ~/ expression).map(e => (x: Expression) => CoalesceExp(x,e)) |
    ("?" ~~ !">" ~/ expression.? ~ ":" ~/ assignmentCondExp).map(e => (x) => TernaryExp(x,e._1, e._2)))

  val assignmentCondExp : P[Expression] = P(//(listIntrinsic ~ "=" ~ assignmentExp).map(t => SimpleAssignmentExp(t._1, t._2)) |
    //(NoCut(variable) ~ assignmentFactory).map(t => t._2(t._1)) |
    (logicalOrExp ~ conditionalExpFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))
  //TODO

  val assignmentFactory : P[Variable => Expression] = P(
    ("=" ~ "&".!.? ~ assignmentCondExp).map(e => (x: Variable) => SimpleAssignmentExp(e._1.isDefined, x, e._2)) |
    (assignmentOp ~~ "=" ~/ assignmentCondExp).map(e => (x: Variable) => CompoundAssignmentExp(e._1, x, e._2)))

  val logicalOrExp: P[Expression] = P(logicalAndExp.rep(sep="||".~/, min=1)).map(_.reduceLeft(LogicalOrExp))
  val logicalAndExp: P[Expression] = P(bitwiseOrExp.rep(sep="&&".~/, min=1)).map(_.reduceLeft(LogicalAndExp))
  val bitwiseOrExp: P[Expression] = P(bitwiseXOrExp.rep(sep=("|" ~~ !"|").~/, min=1)).map(_.reduceLeft(BitwiseOrExp))
  val bitwiseXOrExp: P[Expression] = P(bitwiseAndExp.rep(sep="^".~/, min=1)).map(_.reduceLeft(BitwiseXOrExp))
  val bitwiseAndExp: P[Expression] = P(equalityExp.rep(sep=("&" ~~ !"&").~/, min=1)).map(_.reduceLeft(BitwiseAndExp))

  val equalityExp: P[Expression] = P(relationalExp ~ (equalityOp ~/ relationalExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => EqualityExp(op._1, exp, op._2)))
  val relationalExp: P[Expression] = P(shiftExp ~ (relationalOp ~ shiftExp).rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => RelationalExp(op._1, exp, op._2)))

  val shiftExp: P[Expression] = P(additiveExp ~ shiftFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val shiftFactory : P[Expression => Expression] = P((
    "<<" ~~ !"=" ~~ !"<" ~/ additiveExp).map(e => (x: Expression) => LShiftExp(x, e)) |
    (">>" ~~ !"=" ~/ additiveExp).map(e => (x: Expression) => RShiftExp(x, e)))

  val additiveExp: P[Expression] = P(multExp ~ additiveFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val additiveFactory : P[Expression => Expression] = P((
    "+" ~/ multExp).map(e => (x: Expression) => AddExp(x, e)) |
    ("-" ~/ multExp).map(e => (x: Expression) => SubExp(x, e)) |
    ("." ~/ multExp).map(e => (x) => SubExp(x, e)))

  val multExp: P[Expression] = P(exponentiationExp ~ multFactory.rep)
    .map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val multFactory : P[Expression => Expression] = P((
    "*" ~/ exponentiationExp).map(e => (x: Expression) => MulExp(x, e)) |
    ("/" ~/ exponentiationExp).map(e => (x: Expression) => DivExp(x, e)) |
    ("%" ~/ exponentiationExp).map(e => (x) => ModExp(x, e)))

  val exponentiationExp: P[Expression] = P(instanceOfExp ~ ("**" ~/ expression).?)
    .map(t => if(t._2.isDefined) ExponentiationExp(t._1, t._2.get) else t._1)

  val instanceOfExp : P[Expression] = P(unaryExp ~~
    (ws ~ INSTANCEOF ~~ space ~ (qualifiedName.map(Right(_)) | expression.map(Left(_)))).?
  ).map(t => if(t._2.isDefined) InstanceOfExp(t._1, t._2.get) else t._1)

  val unaryExp : P[Expression] = P(prefixIncrementExp | prefixDecrementExp |
    unaryOpExp | errorControlExp | shellCommandExp | castExp | postfixExp)

  val prefixIncrementExp : P[Expression] = P("++" ~/ variable).map(PrefixIncrementExp)
  val prefixDecrementExp : P[Expression] = P("--" ~/ variable).map(PrefixDecrementExp)
  val unaryOpExp : P[Expression] = P(unaryOp ~ unaryExp).map(t => UnaryOpExp(t._1,t._2))
  val errorControlExp : P[Expression] = P("@" ~/ expression).map(ErrorControlExp)
  val shellCommandExp : P[Expression] = P("`" ~~ dqCharSequence ~~ "`").map(ShellCommandExp)

  val castExp : P[Expression] = P("(" ~ castType ~ ")" ~/ expression).map(t => CastExp(t._1, t._2))
  val castType : P[CastType.Value] = P(arrayCastType | binaryCastType | booleanCastType | boolCastType |
    doubleCastType | integerCastType | intCastType | floatCastType | objectCastType |
    realCastType | stringCastType | unsetCastType)

  val postfixOperatorFactory : P[Variable => Expression] =
    P("++".!.map(_ => (x: Variable) => PostfixIncrementExp(x)) |
      "--".!.map(_ => (x: Variable) => PostfixDecrementExp(x)) |
      "::" ~ name.map(n => (x: Variable) => ClassConstAcc(x, n)) |
    assignmentFactory)

  val postfixExp : P[Expression] = P(listAssignment | primaryExpWithoutVariable | cloneExp | objectCreationExp |
    (variable ~/ postfixOperatorFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))

  val listAssignment : P[Expression] = P(NoCut(listIntrinsic) ~ "=" ~/ assignmentCondExp).map(t => ListAssignmentExp(t._1, t._2))
  val cloneExp : P[Expression] = P(CLONE ~~ space ~/ expression).map(CloneExp)

  val objectCreationExp : P[Expression] = P(NEW ~~ ws ~/
    ((CLASS ~~ &("(" | "{" | ws) ~/ ("(" ~/ argumentExpressionList ~ ")").? ~ classDeclBody)
      .map(t => AnonymousClassCreationExp(ClassDecl(None, None, t._2._1, t._2._2, t._2._3), t._1)) |
    ((qualifiedName.map(Left(_)) | expression.map(Right(_))) ~ ("(" ~/ argumentExpressionList ~ ")").?)
      .map(t => InstanceCreationExp(t._1, t._2)))
  )

  val primaryExpWithoutVariable : P[Expression] = P(constAccExp |
    literal | intrinsic | anonymousFuncExp | enclosedExp)

  //unnecessary
  val constAccExp : P[Expression] = P(Fail)

  val listIntrinsic : P[ListIntrinsic] = P(LIST ~ "(" ~/ ((
    ",".? ~ NoCut(expression).rep(sep=",".rep(1))).map(Left(_)) |
    ((expression ~ "=>" ~ expression).rep(sep=",") ~ ",".?).map(Right(_))) ~ ")"
  ).map(ListIntrinsic)
  val echoIntrinsic : P[EchoIntrinsic] = P(ECHO ~~ space ~/ expression.rep(min=1, sep=",")).map(EchoIntrinsic)
  val unsetIntrinsic : P[UnsetIntrinsic] = P(UNSET ~ "(" ~ variable.rep(1, sep=",") ~ ")").map(UnsetIntrinsic)

  val emptyIntrinsic : P[EmptyIntrinsic] = P(EMPTY ~ "(" ~/ expression ~ ")").map(EmptyIntrinsic)
  val evalIntrinsic : P[EvalIntrinsic] = P(EVAL ~ "(" ~/ expression ~ ")").map(EvalIntrinsic)
  val exitIntrinsic : P[ExitIntrinsic] = P(EXIT ~ ("(" ~/ expression.? ~ ")").? |
    (DIE ~ ("(" ~/ expression.? ~ ")").?)).map(t => ExitIntrinsic(t.getOrElse(None)))
  val issetIntrinsic : P[IssetIntrinsic] = P(ISSET ~ "(" ~/ variable.rep(1, sep=",") ~ ")").map(IssetIntrinsic)
  val printIntrinsic : P[PrintIntrinsic] = P(PRINT ~~ space ~/ expression).map(PrintIntrinsic)

  val intrinsicConstruct : P[Expression] = P(echoIntrinsic | unsetIntrinsic | listIntrinsic)
  val intrinsicOperator : P[Expression] = P(emptyIntrinsic | evalIntrinsic | exitIntrinsic | issetIntrinsic | printIntrinsic)
  val intrinsic : P[Expression] = P(intrinsicOperator | intrinsicConstruct)

  val anonymousFuncExp : P[Expression] = P(Fail)
  val enclosedExp : P[Expression] = P("(" ~ expression ~ ")")

  val argumentExpressionList : P[Seq[ArgumentExpression]] = P(
    argumentExp.rep(sep=","))

  val argumentExp: P[ArgumentExpression] = P(
    "...".!.? ~ assignmentCondExp
  ).map(t => ArgumentExpression(t._1.isDefined, t._2))


  //unused part

  //val expression : P[Expression] = P(exp1).map(_ => SpecialExp())

  def exp1 : P[Expression] = P((randomOutside.? ~ "(" ~ exp2.? ~ ")" ~ exp1.?).map(_ => SpecialExp()) |
    (randomOutside.? ~ "{" ~ exp2.? ~ "}" ~  exp1.?) .map(_ => SpecialExp()) |
    (randomOutside.? ~ "[" ~ exp2.? ~ "]" ~ exp1.?) .map(_ => SpecialExp()) |
    (randomOutside ~ (":".rep(1) ~ exp1).?) .map(_ => SpecialExp()))

  def exp2 : P[Expression] = P((random.? ~ "(" ~ exp2.? ~ ")" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "{" ~ exp2.? ~ "}" ~ exp2.?) .map(_ => SpecialExp()) |
    (random.? ~ "[" ~ exp2.? ~ "]" ~ exp2.?) .map(_ => SpecialExp()) |
    (random ~ (":".rep(1) ~ exp2).?) .map(_ => SpecialExp()))

  //val random1 : P[Expression] = """[^\(\)\{\}\[\]:]+""".r ^^^ SpecialExp()

  //val randomOutside1 : P[Expression] = """[^\(\)\{\}\[\]:,;]+""".r ^^^ SpecialExp()

  def charSeqInside = "(){}[]:"
  def charSeqOutside = "(){}[]:,;"

  def random = P(CharsWhile(charSeqInside.indexOf(_) == -1, 1))
  def randomOutside = P(CharsWhile(charSeqOutside.indexOf(_) == -1, 1))

  //end unused part
}
