package parser

import ast.Expressions._

import parser.Basic._
import parser.Lexical.ws

import fastparse.noApi._
import parser.WsAPI._



object ExpressionParser {
  val expression : P[Expression] = P(yieldExp | requireExp | requireOnceExp | includeExp | includeOnceExp | logicalOrExpr2)

  val requireExp : P[RequireExp] = P("require" ~ expression).map(RequireExp)

  val requireOnceExp : P[RequireOnceExp] = P("require_once" ~ expression).map(RequireOnceExp)

  val includeExp : P[IncludeExp] = P("include" ~ expression).map(IncludeExp)

  val includeOnceExp : P[IncludeOnceExp] = P("include_once" ~ expression).map(IncludeOnceExp)

  val yieldExp : P[Expression] = P(("yield" ~~ ws ~ "from" ~ arrayElementInitializer).map(YieldFromExp) |
    ("yield" ~ expression).map(YieldExp))

  val logicalOrExpr2 : P[Expression] = P(logicalXOrExp.rep(sep="or", min=1)).map(_.reduceLeft(LogicalOrExp2))

  val logicalXOrExp : P[Expression] = P(logicalAndExp2.rep(sep="xor", min=1)).map(_.reduceLeft(LogicalXOrExp))

  val logicalAndExp2 : P[Expression] = P(assignmentCondExp.rep(sep="and", min=1)).map(_.reduceLeft(LogicalAndExp2))

  val conditionalExpFactory : P[Expression => Expression] = P(("??" ~ expression).map(e => (x: Expression) => CoalesceExp(x,e)) |
    ("?" ~ expression.? ~ ":" ~ assignmentCondExp).map(e => (x) => TernaryExp(x,e._1, e._2)))

  val assignmentCondExp : P[Expression] = P(//(listIntrinsic ~ "=" ~ assignmentExp).map(t => SimpleAssignmentExp(t._1, t._2)) |
    (variable ~ assignmentFactory).map(t => t._2(t._1)) |
    (logicalOrExp ~ conditionalExpFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))
  //TODO

  val assignmentFactory : P[Variable => Expression] = P(
    ("=" ~ "&".!.? ~ assignmentCondExp).map(e => (x: Variable) => SimpleAssignmentExp(e._1.isDefined, x, e._2)) |
    (assignmentOp ~~ "=" ~ assignmentCondExp).map(e => (x: Variable) => CompoundAssignmentExp(e._1, x, e._2)))

  val logicalOrExp: P[Expression] = P(logicalAndExp.rep(sep="||", min=1)).map(_.reduceLeft(LogicalOrExp))
  val logicalAndExp: P[Expression] = P(bitwiseOrExp.rep(sep="&&", min=1)).map(_.reduceLeft(LogicalAndExp))
  val bitwiseOrExp: P[Expression] = P(bitwiseXOrExp.rep(sep="|", min=1)).map(_.reduceLeft(BitwiseOrExp))
  val bitwiseXOrExp: P[Expression] = P(bitwiseAndExp.rep(sep="^", min=1)).map(_.reduceLeft(BitwiseXOrExp))
  val bitwiseAndExp: P[Expression] = P(equalityExp.rep(sep="&", min=1)).map(_.reduceLeft(BitwiseAndExp))

  val equalityExp: P[Expression] = P(
    relationalExp ~ (equalityOp ~ relationalExp).rep).map(t => t._2.foldLeft(t._1)((exp, op) => EqualityExp(op._1, exp, op._2)))
  val relationalExp: P[Expression] = P(
    shiftExp ~ (relationalOp ~ shiftExp).rep).map(t => t._2.foldLeft(t._1)((exp, op) => RelationalExp(op._1, exp, op._2)))

  val shiftExp: P[Expression] = P(additiveExp ~ shiftFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val shiftFactory : P[Expression => Expression] = P(("<<" ~ additiveExp).map(e => (x: Expression) => LShiftExp(x, e)) |
    (">>" ~ additiveExp).map(e => (x: Expression) => RShiftExp(x, e)))

  val additiveExp: P[Expression] = P(multExp ~ additiveFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val additiveFactory : P[Expression => Expression] = P(("+" ~ multExp).map(e => (x: Expression) => AddExp(x, e)) |
    ("-" ~ multExp).map(e => (x: Expression) => SubExp(x, e)) |
    ("." ~ multExp).map(e => (x) => SubExp(x, e)))

  val multExp: P[Expression] = P(exponentiationExp ~ multFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val multFactory : P[Expression => Expression] = P(("*" ~ exponentiationExp).map(e => (x: Expression) => MulExp(x, e)) |
    ("/" ~ exponentiationExp).map(e => (x: Expression) => DivExp(x, e)) |
    ("%" ~ exponentiationExp).map(e => (x) => ModExp(x, e)))

  val exponentiationExp: P[Expression] = P(
    instanceOfExp ~ ("**" ~ expression).?).map(t => if(t._2.isDefined) ExponentiationExp(t._1, t._2.get) else t._1)

  val instanceOfExp : P[Expression] = P(unaryExp ~ ("instanceof" ~ (expression.map(Left(_)) |
    qualifiedName.map(Right(_)))).?).map(t => if(t._2.isDefined) InstanceOfExp(t._1, t._2.get) else t._1)

  val unaryExp : P[Expression] = P(prefixIncrementExp | prefixDecrementExp |
    unaryOpExp | errorControlExp | shellCommandExp | castExp | postfixExp)

  val prefixIncrementExp : P[Expression] = P("++" ~ variable).map(PrefixIncrementExp)

  val prefixDecrementExp : P[Expression] = P("--" ~ variable).map(PrefixDecrementExp)

  val unaryOpExp : P[Expression] = P(unaryOp ~ unaryExp).map(t => UnaryOpExp(t._1,t._2))

  val errorControlExp : P[Expression] = P("@" ~ expression).map(ErrorControlExp)

  val shellCommandExp : P[Expression] = P("`" ~ dqCharSequence ~ "`").map(ShellCommandExp)

  val castExp : P[Expression] = P("(" ~ castType ~ ")" ~ expression).map(t => CastExp(t._1, t._2))

  val castType : P[CastType.Value] = P("array".!.map(_ => CastType.ARRAY) | "int".!.map(_ => CastType.INT))
  //TODO

  val postfixOperatorFactory : P[Variable => Expression] = P(
    "++".!.map(_ => (x) => PostfixIncrementExp(x)) |
    "--".!.map(_ => (x) => PostfixDecrementExp(x)))

  val postfixExp : P[Expression] = P(primaryExpWithoutVariable | cloneExp | objectCreationExp |
    (variable ~ postfixOperatorFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1))

  val cloneExp : P[Expression] = P("clone" ~ expression).map(CloneExp)

  val arrayElementInitializer : P[ArrayElementInitializer] = P("hi".!.map(_ => ArrayElementInitializer(None, SpecialExp(), true)))
  val objectCreationExp : P[Expression] = P(Fail)
  //TODO

  val primaryExpWithoutVariable : P[Expression] = P(classConstAccExp | constAccExp |
    literal | arrayCreationExp | intrinsic | anonymousFuncExp | enclosedExp)

  //TODO
  val classConstAccExp : P[Expression] = P(Fail)
  val constAccExp : P[Expression] = P(Fail)
  val arrayCreationExp : P[Expression] = P(Fail)
  val intrinsicOperator : P[Expression] = P(Fail)
  val intrinsicConstruct : P[Expression] = P(echoIntrinsic | Fail)
  val echoIntrinsic : P[EchoIntrinsic] = P("echo" ~ expression.rep(min=1, sep=",")).map(EchoIntrinsic)
  val intrinsic : P[Expression] = P(intrinsicOperator | intrinsicConstruct)
  val anonymousFuncExp : P[Expression] = P(Fail)
  val enclosedExp : P[Expression] = P(Fail)

  val variableName : P[SimpleNameVar] = P("$" ~ name).map(SimpleNameVar)

  val simpleVariable : P[SimpleVar] = P(("$" ~ simpleVariable).map(SimpleAccessVar) |
    ("$" ~ "{" ~ expression ~ "}").map(SimpleExpVar) | variableName)

  val scopeAccVar : P[ScopeAccessVar] = P(selfScope | parentScope | staticScope).map(ScopeAccessVar)
  val qualifiedNameVar : P[QualifiedNameVar] = P(qualifiedName).map(QualifiedNameVar)

  val memberName : P[MemberName] = P(name.map(NameMember) | simpleVariable.map(SimpleVarMember) | ("{" ~ expression ~ "}").map(ExpMember))

  val argumentExpressionList : P[Seq[ArgumentExpression]] = P(argumentExp.rep(sep=","))

  val argumentExp: P[ArgumentExpression] = P("...".!.? ~ assignmentCondExp).map(t => ArgumentExpression(t._1.isDefined, t._2))

  val stringLiteralVar : P[Variable] = P(stringLiteral).map(StringLiteralVar)

  val expressionVar : P[ExpressionVar] = P("(" ~ expression ~ ")").map(ExpressionVar)

  val arrayCreationVar = P(Fail)

  val memberCallStaticAccFactory : P[Variable => Variable] = P("::" ~ memberName ~ "(" ~ argumentExpressionList ~ ")").map(t => (x: Variable) => MemberCallStaticAcc(x, t._1, t._2))
  val simpleVarStaticAccFactory : P[Variable => Variable] = P("::" ~ simpleVariable).map(t => (x: Variable) => SimpleVarStaticAcc(x, t))
  val arrayAccFactory : P[Variable => Variable] = P("[" ~ expression.? ~ "]").map(t => (x: Variable) => ArrayAcc(x, t))
  val blockAccFactory : P[Variable => Variable] = P("{" ~ expression ~ "}").map(t => (x: Variable) => BlockAcc(x, t))
  val memberPropertyAccFactory : P[Variable => Variable] = P("->" ~ memberName ~ ("(" ~ argumentExpressionList ~ ")").?).map(t =>
    if(t._2.isDefined) (x) => MemberCallPropertyAcc(x, t._1, t._2.get)
    else (x) => MemberPropertyAcc(x, t._1)
  )

  val directCallAccFactory : P[Variable => Variable] = P("(" ~ argumentExpressionList ~ ")").map(t => (x: Variable) => CallAccessor(x, t))

  val qualifiedNameVarWithCall : P[Variable] = P(qualifiedNameVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)
  val expressionVarWithCall : P[Variable] = P(expressionVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)

  val singleVariable : P[Variable] = P(simpleVariable | stringLiteralVar | scopeAccVar | qualifiedNameVarWithCall | expressionVarWithCall | arrayCreationVar)
  val variable : P[Variable] = P(singleVariable ~ (memberCallStaticAccFactory | arrayAccFactory | blockAccFactory | memberPropertyAccFactory).rep).map(t => t._2.foldLeft(t._1)((a,b) => b(a)))

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
