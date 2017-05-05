package parser

import fastparse.noApi._
import WsAPI._
import PHPParser._
import ast.Ast._

/**
  * Created by tobias on 03.05.17.
  */
object ExpressionParser {
  def expression : P[Expression] = P(yieldExp | requireExp | requireOnceExp | includeExp | includeOnceExp | logicalOrExpr2).log()

  def requireExp : P[RequireExp] = P("require" ~ expression).map(RequireExp)

  def requireOnceExp : P[RequireOnceExp] = P("require_once" ~ expression).map(RequireOnceExp)

  def includeExp : P[IncludeExp] = P("include" ~ expression).map(IncludeExp)

  def includeOnceExp : P[IncludeOnceExp] = P("include_once" ~ expression).map(IncludeOnceExp)

  def yieldExp : P[Expression] = P(("yield" ~ "from" ~ arrayElementInitializer).map(YieldFromExp) |
    ("yield" ~ expression).map(YieldExp))

  def logicalOrExpr2 : P[Expression] = P(logicalXOrExp.rep(sep="or", min=1)).map(_.reduceLeft(LogicalOrExp2)).log()

  def logicalXOrExp : P[Expression] = P(logicalAndExp2.rep(sep="xor", min=1)).map(_.reduceLeft(LogicalXOrExp)).log()

  def logicalAndExp2 : P[Expression] = P(assignmentCondExp.rep(sep="and", min=1)).map(_.reduceLeft(LogicalAndExp2))

  def conditionalExpFactory : P[Expression => Expression] = P(("??" ~ expression).map(e => (x: Expression) => CoalesceExp(x,e)) |
    ("?" ~ expression.? ~ ":" ~ assignmentCondExp).map(e => (x) => TernaryExp(x,e._1, e._2)))

  def assignmentCondExp : P[Expression] = P((logicalOrExp ~ conditionalExpFactory).map(t => t._2(t._1)).log() |
    //(listIntrinsic ~ "=" ~ assignmentExp).map(t => SimpleAssignmentExp(t._1, t._2)) |
    (variable ~ assignmentFactory).map(t => t._2(t._1)).log() | logicalOrExp)

  def assignmentFactory : P[Variable => Expression] = P(("=" ~ "&".!.? ~ assignmentCondExp).map(e => (x: Variable) => SimpleAssignmentExp(e._1.isDefined, x, e._2)).log() |
    (assignmentOp ~~ "=" ~ assignmentCondExp).map(e => (x: Variable) => CompoundAssignmentExp(e._1, x, e._2)))

  def logicalOrExp: P[Expression] = P(logicalAndExp.rep(sep="||", min=1)).map(_.reduceLeft(LogicalOrExp))
  def logicalAndExp: P[Expression] = P(bitwiseOrExp.rep(sep="&&", min=1)).map(_.reduceLeft(LogicalAndExp))
  def bitwiseOrExp: P[Expression] = P(bitwiseXOrExp.rep(sep="|", min=1)).map(_.reduceLeft(BitwiseOrExp))
  def bitwiseXOrExp: P[Expression] = P(bitwiseAndExp.rep(sep="^", min=1)).map(_.reduceLeft(BitwiseXOrExp))
  def bitwiseAndExp: P[Expression] = P(equalityExp.rep(sep="&", min=1)).map(_.reduceLeft(BitwiseAndExp))

  def equalityExp: P[Expression] = P(relationalExp ~ (equalityOp ~ relationalExp).rep).map(t => t._2.foldLeft(t._1)((exp, op) => EqualityExp(op._1, exp, op._2)))
  def relationalExp: P[Expression] = P(shiftExp ~ (relationalOp ~ shiftExp).rep).map(t => t._2.foldLeft(t._1)((exp, op) => RelationalExp(op._1, exp, op._2)))

  def shiftExp: P[Expression] = P(additiveExp ~ shiftFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp))).log()
  val shiftFactory : P[Expression => Expression] = P(("<<" ~ additiveExp).map(e => (x: Expression) => LShiftExp(x, e)) | (">>" ~ additiveExp).map(e => (x: Expression) => RShiftExp(x, e)))

  def additiveExp: P[Expression] = P(multExp ~ additiveFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val additiveFactory : P[Expression => Expression] = P(("+" ~ multExp).map(e => (x: Expression) => AddExp(x, e)) | ("-" ~ multExp).map(e => (x: Expression) => SubExp(x, e)) | ("." ~ multExp).map(e => (x) => SubExp(x, e)))

  def multExp: P[Expression] = P(exponentiationExp ~ multFactory.rep).map(t => t._2.foldLeft(t._1)((exp, op) => op(exp)))
  val multFactory : P[Expression => Expression] = P(("*" ~ exponentiationExp).map(e => (x: Expression) => MulExp(x, e)) | ("/" ~ exponentiationExp).map(e => (x: Expression) => DivExp(x, e)) | ("%" ~ exponentiationExp).map(e => (x) => ModExp(x, e)))

  def exponentiationExp: P[Expression] = P(instanceOfExp ~ ("**" ~ expression).?).map(t => if(t._2.isDefined) ExponentiationExp(t._1, t._2.get) else t._1)

  def instanceOfExp : P[Expression] = P(unaryExp ~ ("instanceof" ~ (expression.map(Left(_)) | qualifiedName.map(Right(_)))).?).map(t => if(t._2.isDefined) InstanceOfExp(t._1, t._2.get) else t._1).log()

  def unaryExp : P[Expression] = P(prefixIncrementExp | prefixDecrementExp |
    unaryOpExp | errorControlExp | shellCommandExp | castExp | postfixExp).log()

  def prefixIncrementExp : P[Expression] = P("++" ~ variable).map(PrefixIncrementExp)

  def prefixDecrementExp : P[Expression] = P("--" ~ variable).map(PrefixDecrementExp)

  def unaryOpExp : P[Expression] = P(unaryOp ~ unaryExp).map(t => UnaryOpExp(t._1,t._2))

  def errorControlExp : P[Expression] = P("@" ~ expression).map(ErrorControlExp)

  def shellCommandExp : P[Expression] = P("`" ~ dqCharSequence ~ "`").map(ShellCommandExp)

  def dqCharSequence : P[String] = Fail //TODO

  def castExp : P[Expression] = P("(" ~ castType ~ ")" ~ expression).map(t => CastExp(t._1, t._2))

  def castType : P[CastType.Value] = P("array".!.map(_ => CastType.ARRAY) | "int".!.map(_ => CastType.INT))
  //TODO

  def postfixExp : P[Expression] = P(primaryExp | cloneExp | objectCreationExp | postfixIncrementExp | postfixDecrementExp)
  //TODO

  def cloneExp : P[Expression] = P(Fail)
  def postfixIncrementExp : P[Expression] = P(Fail)
  def postfixDecrementExp : P[Expression] = P(Fail)
  def arrayElementInitializer : P[ArrayElementInitializer] = P(Fail)
  def objectCreationExp : P[Expression] = P(Fail)
  //TODO

  //def postfixIncrementExp : P[Expression] = variable <~ "++" ^^ {case va => PostfixIncrementExp(va)}

  //def postfixDecrementExp : P[Expression] = variable <~ "--" ^^ {case va => PostfixIncrementExp(va)}

  //def cloneExp : P[Expression] = "clone" ~> expression ^^ {case exp => CloneExp(exp)}

  def primaryExp : P[Expression] = P(variable | classConstAccExp | constAccExp |
    literal | arrayCreationExp | intrinsic | anonymousFuncExp | enclosedExp)

  //TODO
  def classConstAccExp : P[Expression] = P(Fail)
  def constAccExp : P[Expression] = P(Fail)
  def arrayCreationExp : P[Expression] = P(Fail)
  def intrinsic : P[Expression] = P(Fail)
  def anonymousFuncExp : P[Expression] = P(Fail)
  def enclosedExp : P[Expression] = P(Fail)

  def variableName : P[SimpleNameVar] = P("$" ~ name).map(SimpleNameVar)

  def simpleVariable : P[SimpleVar] = P(("$" ~ simpleVariable).map(SimpleAccessVar) |
    ("$" ~ "{" ~ expression ~ "}").map(SimpleExpVar) | variableName)

  def scopeAccVar : P[ScopeAccessVar] = P(selfScope | parentScope | staticScope).map(ScopeAccessVar)
  def qualifiedNameVar : P[QualifiedNameVar] = P(qualifiedName).map(QualifiedNameVar).log()

  def memberName : P[MemberName] = P(name.map(NameMember) | simpleVariable.map(SimpleVarMember) | ("{" ~ expression ~ "}").map(ExpMember))

  def argumentExpressionList : P[Seq[ArgumentExpression]] = P(argumentExp.rep(sep=",")).log()

  def argumentExp: P[ArgumentExpression] = P("...".!.? ~ assignmentCondExp).map(t => ArgumentExpression(t._1.isDefined, t._2))

  def stringLiteralVar : P[Variable] = P(stringLiteral).map(StringLiteralVar).log()

  def expressionVar : P[ExpressionVar] = P("(" ~ expression ~ ")").map(ExpressionVar)

  def arrayCreationVar = Fail

  def memberCallStaticAccFactory : P[Variable => Variable] = P("::" ~ memberName ~ "(" ~ argumentExpressionList ~ ")").map(t => (x: Variable) => MemberCallStaticAcc(x, t._1, t._2))
  def simpleVarStaticAccFactory : P[Variable => Variable] = P("::" ~ simpleVariable).map(t => (x: Variable) => SimpleVarStaticAcc(x, t))
  def arrayAccFactory : P[Variable => Variable] = P("[" ~ expression.? ~ "]").map(t => (x: Variable) => ArrayAcc(x, t))
  def blockAccFactory : P[Variable => Variable] = P("{" ~ expression ~ "}").map(t => (x: Variable) => BlockAcc(x, t))
  def memberPropertyAccFactory : P[Variable => Variable] = P("->" ~ memberName ~ ("(" ~ argumentExpressionList ~ ")").?).map(t =>
    if(t._2.isDefined) (x) => MemberCallPropertyAcc(x, t._1, t._2.get)
    else (x) => MemberPropertyAcc(x, t._1)
  )

  def directCallAccFactory : P[Variable => Variable] = P("(" ~ argumentExpressionList ~ ")").map(t => (x: Variable) => CallAccessor(x, t))

  def qualifiedNameVarWithCall : P[Variable] = P(qualifiedNameVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)
  def expressionVarWithCall : P[Variable] = P(expressionVar ~ directCallAccFactory.?).map(t => if(t._2.isDefined) t._2.get(t._1) else t._1)

  def singleVariable : P[Variable] = P(simpleVariable | stringLiteralVar | scopeAccVar | qualifiedNameVarWithCall | expressionVarWithCall | arrayCreationVar)
  def variable : P[Variable] = P(singleVariable ~ (memberCallStaticAccFactory | arrayAccFactory | blockAccFactory | memberPropertyAccFactory).rep).map(t => t._2.foldLeft(t._1)((a,b) => b(a))).log()

}
