package parser

import ast.Basic._
import fastparse.noApi._
import parser.PHPParser._
import parser.StatementParser.statement
import parser.literals.WsAPI._
import parser.literals.Keywords.{PHP, NAMESPACE}
import parser.literals.Literals.name

object Basic {

  def script : P[Script] = {
    isTagProcessed = true
    P(text ~ normalStartTag.? ~ statement.rep ~ End).map(t => {
      isTagProcessed = t._2.isDefined
      Script(t._1, t._3)
    })
  }

  val text : P[Text] = P((!startTag ~ AnyChar.!).rep).map(t => Text(t.mkString))

  def semicolonFactory : P[Option[Text]] = P(";".!.map(_ => None) |
    ("?>" ~ text ~ normalStartTag.?).map(t => {
      isTagProcessed = t._2.isDefined
      Some(t._1)
    })
  )

  val normalStartTag = P("<?".! ~~ PHP)
  val echoStartTag = P("<?=".!)
  val endTag = P("?>")
  val startTag = P(normalStartTag | echoStartTag)

  def qualifiedName : P[QualifiedName] = P((NAMESPACE ~ "\\" ~ (name ~ "\\").rep ~ name).map(t => QualifiedName(NamespaceType.LOCAL, t._1, t._2)) |
    ("\\".!.? ~ (name ~ "\\").rep ~ name).map(t =>
      if(t._1.isDefined) QualifiedName(NamespaceType.GLOBAL, t._2, t._3)
      else QualifiedName(NamespaceType.RELATIVE, t._2, t._3)
    ))

}
