package parser

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import parser.literals.Keywords.{PHP, NAMESPACE}
import parser.literals.Literals.Name

import ast.{Basic => BAst}

import parser.PHPParser._
import parser.statements.StatementParser.Statement

object Basic {

  def Script : P[BAst.Script] = {
    isTagProcessed = true
    P(Text ~~ NormalStartTag.? ~ Statement.rep ~ End).map(t => {
      isTagProcessed = t._2.isDefined
      BAst.Script(t._1, t._3)
    })
  }

  val Text : P[BAst.Text] = P((!StartTag ~~ AnyChar.!).repX).map(t => BAst.Text(t.mkString))

  def SemicolonFactory : P[Option[BAst.Text]] =
    P(";".!.map(_ => None) | ("?>" ~~ Text ~~ NormalStartTag.?).map(t => {
      isTagProcessed = t._2.isDefined
      Some(t._1)
    })
  )

  val NormalStartTag = P("<?".! ~~ PHP)
  val EchoStartTag = P("<?=".!)
  val EndTag = P("?>")
  val StartTag = P(NormalStartTag | EchoStartTag)

  val WsOrSemicolon = P(Ws | ";" | "?>")
  val WsExp = P(Ws | "(")
  val Semicolon = P(";" | "?>")

  def QualifiedName : P[BAst.QualifiedName] = P((NAMESPACE ~ "\\" ~ (Name ~ "\\").rep ~ Name).map(t => BAst.QualifiedName(BAst.NamespaceType.LOCAL, t._1, t._2)) |
    ("\\".!.? ~ (Name ~ "\\").rep ~ Name).map(t =>
      if(t._1.isDefined) BAst.QualifiedName(BAst.NamespaceType.GLOBAL, t._2, t._3)
      else BAst.QualifiedName(BAst.NamespaceType.RELATIVE, t._2, t._3)
    ))

  val NamespaceName: P[Seq[BAst.Name]] = P(Name.rep(min = 0, sep = "\\"))
}
