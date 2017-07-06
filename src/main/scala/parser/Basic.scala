package parser

import fastparse.noApi._
import parser.literals.WsAPI._
import parser.literals.Lexical.Ws

import parser.literals.Keywords.{PHP, NAMESPACE}
import parser.literals.Literals.Name

import ast.{Basic => BAst}

import parser.statements.StatementParser.AvailableStatement

object Basic {

  val NormalStartTag = P("<?".! ~~ PHP)
  val EchoStartTag = P("<?=".!)
  val EndTag = P("?>")
  val StartTag = P(NormalStartTag | EchoStartTag)

  val WsOrSemicolon = P(Ws | ";" | "?>")
  val WsExp = P(Ws | "(")
  val Semicolon = P(";" | "?>")

  val Script : P[BAst.Script] = P(Text ~~ NormalStartTag.? ~ AvailableStatement.rep ~ End)
    .map(t => BAst.Script(t._1, t._3))

  val Text : P[BAst.Text] = P((!StartTag ~~ AnyChar.!).repX).map(t => BAst.Text(t.mkString))

  val SemicolonFactory : P[Option[BAst.Text]] =
    P((";".!.map(_ => None) ~ !EchoStartTag) | ("?>" ~~ Text ~~ ((NormalStartTag ~~ !EchoStartTag) | EchoStartTag | End)).map(t => Some(t._1)))

  val QualifiedName : P[BAst.QualifiedName] = P((NAMESPACE ~ "\\" ~ (Name ~ "\\").rep ~ Name).map {
    case (path, name) => BAst.QualifiedName(BAst.NamespaceType.LOCAL, path, name)
  } | ("\\".!.? ~ (Name ~ "\\").rep ~ Name).map {
    case (Some(_), path, name) => BAst.QualifiedName(BAst.NamespaceType.GLOBAL, path, name)
    case (None, path, name) =>  BAst.QualifiedName(BAst.NamespaceType.RELATIVE, path, name)
  })

  val NamespaceName: P[Seq[BAst.Name]] = P(Name.rep(min = 0, sep = "\\"))
}
