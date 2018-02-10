package de.thm.mni.ii.phpparser.parser.literals

object Lexical {
  import fastparse.all._

  val WsChars = P(CharIn("\u0020\u0009"))
  val Newline = P(StringIn("\r\n", "\n", "\r"))

  val LineCommentText = P(CharsWhile(c => c != '\n' && c != '\r' && c != '?') | !("?>" | Newline) ~ AnyChar)
  val LineComment = P(("//" | "#") ~ LineCommentText.rep)

  val MultilineText = P(CharsWhile(c => c != '*') | (!"*/" ~ AnyChar))
  val MultiLineComment = P("/*" ~ MultilineText.rep ~ ("*/" | End))

  val Comment = P(LineComment | MultiLineComment)

  val Whitespace = P(NoTrace((WsChars | Newline | Comment).rep))

  val Ws = P(&(NoTrace(WsChars | Newline | Comment).rep(1)))
}

object WsAPI extends fastparse.WhitespaceApi.Wrapper(Lexical.Whitespace)
