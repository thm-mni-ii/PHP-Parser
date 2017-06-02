package parser.literals

object Lexical {
  import fastparse.all._

  val wsChars = P(CharIn("\u0020\u0009"))
  val newline = P(StringIn("\r\n", "\n", "\r"))

  val lineCommentText = P(CharsWhile(c => c != '\n' && c != '\r' && c != '?') | !("?>" | newline) ~ AnyChar)
  val lineComment = P(("//" | "#") ~ lineCommentText.rep)

  val multilineText = P(CharsWhile(c => c != '*') | (!"*/" ~ AnyChar))
  val multiLineComment = P("/*" ~ multilineText.rep ~ ("*/" | End))

  val comment = P(lineComment | multiLineComment)

  val whitespace = P(NoTrace((wsChars | newline | comment).rep))

  val ws = P(&(NoTrace(wsChars | newline | comment).rep(1)))
}

object WsAPI extends fastparse.WhitespaceApi.Wrapper(Lexical.whitespace)
