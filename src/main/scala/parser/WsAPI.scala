package parser

object WsAPI extends fastparse.WhitespaceApi.Wrapper(Lexical.ws)

object Lexical {
  import fastparse.all._

  val wsChars = P(CharIn("\u0020\u0009"))
  val newline = P(StringIn("\r\n", "\n", "\r"))

  val lineComment = P(("//" | "#") ~ CharsWhile(c => c != '\n' && c != '\r') ~ (newline | End))

  val multilineText = P(CharsWhile(c => c != '*') | (!"*/" ~ AnyChar))
  val multiLineComment = P("/*" ~ multilineText.rep ~ ("*/" | End))

  val comment = P(lineComment | multiLineComment)

  val ws = P(NoTrace((wsChars | newline | comment).rep))
}
