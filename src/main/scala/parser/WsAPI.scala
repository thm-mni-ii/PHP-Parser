package parser

/**
  * Created by tobias on 02.05.17.
  */
object WsAPI extends fastparse.WhitespaceApi.Wrapper(Lexical.ws)

object Lexical {
  import fastparse.all._

  val ws = P((wsChars|newline).rep)

  val wsChars = P(CharIn("\u0020\u0009"))
  val newline = P(StringIn("\r\n", "\n", "\r"))

}
