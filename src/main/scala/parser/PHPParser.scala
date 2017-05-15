package parser

object PHPParser {
  private[parser] var isTagProcessed = true

  def parse(toParse: String) = Basic.script.parse(toParse)
}
