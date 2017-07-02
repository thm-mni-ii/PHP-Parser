package parser

import parser.Basic.Script

object PHPParser {
  private[parser] var isTagProcessed = true

  def parse(toParse: String) = Script.parse(toParse)
}
