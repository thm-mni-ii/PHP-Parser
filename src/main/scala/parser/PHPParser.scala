package parser

import parser.Basic.script

object PHPParser {
  private[parser] var isTagProcessed = true

  def parse(toParse: String) = script.parse(toParse)
}
