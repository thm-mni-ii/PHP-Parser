package parser

import parser.Basic.Script

object PHPParser {
  def parse(toParse: String) = Script.parse(toParse)
}
