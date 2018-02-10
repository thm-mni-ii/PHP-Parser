package de.thm.mni.ii.phpparser

import fastparse.all._
import fastparse.core.Frame
import de.thm.mni.ii.phpparser.ast.Basic.Script

object PHPParser {

  def parse(toParse: String) = {
    val result = de.thm.mni.ii.phpparser.parser.Basic.Script.parse(toParse)
    result match {
      case Parsed.Success(value, currentIndex) => {
        PHPParser.Success(value)
      }
      case Parsed.Failure(parser, index, extra) => {
        val msg =
          s"""
             |Parsing failed on ${extra.input.repr.prettyIndex(extra.input, index)}
             |  expected: ${extra.traced.expected}
             |  got: "...${extra.input.slice(index, index + 20)}"
          """.stripMargin
        val framePrint = extra.traced.fullStack.reverse.map { case Frame(index, p) => s"  while trying to parse '${p}' at ${extra.input.repr.prettyIndex(extra.input, index)}\n" }.mkString
        val fullMsg =
          s"""
             |${msg}
             |
             |Parsing failed on "...${extra.input.slice(index, index + 20)}"
             |  while trying to parse '${extra.traced.expected}' at ${extra.input.repr.prettyIndex(extra.input, index)}
             |${framePrint}
          """
            .stripMargin
        PHPParser.Failure(msg, fullMsg, result.asInstanceOf[Parsed.Failure])
      }
    }
  }

  sealed abstract class Result {}
  case class Success(script: Script) extends PHPParser.Result
  case class Failure(msg: String, fullMsg: String, failure: Parsed.Failure) extends PHPParser.Result
}
