package de.thm.ii.phpparser

import org.scalatest.{FlatSpec, Matchers}
import de.thm.ii.phpparser.ast.Basic._
import de.thm.ii.phpparser.ast.Expressions._
import de.thm.ii.phpparser.ast.Statements.{CompoundStmnt, ExpressionStmnt, IfStmnt}

import scala.collection.mutable.ArrayBuffer

/**
  * Test PHPParsers ability to parse Statements.
  * @author Andrej Sajenko
  */
class StatementsTest extends FlatSpec with Matchers {

  "PHPParser" must "parse 'if' statement" in {
    PHPParser.parse("<?php if (a < 3) $a = 3;") should matchPattern { case PHPParser.Success(
    Script(Text(""), ArrayBuffer(
      IfStmnt(
        RelationalExp("<",
          QualifiedNameVar(
            QualifiedName(
              NamespaceType.RELATIVE,
              ArrayBuffer(),
              Name("a")
            )
          ),
          DecimalLiteral("3")
        ),
        _,
        ArrayBuffer(), None
      )
    ))) => }
  }

  it must "parse ifelse statement" in {
    PHPParser.parse("<?php if (a < 3) $a = 3; elseif (2 < 3) $b = 3;") should matchPattern {
      case PHPParser.Success(
      Script(Text(""),ArrayBuffer(
        IfStmnt(_, _,ArrayBuffer((
          RelationalExp("<",
            DecimalLiteral("2"),
            DecimalLiteral("3")
          ),
          List(ExpressionStmnt(
            SimpleAssignmentExp(
              false,
              SimpleNameVar(
                Name("b")
              ),
            DecimalLiteral("3")))))),None)))
      ) =>
    }
  }

  it must "parse if else statement" in {
    PHPParser.parse("<?php if (a < 3) $a = 3; else { $b = 3; }") should matchPattern {
      case PHPParser.Success(
        Script(Text(""),ArrayBuffer(
          IfStmnt(_, _, ArrayBuffer(), Some(List(
            CompoundStmnt(ArrayBuffer(
              ExpressionStmnt(
                SimpleAssignmentExp(
                  false,
                  SimpleNameVar(Name("b")), DecimalLiteral("3")
                )
              )
            ))
          ))))
        )
      ) =>
    }
  }

}
