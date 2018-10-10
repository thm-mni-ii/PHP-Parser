package de.thm.mni.ii.phpparser

import org.scalatest.{FlatSpec, Matchers}
import de.thm.mni.ii.phpparser.ast.Basic._
import de.thm.mni.ii.phpparser.ast.Expressions._
import de.thm.mni.ii.phpparser.ast.Statements
import de.thm.mni.ii.phpparser.ast.Statements._

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

  it must "parse empty statement" in {
    PHPParser.parse("<?php ;") should matchPattern { case PHPParser.Success(
    Script(Text(""), ArrayBuffer(EmptyStmnt()))
    ) =>
    }
  }

  it must "parse end tag" in {
    PHPParser.parse("<?php ?>") should matchPattern { case PHPParser.Success(
    Script(Text(""), ArrayBuffer(Statements.EndTagStmnt(stmnt, text)))
    ) =>
    }
  }

  it must "parse do while loop" in {
    PHPParser.parse("<?php do { } while ($i > 0);") should matchPattern { case PHPParser.Success(
    Script(Text(""),ArrayBuffer(DoStmnt(RelationalExp(">",
    SimpleNameVar(Name(i)),OctalLiteral("")),CompoundStmnt(ArrayBuffer()))))
    ) =>
    }
  }

  it must "parse continue in loop" in {
    PHPParser.parse(
      """
        |<?php
        |while (true) {
        |   continue;
        |}
      """.stripMargin) should matchPattern { case PHPParser.Success(
    Script(Text("\n"),ArrayBuffer(WhileStmnt(QualifiedNameVar(
    QualifiedName(NamespaceType.RELATIVE,ArrayBuffer(),Name("true"))),
    List(CompoundStmnt(ArrayBuffer(ContinueStmnt(None)))))))) =>
    }
  }

  it must "pase include / require statements" in {
    PHPParser.parse(
      """
        |<?php
        |include 'a.php';
        |include_once "b.php";
        |require "c.php";
        |require_once ('d.php');
      """.stripMargin) should matchPattern { case PHPParser.Success(
      Script(Text("\n"),ArrayBuffer(
      ExpressionStmnt(IncludeExp(SQStringLiteral(None,"a.php"))),
      ExpressionStmnt(IncludeOnceExp(DQStringLiteral(None,ArrayBuffer(DQStringElement("b.php"))))),
      ExpressionStmnt(RequireExp(DQStringLiteral(None,ArrayBuffer(DQStringElement("c.php"))))),
      ExpressionStmnt(RequireOnceExp(EnclosedExp(SQStringLiteral(None,"d.php"))))))) =>
    }
  }

}
