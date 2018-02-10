package de.thm.mni.ii.phpparser

import org.scalatest.{FlatSpec, Matchers}
import de.thm.mni.ii.phpparser.ast.Basic._
import de.thm.mni.ii.phpparser.ast.Expressions._
import de.thm.mni.ii.phpparser.ast.Statements.{CompoundStmnt, ExpressionStmnt, FuncHeader, ReturnStmnt}

import scala.collection.mutable.ArrayBuffer

/**
  *Test PHPParsers ability to parse php expressions.
  * @author Andrej Sajenko
  */
class ExpressionTest extends FlatSpec with Matchers {
  "PHPParser" must "parse 'echo' expression" in {
    PHPParser.parse("<?php echo 'Hallo, Welt!';") shouldEqual PHPParser.Success(Script(
      Text(""),
      ArrayBuffer(
        ExpressionStmnt(
          EchoIntrinsic(
            ArrayBuffer(
              SQStringLiteral(None, "Hallo, Welt!")
            )
          )
        )
      )
    ))
  }

  it must "parse a simple formular [* / + -]" in {
    PHPParser.parse(
      """
        |<?php
        |$var1 = 23 * (3 + $var / 3 - $b);
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        ExpressionStmnt(SimpleAssignmentExp(false,SimpleNameVar(Name("var1")),
          MulExp(
            DecimalLiteral("23"),
            EnclosedExp(
              SubExp(
                AddExp(
                  DecimalLiteral("3"),
                  DivExp(
                    SimpleNameVar(Name("var")),
                    DecimalLiteral("3")
                  )
                ),
                SimpleNameVar(Name("b"))
              )
            )
          )
      )))))
    => }
  }

  it must "parse a simple formular [&& || == > < >= <=]" in {
    PHPParser.parse(
      """
        |<?php
        |$var1 = 5 > 3 && 2 < 3 || 1 >= 2 && 1 <= 2 || 9 == 3;
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        ExpressionStmnt(SimpleAssignmentExp(false,SimpleNameVar(Name("var1")),
          LogicalOrExp(
            LogicalOrExp(
              LogicalAndExp(
                RelationalExp(">",
                  DecimalLiteral("5"),
                  DecimalLiteral("3")),
                RelationalExp("<",
                  DecimalLiteral("2"),
                  DecimalLiteral("3")
                )
              ),
              LogicalAndExp(
                RelationalExp(">=",
                  DecimalLiteral("1"),
                  DecimalLiteral("2")
                ),
                RelationalExp("<=",
                  DecimalLiteral("1"),
                  DecimalLiteral("2"))
                )
              ),
            EqualityExp("==",
              DecimalLiteral("9"),
              DecimalLiteral("3")
            )
          )
        ))
      )))
    => }
  }

  it must "parse function expression" in {
    PHPParser.parse(
      """
        |<?php
        |$var = function () use ($var1) { return $var1; };
      """.stripMargin
    ) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        ExpressionStmnt(SimpleAssignmentExp(false,SimpleNameVar(Name("var")),
          AnonymousFunctionCreationExp(false,
            FuncHeader(false,None,ArrayBuffer(),None),ArrayBuffer((false,SimpleNameVar(Name("var1")))),
            CompoundStmnt(ArrayBuffer(ReturnStmnt(Some(SimpleNameVar(Name("var1"))))))
          )
        ))
      )))
    => }
  }
}
