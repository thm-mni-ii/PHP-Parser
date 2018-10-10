package de.thm.mni.ii.phpparser

import org.scalatest.{FlatSpec, Matchers}

/**
  * Test PHPParsers ability to detect malformed php code.
  * @author Andrej Sajenko
  */
class FailureTest extends FlatSpec with Matchers {
  "PHPParser" must "fail parsing malformed php code" in {
    PHPParser.parse("<?php ecco 'Hallo, Welt!';") shouldBe a [PHPParser.Failure]
  }
}
