package de.thm.ii.phpparser

import org.scalatest.{FlatSpec, Matchers}
import de.thm.ii.phpparser.ast.Basic._
import de.thm.ii.phpparser.ast.Expressions._
import de.thm.ii.phpparser.ast.Statements._

import scala.collection.mutable.ArrayBuffer

/**
  * Test PHPParsers ability to parse php classes and interfaces.
  * @author Andrej Sajenko
  */
class ClassTest extends FlatSpec with Matchers {

  "PHPParser" must "parse empty class" in {
    PHPParser.parse("<?php class Foo { }") shouldEqual
      PHPParser.Success(Script(Text(""),
        ArrayBuffer(ClassDecl(None, Some(Name("Foo")), None, None, ArrayBuffer()))))
  }

  it must "parse class with extension" in {
    PHPParser.parse("<?php class Foo extends Bar { }") shouldEqual
      PHPParser.Success(Script(Text(""),
        ArrayBuffer(ClassDecl(None, Some(Name("Foo")),
          Some(QualifiedName(NamespaceType.RELATIVE, ArrayBuffer(), Name("Bar"))),
          None, ArrayBuffer()))))
  }

  it must "parse class with interface" in {
    PHPParser.parse("<?php class Foo implements Bar { }") shouldEqual
      PHPParser.Success(Script(Text(""),
        ArrayBuffer(ClassDecl(None, Some(Name("Foo")), None,
          Some(ArrayBuffer(QualifiedName(NamespaceType.RELATIVE,
            ArrayBuffer(), Name("Bar")))),
          ArrayBuffer()))))
  }

  it must "parse class with attributes" in {
    PHPParser.parse(
      """
        |<?php
        |class Foo {
        | public $attr1 = 23;
        | private static $attr2 = 'Str';
        | protected $attr3 = 3;
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_, ArrayBuffer(
        ClassDecl(None, Some(Name("Foo")), None, None, ArrayBuffer(
          PropertyDecl(CombinedMod(None, Some(PublicMod)), ArrayBuffer(
            PropertyElement(SimpleNameVar(Name("attr1")), Some(DecimalLiteral("23"))))),
          PropertyDecl(CombinedMod(Some(StaticMod), Some(PrivateMod)), ArrayBuffer(
            PropertyElement(SimpleNameVar(Name("attr2")), Some(SQStringLiteral(None,"Str"))))),
          PropertyDecl(CombinedMod(None, Some(ProtectedMod)), ArrayBuffer(
            PropertyElement(SimpleNameVar(Name("attr3")),Some(DecimalLiteral("3"))))))
        )
      ))) => }
  }

  it must "parse class with methods" in {
    PHPParser.parse(
      """
        |<?php
        |class Foo {
        | public function met1() {}
        | private static function met2() {}
        | protected function met3() {}
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_, ArrayBuffer(
        ClassDecl(None, Some(Name("Foo")), None, None, ArrayBuffer(
          MethodDecl(ArrayBuffer(PublicMod),
            FuncHeader(false, Some(Name("met1")), ArrayBuffer(),None),
            Some(CompoundStmnt(ArrayBuffer()))),
          MethodDecl(ArrayBuffer(PrivateMod, StaticMod),
            FuncHeader(false,Some(Name(met2)),ArrayBuffer(),None),
            Some(CompoundStmnt(ArrayBuffer()))),
          MethodDecl(ArrayBuffer(ProtectedMod),
            FuncHeader(false,Some(Name(met3)),ArrayBuffer(),None),
            Some(CompoundStmnt(ArrayBuffer()))))
        ))
      )) => }
  }

  it must "parse class methods with parameters" in {
    PHPParser.parse(
      """
        |<?php
        |class Foo {
        | public function met1($var1, $var2) {}
        | public function met2(&$var1) {}
        | public function met4($var1 = "default") {}
        | public function met5() { $a = 2; }
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        ClassDecl(None, Some(Name("Foo")), None, None, ArrayBuffer(
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met1")),ArrayBuffer(SimpleParam(None,false,SimpleNameVar(Name("var1")),None), SimpleParam(None,false,SimpleNameVar(Name("var2")),None)),None),Some(CompoundStmnt(ArrayBuffer()))),
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met2")),ArrayBuffer(SimpleParam(None,true,SimpleNameVar(Name("var1")),None)),None),Some(CompoundStmnt(ArrayBuffer()))),
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met4")),ArrayBuffer(SimpleParam(None,false,SimpleNameVar(Name("var1")),Some(DQStringLiteral(None,ArrayBuffer(DQStringElement("default")))))),None),Some(CompoundStmnt(ArrayBuffer()))),
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met5")),ArrayBuffer(),None),Some(CompoundStmnt(ArrayBuffer(ExpressionStmnt(SimpleAssignmentExp(false,SimpleNameVar(Name("a")),DecimalLiteral("2"))))))),
        )))))
    => }
  }

  it must "parse class methods with types" in {
    PHPParser.parse(
      """
        |<?php
        |class Foo {
        | public function met3(Bar $var1) {}
        | public function met6(): Bar { return $a; }
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
      ClassDecl(None, Some(Name("Foo")), None, None, ArrayBuffer(
        MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met3")),ArrayBuffer(SimpleParam(Some(QualifiedType(QualifiedName(NamespaceType.RELATIVE,ArrayBuffer(),Name("Bar")))),false,SimpleNameVar(Name("var1")),None)),None),Some(CompoundStmnt(ArrayBuffer()))),
        MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("met6")),ArrayBuffer(),Some(QualifiedType(QualifiedName(NamespaceType.RELATIVE,ArrayBuffer(),Name("Bar"))))),Some(CompoundStmnt(ArrayBuffer(ReturnStmnt(Some(SimpleNameVar(Name("a"))))))))
      )))))
    => }
  }

  it must "parse interface with methods" in {
    PHPParser.parse(
      """
        |<?php
        |interface Foo {
        |    public function fun1($name, $var);
        |    public function fun2($template);
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        InterfaceDecl(Name("Foo"),None,ArrayBuffer(
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("fun1")),ArrayBuffer(
            SimpleParam(None,false,SimpleNameVar(Name("name")),None),
            SimpleParam(None,false,SimpleNameVar(Name("var")),None)),None),None),
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("fun2")),ArrayBuffer(
            SimpleParam(None,false,SimpleNameVar(Name("template")),None)),None),None))))))
    => }
  }

  it must "parse abstract class with methods" in {
    PHPParser.parse(
      """
        |<?php
        |abstract class Foo {
        |    abstract protected function bar1();
        |    abstract protected function bar2($prefix);
        |
        |    public function funP() {
        |        print $this->bar1();
        |    }
        |}
      """.stripMargin) should matchPattern { case
      PHPParser.Success(Script(_,ArrayBuffer(
        ClassDecl(Some(AbstractMod),Some(Name("Foo")),None,None,ArrayBuffer(
          MethodDecl(ArrayBuffer(AbstractMod, ProtectedMod),FuncHeader(false,Some(Name("bar1")),ArrayBuffer(),None),None),
          MethodDecl(ArrayBuffer(AbstractMod, ProtectedMod),FuncHeader(false,Some(Name("bar2")),ArrayBuffer(
            SimpleParam(None,false,SimpleNameVar(Name(prefix)),None)),None),None),
          MethodDecl(ArrayBuffer(PublicMod),FuncHeader(false,Some(Name("funP")),ArrayBuffer(),None),Some(
            CompoundStmnt(ArrayBuffer(ExpressionStmnt(PrintIntrinsic(MemberCallPropertyAcc(SimpleNameVar(Name("this")),
              NameMember(Name("bar1")),ArrayBuffer())))))))))
      )))
    => }
  }


}
