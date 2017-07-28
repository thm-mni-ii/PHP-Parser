package phpparser.parser.literals

import fastparse.all._
import fastparse.all.{IgnoreCase => I}

/**
  * Created by tobias on 26.05.17.
  */
object Keywords {

  val AllKeywords = Seq(
    "abstract", "and", "array", "as", "break", "callable", "case", "catch",
    "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else",
    "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile",
    "eval", "exit", "extends", "final", "finally", "for", "foreach", "function", "global", "goto",
    "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset",
    "list", "namespace", "new", "or", "print", "private", "protected", "public", "require",
    "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use",
    "var", "while", "xor", "yield")

  val ABSTRACT = P(I("abstract"))
  val AND = P(I("and"))
  val ARRAY = P(I("array"))
  val AS = P(I("as"))
  val BINARY = P(I("binary"))
  val BOOL = P(I("bool"))
  val BOOLEAN = P(I("boolean"))
  val BREAK = P(I("break"))
  val CALLABLE = P(I("callable"))
  val CASE = P(I("case"))
  val CATCH = P(I("catch"))
  val CLASS = P(I("class"))
  val CLONE = P(I("clone"))
  val CONST = P(I("const"))
  val CONTINUE = P(I("continue"))
  val DECLARE = P(I("declare"))
  val DEFAULT = P(I("default"))
  val DIE = P(I("die"))
  val DO = P(I("do"))
  val DOUBLE = P(I("double"))
  val ECHO = P(I("echo"))
  val ELSE = P(I("else"))
  val ELSEIF = P(I("elseif"))
  val EMPTY = P(I("empty"))
  val ENCODING = P(I("encoding"))
  val ENDDECLARE = P(I("enddeclare"))
  val ENDFOR = P(I("endfor"))
  val ENDFOREACH = P(I("endforeach"))
  val ENDIF = P(I("endif"))
  val ENDSWITCH = P(I("endswitch"))
  val ENDWHILE = P(I("endwhile"))
  val EVAL = P(I("eval"))
  val EXIT = P(I("exit"))
  val EXTENDS = P(I("extends"))
  val FINAL = P(I("final"))
  val FINALLY = P(I("finally"))
  val FLOAT = P(I("float"))
  val FOR = P(I("for"))
  val FOREACH = P(I("foreach"))
  val FROM = P(I("from"))
  val FUNCTION = P(I("function"))
  val GLOBAL = P(I("global"))
  val GOTO = P(I("goto"))
  val IF = P(I("if"))
  val IMPLEMENTS = P(I("implements"))
  val INCLUDE = P(I("include"))
  val INCLUDE_ONCE = P(I("include_once"))
  val INSTANCEOF = P(I("instanceof"))
  val INSTEADOF = P(I("insteadof"))
  val INT = P(I("int"))
  val INTEGER = P(I("integer"))
  val INTERFACE = P(I("interface"))
  val ISSET = P(I("isset"))
  val ITERABLE = P(I("iterable"))
  val LIST = P(I("list"))
  val NAMESPACE = P(I("namespace"))
  val NEW = P(I("new"))
  val OBJECT = P(I("object"))
  val OR = P(I("or"))
  val PARENT = P(I("parent"))
  val PHP = P(I("php"))
  val PRINT = P(I("print"))
  val PRIVATE = P(I("private"))
  val PROTECTED = P(I("protected"))
  val PUBLIC = P(I("public"))
  val REAL = P(I("real"))
  val RETURN = P(I("return"))
  val REQUIRE = P(I("require"))
  val REQUIRE_ONCE = P(I("require_once"))
  val SELF = P(I("self"))
  val STATIC = P(I("static"))
  val STRICT_TYPES = P(I("strict_types"))
  val STRING = P(I("string"))
  val SWITCH = P(I("switch"))
  val THROW = P(I("throw"))
  val TICKS = P(I("ticks"))
  val TRAIT = P(I("trait"))
  val TRY = P(I("try"))
  val UNSET = P(I("unset"))
  val USE = P(I("use"))
  val VAR = P(I("var"))
  val VOID = P(I("void"))
  val WHILE = P(I("while"))
  val XOR = P(I("xor"))
  val YIELD = P(I("yield"))

}
