# PHP-Parser [![Build Status](https://travis-ci.org/thm-mni-ii/PHP-Parser.svg?branch=master)](https://travis-ci.org/thm-mni-ii/PHP-Parser) [![Coverage Status](https://coveralls.io/repos/github/thm-mni-ii/Scala-PHP-Parser/badge.svg?branch=master)](https://coveralls.io/github/thm-mni-ii/Scala-PHP-Parser?branch=master)
This project contains a PHP-Parser based on the current [php language specification](https://github.com/php/php-langspec). It supports PHP version 7.

The parser is written in Scala, but can be used in **Java** as well as in **Scala** projects.
Based on [FastParse](https://github.com/lihaoyi/fastparse) it transforms a valid PHP-Script into an abstract syntax tree.

## Getting Started

Import the artifacts from Maven Central.

```xml
<dependency>
  <groupId>de.thm.mni.ii</groupId>
  <artifactId>phpparser</artifactId>
  <version>1.0.0</version>
</dependency>
```

## Usage

These simple examples present the basic usage of the parser.

#### Java
```java
import de.thm.mni.ii.phpparser.PHPParser;
import de.thm.mni.ii.phpparser.ast.Basic;

public class Main {
    public static void main(String[] args) {
        PHPParser.Result res = (PHPParser.Result) PHPParser.parse("<?php $value = 5;");
        if (res instanceof PHPParser.Success) {
            Basic.Script s = ((PHPParser.Success) res).script();
            System.out.println(s);
        } else if (res instanceof PHPParser.Failure) {
            String msg = ((PHPParser.Failure) res).fullMsg();
            System.out.println(msg);
        }
    }
}
```

#### Scala
```scala
object Main extends App {
  import de.thm.mni.ii.phpparser.PHPParser

  PHPParser.parse("<?php $value = 5;") match {
    case s: PHPParser.Success => println(s.script)
    case f: PHPParser.Failure => println(f.fullMsg)
  }
}
```

The top-level __PHPParser__ parses a whole PHP-Script. The result is an instance of __PHPParser.Success__ or __PHPParser.Failure__. 

__PHPParser.Success__ contains the abstract syntax tree. __PHPParser.Failure__ contains additional information about the error. If you need further error information, take a look at the _failure_-member. FastParse provides additional methods to present the origin of the parse-error. 

## Project Structure

```
.
├── ...
├── src/main/scala/de/thm/mni/ii/phpparser
│   ├── PHPParser.scala    # top-level parser
│   ├── ast/               # case classes of abstract syntax tree
│   └── parser/            # specific parsers for all different elements
└── ...
```

## Contribute

Feel free to send Pull-Requests to improve this parser. If you find a bug, please report it as an issue on github. 

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

