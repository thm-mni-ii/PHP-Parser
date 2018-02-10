# PHP-Parser [![Build Status](https://travis-ci.org/thm-mni-ii/Scala-PHP-Parser.svg?branch=master)](https://travis-ci.org/thm-mni-ii/Scala-PHP-Parser)
This project contains an PHP-Parser based on the current [php language specification](https://github.com/php/php-langspec). It supports PHP version 7.

This project is written in Scala and can be used in **Java** and **Scala**. Based on [FastParse](https://github.com/lihaoyi/fastparse) it transforms an valid PHP-String into an abstract syntax tree.

## Getting Started

There are different ways to include this library to your own projects. The following parts explain some methods to include them in Java and Scala. Currently you have to clone the repository to your local system and build the jar-file. Maven-support is planned for the future. 

#### Java

At first you need to build a standalone jar-library with all dependencies including the core scala-library. The following sbt-command creates such a jar-file. 

```console
$ sbt assembly
```

#### Scala

The previous command works in Scala, too. Nevertheless a jar-file with all dependencies is not necessary, if sbt can load these separately. To build a jar-file without dependencies, you need to execute the following command.

```console
$ sbt package
```

Another possible way is to publish the project to your local Ivy repository.
```console
$ sbt publishLocal
```

After that you need to include this line to your 'build.sbt' file.

```scala
libraryDependencies += "de.thm.mni.ii" %% "PHPParser" % "1.0"
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

The top-level __PHPParser__ performs an parse on a whole script. The result is an instance of __PHPParser.Success__ or __PHPParser.Failure__. 

__PHPParser.Success__ only contains the parsed result. __PHPParser.Failure__ contains additional information about the error. If you need further error information, take a look at the _failure_-member. FastParse provides additional methods to present the origin of the parse-error. 

If you want to parse an specific element of the programing language, you can call _parse_ on any parser in the package parser. 

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

Feel free to send Pull-Requests to improve this parser. If you find any bug, please report it as issue here on github. 

This project is tested against several php-projects, but the current version does not contain unit tests. I would really appreciate it if someone can write some proper unit tests. 

## Authors

* **Tobias Viehmann** - *Initial work*


## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details

