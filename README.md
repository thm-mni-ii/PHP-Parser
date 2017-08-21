# Scala-PHP-Parser [![Build Status](https://travis-ci.org/icampus/Scala-PHP-Parser.svg?branch=master)](https://travis-ci.org/icampus/Scala-PHP-Parser)
This project contains an PHP-Parser based on the current [php language specification](https://github.com/php/php-langspec).

This project is written in Scala and can be used in Java and Scala. Based on [FastParse](https://github.com/lihaoyi/fastparse) it transforms an valid PHP-String into an abstract syntax tree.

## Getting Started

There are different ways to include this library into your own project.

Currently you have to clone the repository to your local system. The following sbt-command builds an jar-File, which you need to include in your project.

```console
$ sbt build
```

Another possible way is to publish the project to your local Ivy repository.
```console
$ sbt publishLocal
```

After that you need to include this line to your 'build.sbt' file.

```scala
libraryDependencies += "de.thm.mni" %% "ScalaPHPParser" % "1.0"
```

## Usage

This simple example presents the basic usage of the parser in Scala.

```scala
object Main extends App {
  import phpparser.PHPParser

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
├── src/main/scala/phpparser
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

