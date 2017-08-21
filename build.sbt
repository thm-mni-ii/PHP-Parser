name := "ScalaPHPParser"

version := "1.0"

scalaVersion := "2.12.2"

organization := "de.thm.mni"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)