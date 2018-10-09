import org.scoverage.coveralls.Imports.CoverallsKeys._

name := "PHPParser"

scalaVersion := "2.12.2"

organization := "de.thm.mni.ii"
version := "1.0.0"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USERNAME"))
  password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

coverallsToken := Option(System.getenv().get("COVERALLS_TOKEN"))

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true
pomIncludeRepository := { _ => false }
publishArtifact in Test := false
crossPaths := false

useGpg := false
usePgpKeyHex("5973708299E8EEEB4319D40156A5D5DE07E4EAA9")
pgpPublicRing := baseDirectory.value / "project" / "gnupg" / "signkey.asc"
pgpSecretRing := baseDirectory.value / "project" / "gnupg" / "signkey.asc"
pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray)

licenses := Seq("MIT-style" -> url("http://www.opensource.org/licenses/mit-license.php"))
homepage := Some(url("https://github.com/thm-mni-ii/PHP-Parser"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/thm-mni-ii/PHP-Parser"),
    "scm:git@github.com:thm-mni-ii/PHP-Parser.git"
  )
)
developers := List(
  Developer(
    id    = "andrej-sajenko",
    name  = "Andrej Sajenko",
    email = "Andrej.Sajenko@mni.thm.de",
    url   = url("https://github.com/andrej-sajenko")
  )
)
publishMavenStyle := true