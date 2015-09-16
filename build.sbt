name := "route-trie"

organization := "io.fcomb"

version := "0.3.1.2"

scalaVersion := "2.11.7"

scalacOptions += "-target:jvm-1.8"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.6.4" % "test"
)

bintrayOrganization := Some("fcomb")

licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.html"))

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/fcomb/route-trie</url>
  <licenses>
    <license>
      <name>MIT License</name>
      <url>http://www.opensource.org/licenses/mit-license.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:fcomb/route-trie.git</url>
    <connection>scm:git:git@github.com:fcomb/route-trie.git</connection>
  </scm>
  <developers>
    <developer>
      <id>fcomb</id>
      <name>fcomb</name>
      <url>https://github.com/fcomb/</url>
    </developer>
  </developers>)
