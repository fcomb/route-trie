name := "route-trie"

organization := "io.fcomb"

version := "0.3.0"

scalaVersion := "2.11.7"

scalacOptions += "-target:jvm-1.8"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.6.4" % "test"
)

publishArtifact in Test := false

publishMavenStyle := false

bintrayOrganization := Some("fcomb")

licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/mit-license.html"))
