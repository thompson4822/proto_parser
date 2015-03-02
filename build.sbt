import sbtprotobuf.{ProtobufPlugin=>PB}

Seq(PB.protobufSettings: _*)

name := "parser"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.11",
  "org.json4s" %% "json4s-ext" % "3.2.11",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)
