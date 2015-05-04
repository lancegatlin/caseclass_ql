scalaVersion := "2.11.6"

organization := "org.lancegatlin"

name := "caseclass_ql"

version := "1.0.0"

scalacOptions ++= Seq("-feature","-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
)