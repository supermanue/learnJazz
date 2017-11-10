name := "learnJazz"

organization := "learnjazz.org"

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-deprecation")


// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test

// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

mainClass := Some("Main")