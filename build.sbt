name := "sudoku-solver"

version := "1.0"

scalaVersion := "2.12.1"

val scalaTestDependency = "org.scalatest" %% "scalatest" % "3.0.1"
val scalaMeter =  "com.storm-enroute" %% "scalameter" % "0.8.2" % "test"

libraryDependencies += scalaTestDependency
libraryDependencies += scalaMeter
