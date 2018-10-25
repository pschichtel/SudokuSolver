enablePlugins(ScalaNativePlugin)

name := "SudokuSolver"

version := "0.1"

scalaVersion := "2.11.12"

nativeMode := "release"

libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
