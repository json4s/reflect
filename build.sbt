scalaVersion := "2.10.0"

name := "reflect"

organization := "com.github.casualjim"

libraryDependencies += "org.specs2" %% "specs2" % "1.13" % "test"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:implicitConversions")