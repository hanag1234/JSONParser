name := "JSONParser"
version := "0.1"
scalaVersion := "2.12.20"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.meerkat" %% "meerkat" % "0.1.0",
  "org.bitbucket.inkytonik.dsinfo" %% "dsinfo" % "0.4.0"
)