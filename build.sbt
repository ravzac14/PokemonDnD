name := "PokemonDnD"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "1.0.0"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11+"

mainClass in assembly := Some("app.Main")

