enablePlugins(ScalaJSPlugin)

name := "GraphHW3"

version := "1.0"

scalaVersion := "2.11.8"

val sprayVersion = "1.3.3"

val akkaVersion = "2.3.9"

libraryDependencies ++= Seq(
  "com.typesafe" % "config" % "1.3.0",
  "com.typesafe.slick" %% "slick" % "3.1.1",
  "org.slf4j" % "slf4j-nop" % "1.6.4",
  "mysql" % "mysql-connector-java" % "5.1.38",
  "net.ruippeixotog" %% "scala-scraper" % "1.0.0",
  "org.scala-js" %%% "scalajs-dom" % "0.9.0",
  "be.doeraene" %%% "scalajs-jquery" % "0.9.0",
  "org.singlespaced" %%% "scalajs-d3" % "0.3.3",
  "io.spray" %% "spray-can" % sprayVersion,
  "io.spray" %% "spray-routing" % sprayVersion,
  "io.spray" %% "spray-client" % sprayVersion,
  "io.spray" %% "spray-json" % "1.3.2",
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
  "org.eclipse.jetty" % "jetty-io" % "9.2.15.v20160210",
  "com.lihaoyi" %% "scalatags" % "0.5.4",
  "com.lihaoyi" %%% "upickle" % "0.3.9"
)
