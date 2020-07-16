name := "get-protos"

version := "1.0"

scalaVersion := "2.13.2"

libraryDependencies ++= CompileDeps

val CompileDeps = Seq(
  "org.json4s" %% "json4s-native" % "3.6.9",
  "com.typesafe.play" %% "play-json" % "2.8.1"
)