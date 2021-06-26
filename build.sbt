val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio-config-scala3-example",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-config" % "1.0.6"
    ),
    // To make the default compiler and REPL use Dotty
    scalaVersion := scala3Version
  )
