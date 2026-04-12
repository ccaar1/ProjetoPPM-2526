val scala3Version = "3.3.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Konane",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    libraryDependencies += "org.scalafx" %% "scalafx" % "22.0.0-R33",
    fork := true,
    run / connectInput := true,
    Compile / mainClass := Some("konane.Main")
  )
