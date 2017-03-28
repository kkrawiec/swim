lazy val root = (project in file(".")).
  settings(
    name := "SWIM",
    version := "1.0",
    scalaVersion := "2.11.8",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
  ).dependsOn(fuel)

lazy val fuel = RootProject( file("../fuel") )
