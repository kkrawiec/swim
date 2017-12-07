// Uncomment this line to use manually created local clone of FUEL repository
// lazy val fuel = RootProject( file("../fuel") )
lazy val fuel = RootProject(uri("git://github.com/kkrawiec/fuel.git"))

lazy val root = (project in file(".")).
  settings(
    name := "SWIM",
    version := "1.0",
    scalaVersion := "2.11.11",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "junit" % "junit" % "4.12" % Test,
        "com.novocode" % "junit-interface" % "0.11" % Test)
  ).dependsOn(fuel)
