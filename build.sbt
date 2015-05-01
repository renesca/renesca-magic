name := "renesca-magic"

val scalaV = "2.11.6"

val paradiseVersion = "2.1.0-M5"

lazy val root = (project in file(".")).settings(
  scalaVersion := scalaV,
  scalacOptions ++= scalacOpts
).
  //  dependsOn(macros).
  aggregate(macros)


lazy val macros = (project in file("macros")).
  settings(
    scalaVersion := scalaV,
    libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaV,
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full),
    scalacOptions ++= scalacOpts
  )

val scalacOpts = Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-Yinline", "-Yinline-warnings",
  "-language:_"
  //,"-Xdisable-assertions", "-optimize"
)

val scalacMacroOpts = Seq(
  "-Ymacro-debug-lite",
  "-Yshow-trees-stringified"
)

