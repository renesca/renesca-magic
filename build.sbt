name := "renesca-magic"

version := "0.0.1"

val scalaV = "2.11.6"

val paradiseVersion = "2.1.0-M5"


scalaVersion := scalaV

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % scalaV,
  "com.github.renesca" %% "renesca" % "0.1.3"
)

scalacOptions ++= scalacOpts

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
