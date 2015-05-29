name := "renesca-magic"

version := "0.1.5"

val scalaV = "2.11.6"

val paradiseVersion = "2.1.0-M5"

scalaVersion := scalaV

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaV

addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)

scalacOptions ++= scalacOpts


// publishing
pgpSecretRing := file("local.secring.gpg")

pgpPublicRing := file("local.pubring.gpg")

organization := "com.github.renesca"

pomExtra := {
  <url>https://github.com/renesca/renesca-magic</url>
    <licenses>
      <license>
        <name>Apache 2</name>
        <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      </license>
    </licenses>
    <scm>
      <url>https://github.com/renesca/renesca-magic</url>
      <connection>scm:git:git@github.com:renesca/renesca-magic.git</connection>
    </scm>
    <developers>
      <developer>
        <id>fdietze</id>
        <name>Felix Dietze</name>
        <url>https://github.com/fdietze</url>
      </developer>
      <developer>
        <id>jkaroff</id>
        <name>Johannes Karoff</name>
        <url>https://github.com/cornerman</url>
      </developer>
    </developers>
}


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

