package renesca.schema.macros

import java.io.{File, PrintWriter}

object Helpers {
  def rev(s: String) = "rev_" + s

  def nameToPlural(name: String) = {
    val lower = name.take(1).toLowerCase + name.drop(1)
    val suffix = if(lower.endsWith("s")) "" else "s"
    lower + suffix
  }

  def traitFactoryName(name: String) = name + "Factory"
  def traitMatchesFactoryName(name: String) = name + "MatchesFactory"
  def traitMatchesClassName(name: String) = name + "Matches"
  def factoryCreateMethod(name: String) = "create" + name
  def factoryMergeMethod(name: String) = "merge" + name
  def factoryMatchesMethod(name: String) = "matches" + name
  def factoryUniqueMatchesMethod(name: String) = "matchesOn" + name.capitalize
  def nameToLabel(name: String) = name.toUpperCase
  def hyperStartRelationName(name:String) = s"${ name }Start"
  def hyperEndRelationName(name:String) = s"${ name }End"

  def writeFile(filename: String, contents: String) = {
    val parent = new File(filename).getParentFile
    if(parent != null) parent.mkdirs()

    val out = new PrintWriter(filename)
    out.println(contents)
    out.close()
  }
}
