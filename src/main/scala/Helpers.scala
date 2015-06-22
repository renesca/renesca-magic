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
  def traitFactoryLocal(name: String) = "create" + name
  def nameToLabel(name: String) = name.toUpperCase
  def relationName(start: String, end: String) = s"${ start }To${ end }"


  def writeFile(filename: String, contents: String) = {
    val parent = new File(filename).getParentFile
    if(parent != null) parent.mkdirs()

    val out = new PrintWriter(filename)
    out.println(contents)
    out.close()
  }
}
