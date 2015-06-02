package renesca.schema.macros

import java.io.{File, PrintWriter}

object Helpers {
  var asserted = 0
  def assertX(a: Any, b: Any) { if(a != b) { println(s"### Assertion failed: $a != $b"); asserted += 1 } }
  def crashOnAsserted() { if(asserted > 0) sys.error(s"$asserted assertions failed") }
  def rev(s: String) = "rev_" + s
  def nameToPlural(name: String) = {
    val lower = name.take(1).toLowerCase + name.drop(1)
    val suffix = if(lower.endsWith("s")) "" else "s"
    lower + suffix
  }
  def traitFactoryName(name: String) = name + "Factory"
  def nameToLabel(name: String) = name.toUpperCase
  def relationName(start: String, end: String) = s"${ start }To${ end }"


  def writeFile(filename: String, contents: String) = {
    val parent = new File(filename).getParentFile()
    if(parent != null) parent.mkdirs()

    val out = new PrintWriter(filename)
    out.println(contents)
    out.close()
  }
}
