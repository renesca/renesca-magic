package helpers

trait Colors {
  def bold(s: String) = s"\u001b[1m${ s }\u001b[0m"
  def red(s: String) = s"\u001b[31m${ s }\u001b[0m"
  def green(s: String) = s"\u001b[32m${ s }\u001b[0m"
  def brown(s: String) = s"\u001b[33m${ s }\u001b[0m"
  def blue(s: String) = s"\u001b[34m${ s }\u001b[0m"
  def purple(s: String) = s"\u001b[35m${ s }\u001b[0m"
  def cyan(s: String) = s"\u001b[36m${ s }\u001b[0m"

  def highlight(code: String): String = {
    val id = "[a-zA-Z_$][a-zA-Z\\d_$]*"
    code.
      replaceAll(s"(\\s|^)(object|class|case class|trait)(\\s+$id)([^A-Za-z_\\d$$])", "$1" + bold(purple("$2")) + bold(green("$3")) + "$4").
      replaceAll(s"(\\s|^)(override def|def|override val|val|override var|var)(\\s+$id)([^A-Za-z_\\d$$])", "$1" + blue("$2") + cyan("$3") + "$4").
      replaceAll(s"(\\s|^)(@$id\\s)", "$1" + brown("$2"))
  }
}
