package helpers

trait CompileSpec extends Colors {

  import scala.tools.cmd.CommandLineParser
  import scala.tools.nsc.reporters.StoreReporter
  import scala.tools.nsc.{CompilerCommand, Global, Settings}

  object Config {
    val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.7/jars/paradise_2.11.7-2.1.0.jar"
    val classpath = System.getProperty("sbt.paths.tests.classpath")

    val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar -cp $classpath"
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => sys.error("compilation has failed: " + error))
  }

  def compileCode(code: String): Boolean = {
    import Config._

    // Step 1: create and initialize the compiler
    val reporter = new StoreReporter()
    val command = new CompilerCommand(args, emptySettings)
    val settings = command.settings
    val global = new Global(settings, reporter)
    val run = new global.Run
    global.phase = run.parserPhase
    global.globalPhase = run.parserPhase
    import global._

    // Step 2: parse the input code
    import scala.compat.Platform.EOL
    val unit = newCompilationUnit(code, "<test>")
    val tree = newUnitParser(unit).parse()
    if(reporter.hasErrors) throw new Exception("parse has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))

    // Step 3: typecheck the input code
    import analyzer._
    phase = run.namerPhase
    globalPhase = run.namerPhase
    val namer = newNamer(rootContext(unit))
    namer.enterSym(tree)
    phase = run.typerPhase
    globalPhase = run.typerPhase
    val typer = newTyper(rootContext(unit))
    val typedTree = typer.typed(tree)
    for(workItem <- unit.toCheck) workItem()
    if(reporter.hasErrors) throw new Exception("typecheck has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL) + EOL + highlight(showCode(typedTree)))

    true
  }
}
