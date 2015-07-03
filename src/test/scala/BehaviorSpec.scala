import org.specs2.mutable.Specification

class BehaviorSpec extends Specification {
  "behavior" >> {
    val paradiseJar = System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.6/jars/paradise_2.11.6-2.1.0-M5.jar"
    val classpath = System.getProperty("sbt.paths.tests.classpath")
    println(classpath)
    val code = "@renesca.schema.macros.GraphSchema object MySchema"

    // Step 1: create and initialize the compiler
    import scala.tools.cmd.CommandLineParser
    import scala.tools.nsc.{Global, CompilerCommand, Settings}
    import scala.tools.nsc.reporters.StoreReporter
    val options = s"-Xplugin-require:macroparadise -Xplugin:$paradiseJar -cp $classpath"
    val args = CommandLineParser.tokenize(options)
    val emptySettings = new Settings(error => sys.error("compilation has failed: " + error))
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
    if (reporter.hasErrors) throw new Exception("parse has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))

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
    for (workItem <- unit.toCheck) workItem()
    if (reporter.hasErrors) throw new Exception("typecheck has failed:" + EOL + (reporter.infos map (_.msg) mkString EOL))

    // Step 4: do the checks
    println(typedTree)

    1 mustEqual 1
  }
}
