import org.specs2.mutable.Specification

class BehaviorSpec extends Specification {
  "behavior" >> {
    val m = scala.reflect.runtime.currentMirror
    // val tb = m.mkToolBox(options = "-Xplugin-require:macroparadise -Xplugin:" + System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.6/jars/paradise_2.11.6-2.1.0-M5.jar")
    // tb.compile(tb.parse("@renesca.schema.macros.GraphSchema object MySchema {}"))

    1 mustEqual 2
  }.pendingUntilFixed
}
