import org.specs2._

class GenerationSpec extends mutable.Specification {
  "this is my specification" >> {
    "where example 1 must be true" >> {

      val m = scala.reflect.runtime.currentMirror
      import scala.tools.reflect.ToolBox
      val tb = m.mkToolBox()
      tb.compile(tb.parse("@renesca.schema.macros.GraphSchema object MySchema {}"))

      1 mustEqual 1
    }
  }
}
