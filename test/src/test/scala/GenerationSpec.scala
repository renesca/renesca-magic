import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import renesca.schema.macros.{Code, Generators, Patterns}

import scala.reflect.macros.whitebox

trait ContextMock extends Mockito {
  val contextMock: whitebox.Context = {
    import scala.reflect.macros.{Universe => macroUniverse}
    import scala.reflect.runtime.{universe => runtimeUniverse}
    mock[whitebox.Context].universe returns runtimeUniverse.asInstanceOf[macroUniverse]
  }
}

class GenerationSpec extends Specification with Mockito with ContextMock {
  "behavior" >> {
    val m = scala.reflect.runtime.currentMirror
    import scala.tools.reflect.ToolBox
    val tb = m.mkToolBox(options = "-Xplugin-require:macroparadise -Xplugin:" + System.getProperty("user.home") + "/.ivy2/cache/org.scalamacros/paradise_2.11.6/jars/paradise_2.11.6-2.1.0-M5.jar")
    tb.compile(tb.parse("@renesca.schema.macros.GraphSchema object MySchema {}"))

    1 mustEqual 1
  }

  "Empty Schema" >> {
    val env = new Patterns with Generators with Code {val context: contextMock.type = contextMock }
    import contextMock.universe._
    import env._

    val schemaPattern = SchemaPattern.unapply(q"object A").get
    val code = schema(Schema(schemaPattern))
    println(showCode(code))

    showCode(code) mustEqual showCode( q"""
           object A {
             import renesca.{graph => raw}
             import renesca.schema._
             import renesca.parameter.StringPropertyValue
             import renesca.parameter.implicits._

             val nodeLabelToFactory = Map[raw.Label,NodeFactory[_ <: Node]]()

             trait RootNodeTraitFactory[NODE <: Node] {
               def wrap(node: raw.Node) = {
                 val factory = nodeLabelToFactory(node.labels.head).asInstanceOf[NodeFactory[NODE]]
                 factory.wrap(node)
               }
             }
           }    """)
  }
}
