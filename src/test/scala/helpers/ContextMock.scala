package helpers

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

import scala.reflect.macros.whitebox

trait ContextMock extends Specification with Mockito {
  val contextMock: whitebox.Context = {
    import scala.reflect.macros.{Universe => macroUniverse}
    import scala.reflect.runtime.{universe => runtimeUniverse}
    val context = mock[whitebox.Context].smart
    context.universe returns runtimeUniverse.asInstanceOf[macroUniverse]
    context
  }
}
