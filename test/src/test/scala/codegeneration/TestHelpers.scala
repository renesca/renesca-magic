package codegeneration

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import renesca.schema.macros.{Code, Generators, Patterns}

import scala.reflect.macros.whitebox

trait ContextMock extends Mockito {
  val contextMock: whitebox.Context = {
    import scala.reflect.macros.{Universe => macroUniverse}
    import scala.reflect.runtime.{universe => runtimeUniverse}
    val context = mock[whitebox.Context]
    context.universe returns runtimeUniverse.asInstanceOf[macroUniverse]
    //TODO: context.abort returns 
    context
  }
}

trait CodeComparison extends Specification with ContextMock {
  val magic = new Patterns with Generators with Code {val context: contextMock.type = contextMock }

  import contextMock.universe._
  import magic._

  val wildcardRegex = "_\\$\\d+".r
  def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  def comparable(code: Tree) = withoutSpaces(comparableWildcards(showCode(code)))
  def containCode(c1: Tree, c: Tree*) = c.map { c2 => comparable(c1) must contain(comparable(c2)) }
  def generate(code: Tree) = schema(Schema(SchemaPattern.unapply(code).get))
  def generatedContainsCode(c1: Tree, c: Tree*) = containCode(generate(c1), c: _*)
  def generatedContainsCodePrint(c1: Tree, c: Tree*) = containCode({
    val g = generate(c1);
    println(showCode(c1) + "\n--- generates: ---\n" + showCode(g) + "\n----------\n");
    g 
  }, c: _*)
}

