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
  sequential

  val magic = new Patterns with Generators with Code {val context: contextMock.type = contextMock }

  import contextMock.universe._
  import magic._

  implicit def TreeToString(t: Tree): String = showCode(t)

  val wildcardRegex = "_\\$\\d+".r
  def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  def comparable(code: String) = withoutSpaces(comparableWildcards(code))
  def containCode(source: Tree, generated: Tree, snippets: String*) = snippets.map { snippet =>
    comparable(showCode(generated)) must (contain(comparable(snippet))).setMessage(
      comparableWildcards(showCode(source)) +
        "\n--- generates: ---\n" +
        comparableWildcards(showCode(generated)) +
        "\n--- which doesn't contain: ---\n" +
        comparableWildcards(snippet) +
        "\n----------\n"
    )
  }
  def generate(code: Tree) = schema(Schema(SchemaPattern.unapply(code).get))
  def generatedContainsCode(source: Tree, snippets: String*) = containCode(source, generate(source), snippets: _*)
}

