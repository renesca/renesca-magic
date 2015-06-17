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

trait ExpectedCode

case class With(code: String) extends ExpectedCode

case class Not(code: String) extends ExpectedCode

trait CodeComparison extends Specification with ContextMock {
  sequential

  val magic = new Patterns with Generators with Code {val context: contextMock.type = contextMock }

  import contextMock.universe._
  import magic._

  implicit def TreeToString(t: Tree): String = showCode(t)
  implicit def TreeToWith(t: Tree): With = With(showCode(t))
  implicit def StringToWith(code: String): With = With(code)

  private def errorMessage(source: Tree, generated: Tree, snippet: String, shouldContain: Boolean) = {
    val failMsg = if (shouldContain) "which doesn't contain:" else "which contains but shouldn't:"
    comparableWildcards(showCode(source)) +
      "\n--- generates: ---\n" +
      comparableWildcards(showCode(generated)) +
      s"\n--- $failMsg ---\n" +
      comparableWildcards(snippet) +
      "\n----------\n"
  }

  val wildcardRegex = "_\\$\\d+".r
  def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  def comparable(code: String) = withoutSpaces(comparableWildcards(code))
  def containCode(source: Tree, generated: Tree, snippets: ExpectedCode*) = snippets.map {
    case With(snippet) =>
      comparable(showCode(generated)) must (contain(comparable(snippet))).setMessage(errorMessage(source, generated, snippet, true))
    case Not(snippet) =>
      comparable(showCode(generated)) must (not(contain(comparable(snippet)).setMessage(errorMessage(source, generated, snippet, false))))
  }
  def generate(code: Tree) = schema(Schema(SchemaPattern.unapply(code).get))
  def generatedContainsCode(source: Tree, snippets: ExpectedCode*) = containCode(source, generate(source), snippets: _*)
}

