package helpers

import org.specs2.mutable.Specification
import renesca.schema.macros.{Aborter, Code, Generators, Patterns, Warner}


trait CodeComparisonSpec extends Specification with ContextMock {
  sequential

  trait ExpectedCode

  case class With(code: String) extends ExpectedCode

  case class Not(code: String) extends ExpectedCode

  val magic = new Patterns with Generators with Code {
    val context: contextMock.type = contextMock
    val aborter = mock[Aborter].smart
    aborter.abort(anyString) answers { msg => throw new RuntimeException(msg.toString) }
    val warner = mock[Warner].smart
    warner.warning(anyString) answers { msg => throw new RuntimeException(msg.toString) }
  }

  import contextMock.universe._
  import magic.{schema, Schema, SchemaPattern}

  implicit def TreeToString(t: Tree): String = showCode(t)
  implicit def TreeToWith(t: Tree): With = With(showCode(t))
  implicit def StringToWith(code: String): With = With(code)

  private def errorMessage(source: Tree, generated: Tree, snippet: String, shouldContain: Boolean) = {
    val failMsg = if(shouldContain) "which doesn't contain:" else "which contains but shouldn't:"
    comparableWildcards(showCode(source)) +
      "\n--- generates: ---\n" +
      comparableWildcards(showCode(generated)) +
      s"\n--- $failMsg ---\n" +
      comparableWildcards(snippet) +
      "\n----------\n"
  }

  private val wildcardRegex = "_\\$\\d+".r
  private def comparableWildcards(code: String) = wildcardRegex.replaceAllIn(code, "_")
  private def withoutSpaces(code: String) = code.replaceAll("\\s", "")
  private def comparable(code: String) = withoutSpaces(comparableWildcards(code))
  private def containCode(source: Tree, generated: Tree, snippets: ExpectedCode*) = snippets.map {
    case With(snippet) =>
      comparable(showCode(generated)) must (contain(comparable(snippet))).setMessage(errorMessage(source, generated, snippet, true))
    case Not(snippet)  =>
      comparable(showCode(generated)) must (not(contain(comparable(snippet)).setMessage(errorMessage(source, generated, snippet, false))))
  }
  def generate(code: Tree) = schema(Schema(SchemaPattern.unapply(code).get))
  def generatedContainsCode(source: Tree, snippets: ExpectedCode*) = containCode(source, generate(source), snippets: _*)
}

