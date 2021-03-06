package renesca.schema.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

class Aborter(context: whitebox.Context) {
  def abort(msg: String) {
    context.abort(context.universe.NoPosition, msg)
  }
}

class Warner(context: whitebox.Context) {
  def warning(msg: String) {
    context.warning(context.universe.NoPosition, msg)
  }
}

trait Context {
  val context: whitebox.Context
  val aborter: Aborter
  def abort(msg: String): Nothing = {
    aborter.abort(msg)
    throw new RuntimeException("This should never happen. It only forces the return of Nothing.")
  }
  val warner: Warner
  def warning(msg: String): Nothing = {
    warner.warning(msg)
    throw new RuntimeException("This should never happen. It only forces the return of Nothing.")
  }
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
class GraphSchema extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GraphSchemaMacro.graphSchema
}

object GraphSchemaMacro {
  // TODO: why are implicits not working here?
  // implicit def treeToString(l: Tree): String = l match { case Literal(Constant(string: String)) => string }
  // TODO: validation: nodeTraits(propertyTypes), nodes need to inherit exactly one NodeTrait
  // TODO: compile error when nodes inherit not only from nodeTraits

  def graphSchema(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    val env = new Patterns with Generators with Code {
      val context: c.type = c
      val aborter = new Aborter(c)
      val warner = new Warner(c)
    }
    import env._

    c.Expr[Any](annottees.map(_.tree).toList match {
      case SchemaPattern(schemaPattern) :: Nil =>

        val code = schema(Schema(schemaPattern))
        Helpers.writeFile(s"magic/${ schemaPattern.name }.generated.scala", c.universe.showCode(code))
        code
    })
  }
}
