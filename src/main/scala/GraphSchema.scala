package renesca.schema.macros

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import PartialFunction._
import scala.annotation.StaticAnnotation

trait Context {
  val context: whitebox.Context
}

class GraphSchema extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GraphSchemaMacro.graphSchema
}

object GraphSchemaMacro {
  // TODO: why are implicits not working here?
  // implicit def treeToString(l: Tree): String = l match { case Literal(Constant(string: String)) => string }
  // TODO: validation: nodeTraits(propertyTypes), nodes need to inherit exactly one NodeTrait
  // TODO: compile error when nodes inherit not only from nodeTraits

  def graphSchema(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    // import schema and patterns:
    // we need to pass the context and its type to the objects we want to
    // import, because the macro types depend on the context. furthermore, we
    // need to make sure that we import the same patternContext as the
    // schemaContext, otherwise the compiler complains about mismatching types.

    val env = new Patterns with Generators with Code {val context: c.type = c }
    import env._

    c.Expr[Any](annottees.map(_.tree).toList match {
      case SchemaPattern(schemaPattern) :: Nil =>

        Schema // run assertions
        Helpers.crashOnAsserted()
        schema(Schema(schemaPattern))
    })
  }
}


