package renesca.schema.macros

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import PartialFunction._

trait PatternTraits extends Context {

  import context.universe._

  implicit def typeNameToString(tn: TypeName): String = tn.toString
  implicit def termNameToString(tn: TermName): String = tn.toString
  implicit def treeToString(tn: Tree): String = tn.toString
  implicit def treeListToStringList(tnl: List[Tree]): List[String] = tnl.map(_.toString)

  trait NamePattern {
    def name: String
  }

  trait SuperTypesPattern {
    def _superTypes: List[String]
    def superTypes = _superTypes diff List("scala.AnyRef")
  }

  trait StartEndNodePattern {
    def startNode: String
    def endNode: String
  }

  trait StatementsPattern {
    def statements: List[Tree]
  }

}

trait Patterns extends Context with PatternTraits {

  import context.universe._

  case class SchemaPattern(name: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern

  object SchemaPattern {
    def unapply(tree: Tree): Option[SchemaPattern] = condOpt(tree) {
      case q""" object $name extends ..$superTypes { ..$statements } """ =>
        SchemaPattern(name, superTypes, statements)
    }
  }

  object GraphPattern {
    //TODO: statements
    //TODO: extract modifier pattern
    def unapply(tree: Tree): Option[GraphPattern] = condOpt(tree) {
      case q""" $mods trait $name extends ..$superTypes { Nodes(..$graphNodes) }""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Graph"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get => GraphPattern(name, superTypes, graphNodes)

      case q""" $mods trait $name extends ..$superTypes""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Graph"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get => GraphPattern(name, superTypes, Nil)

      case q""" $mods class $name extends ..$superTypes { ..$statements }""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Graph"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get => abort(s"Graph class `$name` is not allowed. Use a trait instead.")

      case q""" $mods object $name extends ..$superTypes { ..$statements }""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Graph"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get => abort(s"Graph object `$name` is not allowed. Use a trait instead.")
    }
  }

  case class GraphPattern(name: String, _superTypes: List[String], nodes: List[String]) extends NamePattern with SuperTypesPattern

  object NodeTraitPattern {
    def unapply(tree: Tree): Option[NodeTraitPattern] = condOpt(tree) {
      //http://stackoverflow.com/questions/26305528/scala-annotations-are-not-found
      case q""" $mods trait $name extends ..$superTypes { ..$statements } """ if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Node"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                       => false
      }.get =>
        NodeTraitPattern(name, superTypes, statements)
    }
  }

  case class NodeTraitPattern(name: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

  object RelationTraitPattern {
    def unapply(tree: Tree): Option[RelationTraitPattern] = condOpt(tree) {
      case q""" $mods trait $name extends ..$superTypes { ..$statements } """ if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Relation"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                           => false
      }.get =>
        RelationTraitPattern(name, superTypes, statements)
    }
  }

  case class RelationTraitPattern(name: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

  object NodePattern {
    def unapply(tree: Tree): Option[NodePattern] = condOpt(tree) {
      case q"""@Node class $name extends ..${superTypes} { ..$statements }"""  =>
        NodePattern(name, superTypes, statements)
      case q"""@Node object $name extends ..${superTypes} { ..$statements }""" =>
        abort(s"Node object `$name` is not allowed. Use a class or trait instead.")
    }
  }

  case class NodePattern(name: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

  object RelationPattern {
    def unapply(tree: Tree): Option[RelationPattern] = condOpt(tree) {
      case q"""@Relation class $name (startNode:$startNode, endNode:$endNode) extends ..$superTypes {..$statements}""" =>
        RelationPattern(name, startNode, endNode, superTypes, statements)

      case q"""@Relation class $name extends ..$superTypes {..$statements}"""   =>
        abort(s"Relation class `$name` needs startNode and endNode.")
      case q"""@Relation object $name  extends ..$superTypes {..$statements}""" =>
        abort(s"Relation object `$name` is not allowed. Use a class or trait instead.")
    }
  }

  case class RelationPattern(name: String, startNode: String, endNode: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with StartEndNodePattern with SuperTypesPattern with StatementsPattern

  object HyperRelationPattern {
    def unapply(tree: Tree): Option[HyperRelationPattern] = condOpt(tree) {
      case q"""@HyperRelation class $name (startNode:$startNode, endNode:$endNode) extends ..$superTypes {..$statements}""" =>
        HyperRelationPattern(name, startNode, endNode, superTypes, statements)

      case q"""@HyperRelation class $name extends ..$superTypes {..$statements}"""  =>
        abort(s"HyperRelation class `$name` needs startNode and endNode.")
      case q"""@HyperRelation object $name extends ..$superTypes {..$statements}""" =>
        abort(s"HyperRelation object `$name` is not allowed. Use a class instead.")
      case q"""$mods trait $name extends ..$superTypes {..$statements}""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("HyperRelation"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                                => false
      }.get                                                                         =>
        abort(s"HyperRelation trait `$name` is not allowed. Use a class instead.")
    }
  }

  case class HyperRelationPattern(name: String, startNode: String, endNode: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StartEndNodePattern with StatementsPattern

}
