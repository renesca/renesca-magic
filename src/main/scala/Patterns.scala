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


  object GroupPattern {
    //TODO: statements
    //TODO: extract modifier pattern
    def unapply(tree: Tree): Option[GroupPattern] = condOpt(tree) {
      case q""" $mods trait $name extends ..$superTypes { List(..$groupNodes) }""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Group"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get =>
        GroupPattern(name, superTypes, groupNodes)
      case q""" $mods trait $name extends ..$superTypes""" if mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName("Group"))), termNames.CONSTRUCTOR), Nil) => true
        case _                                                                        => false
      }.get =>
        GroupPattern(name, superTypes, Nil)
    }
  }

  case class GroupPattern(name: String, _superTypes: List[String], nodes: List[String]) extends NamePattern with SuperTypesPattern

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
      case q"""@Node class $name extends ..${superTypes} { ..$statements }""" =>
        NodePattern(name, superTypes, statements)
    }
  }

  case class NodePattern(name: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

  object RelationPattern {
    def unapply(tree: Tree): Option[RelationPattern] = condOpt(tree) {
      case q"""@Relation class $name (startNode:$startNode, endNode:$endNode) extends ..$superTypes {..$statements}""" =>
        RelationPattern(name, startNode, endNode, superTypes, statements)
    }
  }

  case class RelationPattern(name: String, startNode: String, endNode: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with StartEndNodePattern with SuperTypesPattern with StatementsPattern

  object HyperRelationPattern {
    def unapply(tree: Tree): Option[HyperRelationPattern] = condOpt(tree) {
      case q"""@HyperRelation class $name (startNode:$startNode, endNode:$endNode) extends ..$superTypes {..$statements}""" =>
        HyperRelationPattern(name, startNode, endNode, superTypes, statements)
    }
  }

  case class HyperRelationPattern(name: String, startNode: String, endNode: String, _superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StartEndNodePattern with StatementsPattern
}


