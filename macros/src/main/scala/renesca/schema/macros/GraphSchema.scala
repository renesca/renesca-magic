package renesca.schema.macros

import scala.reflect.macros.whitebox
import scala.language.experimental.macros
import PartialFunction._
import scala.annotation.StaticAnnotation

object Helpers {
  def nodeToFactoryName(name: String) = name + "Factory"
  def rev(s: String) = "rev_" + s
  def nameToPlural(name: String) = {
    val lower = name.take(1).toLowerCase + name.drop(1)
    val suffix = if(lower.endsWith("s")) "" else "s"
    lower + suffix
  }
  def nameToLabel(name: String) = name.toUpperCase
  def relationName(start: String, end: String) = s"${ start }To${ end }"


  trait NamePattern {
    def name: String
    def name_type(implicit c: whitebox.Context) = c.universe.TypeName(name)
    def name_term(implicit c: whitebox.Context) = c.universe.TermName(name)
    def name_label = nameToLabel(name)
    def name_plural = nameToPlural(name)
    def name_plural_term(implicit c: whitebox.Context) = c.universe.TermName(name_plural)
  }

  trait SuperTypesPattern {
    def superTypes: List[String]
    def superTypes_type(implicit c: whitebox.Context) = superTypes.map(c.universe.TypeName(_))
  }

  trait StartEndNodePattern {
    def startNode: String
    def endNode: String
    def startNode_type(implicit c: whitebox.Context) = c.universe.TypeName(startNode)
    def startNode_term(implicit c: whitebox.Context) = c.universe.TermName(startNode)
    def endNode_type(implicit c: whitebox.Context) = c.universe.TypeName(endNode)
    def endNode_term(implicit c: whitebox.Context) = c.universe.TermName(endNode)
  }

  trait StatementsPattern {
    def statements(implicit c: whitebox.Context): List[c.universe.Tree]
  }

}

class GraphSchema extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro GraphSchemaMacro.graphSchema
}


object GraphSchemaMacro {
  // TODO: why are implicits not working here?
  // implicit def treeToString(l: Tree): String = l match { case Literal(Constant(string: String)) => string }
  // TODO: validation: nodeTraits(propertyTypes), nodes need to inherit exactly one NodeTrait
  // TODO: compile error when nodes inherit not only from nodeTraits

  def graphSchema(context: whitebox.Context)(annottees: context.Expr[Any]*): context.Expr[Any] = {
    import Helpers._
    implicit val c = context
    import c._


    object SchemaPattern {
      def unapply(tree: Tree): Option[SchemaPattern] = condOpt(tree) {
        case q""" object $name extends ..$superTypes { ..$statements } """ =>
          SchemaPattern(name.toString, superTypes.map(_.toString) diff List("scala.AnyRef"), statements)
      }
    }
    case class SchemaPattern(name: String, superTypes: List[String], statements: List[Tree]) extends NamePattern with StatementsPattern

    object GroupPattern {
      //TODO: statements
      //TODO: extract modifier pattern
      def unapply(tree: Tree): Option[GroupPattern] = condOpt(tree) {
        case q""" $mods trait $name extends ..$superTypes { List(..$groupNodes) }""" if mods.annotations.collectFirst {
          case Apply(Select(New(Ident(TypeName("Group"))), termNames.CONSTRUCTOR), Nil) => true
          case _                                                                        => false
        }.get =>
          GroupPattern(name.toString, superTypes.map { _.toString } diff List("scala.AnyRef"), groupNodes.map { _.toString })
        case q""" $mods trait $name extends ..$superTypes""" if mods.annotations.collectFirst {
          case Apply(Select(New(Ident(TypeName("Group"))), termNames.CONSTRUCTOR), Nil) => true
          case _                                                                        => false
        }.get =>
          GroupPattern(name.toString, superTypes.map { _.toString } diff List("scala.AnyRef"), Nil)
      }
    }
    case class GroupPattern(name: String, superTypes: List[String], nodes: List[String]) extends NamePattern with SuperTypesPattern

    object NodeTraitPattern {
      def unapply(tree: Tree): Option[NodeTraitPattern] = condOpt(tree) {
        //http://stackoverflow.com/questions/26305528/scala-annotations-are-not-found
        case q""" $mods trait $name extends ..$superTypes { ..$statements } """ if mods.annotations.collectFirst {
          case Apply(Select(New(Ident(TypeName("Node"))), termNames.CONSTRUCTOR), Nil) => true
          case _                                                                       => false
        }.get =>
          NodeTraitPattern(name.toString, superTypes.map { _.toString } diff List("scala.AnyRef"), statements)
      }
    }
    case class NodeTraitPattern(name: String, superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

    object NodePattern {
      def unapply(tree: Tree): Option[NodePattern] = condOpt(tree) {
        case q"""@Node class $name extends ..${superTypes} { ..$statements }""" =>
          NodePattern(name.toString, superTypes.map { _.toString }, statements)
      }
    }
    case class NodePattern(name: String, superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StatementsPattern

    object RelationPattern {
      def unapply(tree: Tree): Option[Relation] = condOpt(tree) {
        case q"""@Relation class $name (startNode:$startNode, endNode:$endNode) {..$statements}""" =>
          Relation(name.toString, startNode.toString, endNode.toString, statements)
      }
    }
    case class Relation(name: String, startNode: String, endNode: String, statements: List[Tree]) extends NamePattern with StartEndNodePattern with StatementsPattern

    object HyperRelationPattern {
      def unapply(tree: Tree): Option[HyperRelation] = condOpt(tree) {
        case q"""@HyperRelation class $name (startNode:$startNode, endNode:$endNode) extends ..$superTypes {..$statements}""" =>
          HyperRelation(name.toString, startNode.toString, endNode.toString, superTypes.map { _.toString } diff List("scala.AnyRef"), statements)
      }
    }
    case class HyperRelation(name: String, startNode: String, endNode: String, superTypes: List[String], statements: List[Tree]) extends NamePattern with SuperTypesPattern with StartEndNodePattern with StatementsPattern {
      def startRelation = relationName(startNode, name)
      def startRelation_type = TypeName(startRelation)
      def startRelation_term = TermName(startRelation)
      def startRelation_label = nameToLabel(startRelation)
      def endRelation = relationName(name, endNode)
      def endRelation_type = TypeName(endRelation)
      def endRelation_term = TermName(endRelation)
      def endRelation_label = nameToLabel(endRelation)
    }

    case class Node(
                     name: String,
                     superTypes: List[String],
                     neighbours: List[(String, String)],
                     rev_neighbours: List[(String, String)],
                     statements: List[Tree],
                     flatStatements: List[Tree]
                     ) extends NamePattern with SuperTypesPattern {
      def neighbours_terms = neighbours.map { case (relation, endNode) =>
        (TermName(nameToPlural(relation)), TypeName(endNode), TermName(endNode))
      }
      def rev_neighbours_terms = rev_neighbours.map { case (relation, startNode) =>
        (TermName(nameToPlural(relation)), TypeName(startNode), TermName(startNode))
      }
    }

    object NodeTrait {

      import Schema._

      def apply(
                 nodeTraitPatterns: List[NodeTraitPattern],
                 selectedNodePatterns: List[NodePattern],
                 relationPatterns: List[Relation],
                 hyperRelationPatterns: List[HyperRelation],
                 nodeTraitPattern: NodeTraitPattern
                 ) =
        new NodeTrait(
          name = nodeTraitPattern.name,
          superTypes = if(nodeTraitPattern.superTypes.nonEmpty) nodeTraitPattern.superTypes else List("SchemaNode"),
          subNodes = nodeTraitToNodes(nodeTraitPatterns, selectedNodePatterns, nodeTraitPattern),
          subRelations = nodeNamesToRelations(nodeTraitToNodes(nodeTraitPatterns, selectedNodePatterns, nodeTraitPattern), relationPatterns).map(_.name),
          subHyperRelations = nodeNamesToRelations(nodeTraitToNodes(nodeTraitPatterns, selectedNodePatterns, nodeTraitPattern), hyperRelationPatterns).map(_.name),
          commonHyperNodeTraits = nodeTraitToCommonHyperNodeTraits(nodeTraitPatterns, selectedNodePatterns, hyperRelationPatterns, nodeTraitPattern),
          statements = nodeTraitPattern.statements
        )
    }
    case class NodeTrait(name: String,
                         superTypes: List[String],
                         subNodes: List[String],
                         subRelations: List[String],
                         subHyperRelations: List[String],
                         commonHyperNodeTraits: List[String],
                         statements: List[Tree]
                          ) extends NamePattern with SuperTypesPattern {
      def commonHyperNodeTraits_type = commonHyperNodeTraits.map(TypeName(_))
    }
    case class Group(name: String,
                     nodes: List[String],
                     relations: List[String],
                     hyperRelations: List[String],
                     nodeTraits: List[NodeTrait]
                      ) extends NamePattern

    object Schema {
      def apply(schema: SchemaPattern): Schema = {
        import schema._
        val nodePatterns: List[NodePattern] = schema.statements.collect { case NodePattern(node) => node }
        val relations: List[Relation] = schema.statements.collect { case RelationPattern(relationPattern) => relationPattern }
        val hyperRelations: List[HyperRelation] = schema.statements.collect { case HyperRelationPattern(hyperRelationPattern) => hyperRelationPattern }
        val nodeTraitPatterns: List[NodeTraitPattern] = schema.statements.collect { case NodeTraitPattern(nodeTraitpattern) => nodeTraitpattern }
        val groupPatterns: List[GroupPattern] = schema.statements.collect { case GroupPattern(groupPattern) => groupPattern }

        val nodes = nodePatterns.map { nodePattern => import nodePattern._
          Node(name, superTypes, neighbours(nodePattern, relations), rev_neighbours(nodePattern, relations), statements, flatNodeStatements(nodePatterns, nodeTraitPatterns, nodePattern))
        }
        val nodeTraits = nodeTraitPatterns.map(nodeTraitPattern => NodeTrait(nodeTraitPatterns, nodePatterns, relations, hyperRelations, nodeTraitPattern))
        val groups = groupPatterns.map(groupPattern =>
          Group(groupPattern.name,
            nodes = groupToNodes(groupPatterns, groupPattern),
            relations = groupToRelations(groupPatterns, relations, groupPattern),
            hyperRelations = groupToRelations(groupPatterns, hyperRelations, groupPattern),
            nodeTraits = nodeTraitPatterns.map(nodeTraitPattern =>
              NodeTrait(nodeTraitPatterns, groupToNodes(groupPatterns, groupPattern).map(nameToPattern(nodePatterns, _)), relations, hyperRelations, nodeTraitPattern))
          )
        )
        Schema(name, superTypes, nodes, relations, hyperRelations, nodeTraits, groups, statements)
      }

      def flatNodeStatements(nodePatterns: List[NodePattern], nodeTraitPatterns: List[NodeTraitPattern], nodePattern: NodePattern): List[Tree] = {
        val superTypes: List[StatementsPattern with NamePattern with SuperTypesPattern] = nodePattern.superTypes.map(superType => nameToPattern(nodePatterns ++ nodeTraitPatterns, superType))
        val flatSuperTypes: List[StatementsPattern] = nodePattern :: patternToFlatSuperTypes(superTypes, nodePattern)
        flatSuperTypes.flatMap(_.statements)
      }
      def nameToPattern[P <: NamePattern](patterns: List[P], name: String): P = patterns.find(_.name == name).get
      def neighbours(nodePattern: NodePattern, relations: List[Relation]): List[(String, String)] = relations.filter(_.startNode == nodePattern.name).map(r => r.name -> r.endNode)
      def rev_neighbours(nodePattern: NodePattern, relations: List[Relation]): List[(String, String)] = relations.filter(_.endNode == nodePattern.name).map(r => r.name -> r.startNode)
      def isDeepSuperType[P <: NamePattern with SuperTypesPattern](patterns: List[P], subPattern: P, superPattern: P): Boolean = {
        subPattern.superTypes match {
          case Nil        => false
          case superTypes => superTypes.exists { name =>
            val pattern = nameToPattern(patterns, name)
            superPattern.name == name || isDeepSuperType(patterns, pattern, superPattern)
          }
        }
      }
      def patternToSuperTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = pattern.superTypes.map(nameToPattern(patterns, _))
      def patternToFlatSuperTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = patterns.filter { superPattern =>
        isDeepSuperType(patterns, pattern, superPattern)
      }
      def patternToSubTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = patterns.filter(_.superTypes.contains(pattern.name))
      def patternToFlatSubTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = patterns.filter { supPattern =>
        isDeepSuperType(patterns, supPattern, pattern)
      }
      def nodeTraitToNodes(nodeTraits: List[NodeTraitPattern], nodePatterns: List[NodePattern], nodeTrait: NodeTraitPattern): List[String] = {
        patternToFlatSuperTypes(nodeTraits, nodeTrait).flatMap(superTrait => nodePatterns.filter(_.superTypes contains superTrait.name)).distinct.map(_.name)
      }
      def nodeNamesToRelations[R <: StartEndNodePattern](nodeNames: List[String], relations: List[R]): List[R] = {
        relations.filter(relation => nodeNames.contains(relation.startNode) && nodeNames.contains(relation.endNode))
      }
      def nodeTraitToCommonHyperNodeTraits(nodeTraitPatterns: List[NodeTraitPattern], nodePatterns: List[NodePattern], hyperRelations: List[HyperRelation], nodeTrait: NodeTraitPattern): List[String] = {
        val subHyperRelations = nodeNamesToRelations(nodeTraitToNodes(nodeTraitPatterns, nodePatterns, nodeTrait), hyperRelations)
        val flatSuperTypes: List[List[String]] = subHyperRelations.map(hyperRelation => patternToFlatSuperTypes(nodeTraitPatterns, hyperRelation).map(_.name))
        flatSuperTypes.reduce(_ intersect _)
      }
      def groupToNodes(groupPatterns: List[GroupPattern], groupPattern: GroupPattern): List[String] = patternToFlatSubTypes(groupPatterns, groupPattern).flatMap(_.nodes)
      def groupToRelations(groupPatterns: List[GroupPattern], relations: List[NamePattern with StartEndNodePattern], groupPattern: GroupPattern): List[String] =
        nodeNamesToRelations(groupToNodes(groupPatterns, groupPattern), relations).map(_.name)
    }

    case class Schema(
                       name: String,
                       superTypes: List[String],
                       nodes: List[Node],
                       relations: List[Relation],
                       hyperRelations: List[HyperRelation],
                       nodeTraits: List[NodeTrait],
                       groups: List[Group],
                       statements: List[Tree]
                       ) extends NamePattern with SuperTypesPattern

    c.Expr[Any](annottees.map(_.tree).toList match {
      case SchemaPattern(schemaPattern) :: Nil =>
        object Code {

          def relationStart(schema: Schema, name: String): String = schema.relations.find(_.name == name).get.startNode
          def relationEnd(schema: Schema, name: String): String = schema.relations.find(_.name == name).get.endNode

          def propertyGetter(name: String, typeName: Tree) =
            q""" def ${ TermName(name) }:$typeName = node.properties(${ name }).asInstanceOf[${ TypeName(typeName.toString + "PropertyValue") }] """
          def propertyOptionGetter(name: String, typeName: Tree) =
            q""" def ${ TermName(name) }:Option[$typeName] = node.properties.get(${ name }).asInstanceOf[Option[${ TypeName(typeName.toString + "PropertyValue") }]] """
          def propertySetter(name: String, typeName: Tree) =
            q""" def ${ TermName(name + "_$eq") }(newValue:$typeName){ node.properties(${ name }) = newValue} """
          def propertyOptionSetter(name: String, typeName: Tree) =
            q""" def ${ TermName(name + "_$eq") }(newValue:Option[$typeName]){ if(newValue.isDefined) node.properties(${ name }) = newValue.get else node.properties -= ${ name } }"""
          def generatePropertyAccessors(statement: Tree): List[Tree] = statement match {
            case q"val $propertyName:Option[$propertyType]" => List(propertyOptionGetter(propertyName.toString, propertyType))
            case q"var $propertyName:Option[$propertyType]" => List(propertyOptionGetter(propertyName.toString, propertyType), propertyOptionSetter(propertyName.toString, propertyType))
            case q"val $propertyName:$propertyType"         => List(propertyGetter(propertyName.toString, propertyType))
            case q"var $propertyName:$propertyType"         => List(propertyGetter(propertyName.toString, propertyType), propertySetter(propertyName.toString, propertyType))
            case somethingElse                              => List(somethingElse)
          }

          def generateIndirectNeighbourAccessors(schema: Schema, statement: Tree): Tree = statement match {
            case q"""def $chainName = $rel1 --> $rel2""" =>
              q"""def $chainName:Set[${ TypeName(relationEnd(schema, rel2.toString)) }] = ${ TermName(nameToPlural(rel1.toString)) }.flatMap(_.${ TermName(nameToPlural(rel2.toString)) })"""
            case q"""def $chainName = $rel1 <-- $rel2""" =>
              q"""def $chainName:Set[${ TypeName(relationStart(schema, rel1.toString)) }] = ${ TermName(rev(nameToPlural(rel2.toString))) }.flatMap(_.${ TermName(rev(nameToPlural(rel1.toString))) })"""
            case otherStatement                          => otherStatement
          }

          def isOptionalType(typeName: Tree) = typeName match {
            case q"Option[$x]" => true
            case _             => false
          }


          def nodeFactories(schema: Schema): List[Tree] = schema.nodes.map { node => import node._
            val localNonOptionalParamsWithoutDefault = statements.collect {
              case statement@(q"val $x:$propertyType") if !isOptionalType(propertyType) => statement
              case statement@(q"var $x:$propertyType") if !isOptionalType(propertyType) => statement
            }
            val localParamsWithDefault = statements.collect {
              case statement@(q"val $x: $y = $default") if !default.isEmpty => statement
              case statement@(q"var $x: $y = $default") if !default.isEmpty => statement
            }
            val localOptionalParamsWithoutDefault = statements.collect {
              case statement@(q"val $propertyName:Option[$propertyType]") => q"val $propertyName:Option[$propertyType] = None"
              case statement@(q"var $propertyName:Option[$propertyType]") => q"val $propertyName:Option[$propertyType] = None"
            }

            val localParams = List(localNonOptionalParamsWithoutDefault ::: localParamsWithDefault ::: localOptionalParamsWithoutDefault)

            val properties = statements.map {
              case q"val $propertyName:$propertyType = $x" if !isOptionalType(propertyType) => propertyName
              case q"var $propertyName:$propertyType = $x" if !isOptionalType(propertyType) => propertyName
            }
            val optionalProperties = statements.map {
              case q"val $propertyName:Option[$x] = $y" => propertyName
              case q"var $propertyName:Option[$x] = $y" => propertyName
            }

            val propertyAssignments = properties.map { propertyName => q"schemaNode.node.properties(${ propertyName.toString }) = $propertyName" }
            val optionalPropertyAssignments = properties.map { propertyName => q"if($propertyName.isDefined) schemaNode.node.properties(${ propertyName.toString }) = $propertyName.get" }

            q"""
           object $name_term extends SchemaNodeFactory[$name_type] {
             def create(node: Node) = new $name_type(node)
             val label = Label($name_label)
             def local(...$localParams) = {
              val schemaNode = super.local
              ..$propertyAssignments
              ..$optionalPropertyAssignments
              schemaNode
             }
           }
           """
          }

          def nodeClasses(schema: Schema): List[Tree] = schema.nodes.map { node => import node._
            val directNeighbours = node.neighbours_terms.map {
              case (relationPlural, endNode_type, endNode_term) =>
                q"""def $relationPlural:Set[$endNode_type] = successorsAs($endNode_term)"""
            }

            val directRevNeighbours = node.rev_neighbours_terms.map {
              case (relationPlural, startNode_type, startNode_term) =>
                q"""def $relationPlural:Set[$startNode_type] = predecessorsAs($startNode_term)"""
            }

            val nodeBody = statements.map(generateIndirectNeighbourAccessors(schema, _)).flatMap(generatePropertyAccessors(_))

            q"""
           case class $name_type(node: Node) extends ..$superTypes_type {
             ..$directNeighbours
             ..$directRevNeighbours
             ..$nodeBody
           }
           """
          }

          def relationFactories(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
            q"""
           object $name_term extends SchemaRelationFactory[$startNode_type, $name_type, $endNode_type] {
               def startNodeFactory = $startNode_term
               def endNodeFactory = $endNode_term
               def relationType = RelationType($name_label)
               def create(relation: Relation) = $name_term(
                 $startNode_term.create(relation.startNode),
                 relation,
                 $endNode_term.create(relation.endNode))
           }
           """
          }

          def relationClasses(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
            q"""
           case class $name_type(startNode: $startNode_type, relation: Relation, endNode: $endNode_type)
             extends SchemaRelation[$startNode_type, $endNode_type] {
             ..$statements
           }
           """
          }


          def hyperRelationFactories(schema: Schema): List[Tree] = schema.hyperRelations.map { hyperRelation => import hyperRelation._
            //TODO: local method based on properties (like NodeFactory)
            q"""
           object $name_term extends SchemaHyperRelationFactory[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type]
             with SchemaAbstractRelationFactory[$startNode_type, $name_type, $endNode_type] {

             override def label = Label($name_label)
             override def startRelationType = RelationType($startRelation_label)
             override def endRelationType = RelationType($endRelation_label)

             override def startNodeFactory = $startNode_term
             override def factory = $name_term
             override def endNodeFactory = $endNode_term

             override def create(node: Node) = new $name_type(node)
             override def startRelationCreate(relation: Relation) = $startRelation_term(startNodeFactory.create(relation.startNode), relation, factory.create(relation.endNode))
             override def endRelationCreate(relation: Relation) = $endRelation_term(factory.create(relation.startNode), relation, endNodeFactory.create(relation.endNode))
           }
           """
          }

          def hyperRelationClasses(schema: Schema): List[Tree] = schema.hyperRelations.map { hyperRelation => import hyperRelation._
            //TODO: generate indirect neighbour-accessors based on hyperrelations
            //TODO: property accessors
            List( q"""
           case class $name_type(node:Node)
              extends SchemaHyperRelation[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type]
              with ..$superTypes_type {
             ..$statements
           }
           """, q"""
           case class $startRelation_type(startNode: $startNode_type, relation: Relation, endNode: $name_type)
             extends SchemaRelation[$startNode_type, $name_type]
           """, q"""
           case class $endRelation_type(startNode: $name_type, relation: Relation, endNode: $endNode_type)
             extends SchemaRelation[$name_type, $endNode_type]
           """)
          }.flatten

          def nodeSuperTraits(schema: Schema): List[Tree] = schema.nodeTraits.map { nodeTrait => import nodeTrait._
            val traitBody = statements.flatMap(generatePropertyAccessors(_))
            q""" trait $name_type extends ..$superTypes_type { ..$traitBody } """
          }

          def groupFactories(schema: Schema): List[Tree] = schema.groups.map { group => import group._
            q""" object $name_term {def empty = new $name_type(Graph.empty) } """
          }

          def groupClasses(schema: Schema): List[Tree] = schema.groups.map { group => import group._
            // TODO: create subgroups

            def itemSets(nameAs: String, names: List[String]) = names.map { name => q""" def ${ TermName(nameToPlural(name)) }: Set[${ TypeName(name) }] = ${ TermName(nameAs) }(${ TermName(name) }) """ }
            def allOf(items: List[String]) = items.foldLeft[Tree](q"Set.empty") { case (q"$all", name) => q"$all ++ ${ TermName(nameToPlural(name)) }" }

            val nodeSets = itemSets("nodesAs", nodes)
            val relationSets = itemSets("relationsAs", relations)
            val hyperRelationSets = itemSets("hyperRelationsAs", hyperRelations)

            val nodeTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
              q"def $name_plural_term:Set[$name_type] = ${ allOf(subNodes) }"
            }
            val relationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
              q"def ${ TermName(nameToPlural(name + "Relation")) }:Set[_ <: SchemaRelation[$name_type, $name_type]] = ${ allOf(subRelations) }"
            }
            val hyperRelationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
              q"""def ${ TermName(nameToPlural(name + "HyperRelation")) } :Set[SchemaHyperRelation[
                  $name_type,
                  _ <: SchemaRelation[$name_type, _],
                  _ <: SchemaHyperRelation[$name_type, _, _, _, $name_type] with ..$commonHyperNodeTraits_type,
                  _ <: SchemaRelation[_, $name_type],
                  $name_type]
                  with ..$commonHyperNodeTraits_type]
              = ${ allOf(subHyperRelations) }"""
            }

            q"""
           case class $name_type(graph: Graph) extends SchemaGraph {
             ..$nodeSets
             ..$relationSets
             ..$hyperRelationSets

             ..$nodeTraitSets
             ..$relationTraitSets
             ..$hyperRelationTraitSets

             def nodes: Set[SchemaNode] = ${ allOf(nodes) }
             def relations: Set[_ <: SchemaRelation[_,_]] = ${ allOf(relations) }
             def hyperRelations: Set[_ <: SchemaHyperRelation[_,_,_,_,_]] = ${ allOf(hyperRelations) }
           }
           """
          }

          def otherStatements(schema: Schema): List[Tree] = schema.statements.flatMap {
            case NodePattern(_)
                 | RelationPattern(_)
                 | HyperRelationPattern(_)
                 | NodeTraitPattern(_)
                 | GroupPattern(_) => None
            case other             => Some(other)
          }


          def schema(schema: Schema): Tree = {
            import schema.{name_term, superTypes_type}
            q"""
           object $name_term extends ..$superTypes_type {
             import renesca.graph.{Graph,Label,RelationType,Node,Relation}
             import renesca.parameter.StringPropertyValue
             import renesca.parameter.implicits._

             ..${ nodeFactories(schema) }
             ..${ nodeClasses(schema) }

             ..${ relationFactories(schema) }
             ..${ relationClasses(schema) }

             ..${ hyperRelationFactories(schema) }
             ..${ hyperRelationClasses(schema) }

             ..${ nodeSuperTraits(schema) }
             ..${ groupFactories(schema) }
             ..${ groupClasses(schema) }

             ..${ otherStatements(schema) }
           }
           """
          }
        }

        Code.schema(Schema(schemaPattern))
    })
  }
}
