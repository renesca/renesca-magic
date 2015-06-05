package renesca.schema.macros

trait Generators extends Context with Patterns {

  //TODO: abort when wrong superType inheritance. example: Relation extends NodeTrait

  import context.universe._
  import Helpers._

  trait Named {
    def pattern: NamePattern
    def name = pattern.name

    def name_type = TypeName(name)
    def name_term = TermName(name)
    def name_label = nameToLabel(name)
    def name_plural = nameToPlural(name)
    def name_plural_term = TermName(name_plural)
  }

  trait SuperTypes {
    def pattern: SuperTypesPattern
    def superTypes = pattern.superTypes

    def superTypes_type = superTypes.map(TypeName(_))
  }

  trait StartEndNode {
    def pattern: StartEndNodePattern
    def startNode = pattern.startNode
    def endNode = pattern.endNode

    def startNode_type = TypeName(startNode)
    def startNode_term = TermName(startNode)
    def endNode_type = TypeName(endNode)
    def endNode_term = TermName(endNode)
  }


  trait StartEndRelation extends StartEndNode with Named {
    def pattern: StartEndNodePattern with NamePattern

    def startRelation = relationName(startNode, name)
    def startRelation_type = TypeName(startRelation)
    def startRelation_term = TermName(startRelation)
    def startRelation_label = nameToLabel(startRelation)
    def endRelation = relationName(name, endNode)
    def endRelation_type = TypeName(endRelation)
    def endRelation_term = TermName(endRelation)
    def endRelation_label = nameToLabel(endRelation)
  }

  trait HasOwnFactory {
    val hasOwnFactory: Boolean
    val parameterList: ParameterList
  }

  trait Statements {
    def pattern: StatementsPattern
    def statements = pattern.statements
  }

  object Schema {
    def apply(schemaPattern: SchemaPattern): Schema = {
      import schemaPattern._
      //TODO: dry collect
      val nodePatterns: List[NodePattern] = schemaPattern.statements.collect { case NodePattern(node) => node }
      val relationPatterns: List[RelationPattern] = schemaPattern.statements.collect { case RelationPattern(relationPattern) => relationPattern }
      val hyperRelationPatterns: List[HyperRelationPattern] = schemaPattern.statements.collect { case HyperRelationPattern(hyperRelationPattern) => hyperRelationPattern }
      val nodeTraitPatterns: List[NodeTraitPattern] = schemaPattern.statements.collect { case NodeTraitPattern(nodeTraitpattern) => nodeTraitpattern }
      val relationTraitPatterns: List[RelationTraitPattern] = schemaPattern.statements.collect { case RelationTraitPattern(nodeTraitpattern) => nodeTraitpattern }
      val groupPatterns: List[GroupPattern] = schemaPattern.statements.collect { case GroupPattern(groupPattern) => groupPattern }
      val allRelationPatterns = relationPatterns ::: hyperRelationPatterns

      val nodeTraits = nodeTraitPatterns.map(nodeTraitPattern =>
        NodeTrait(nodeTraitPattern, nodeTraitPatterns, relationTraitPatterns, nodePatterns, hyperRelationPatterns, relationPatterns, hyperRelationPatterns))
      val nodes = nodePatterns.map { rawNodePattern => {
        val nodePattern = rawNodePattern.copy(_superTypes = rawNodePattern.superTypes.filter(nodeTraits.map(_.name) contains _))
        import nodePattern._
        Node(nodePattern, superTypes, rawNodePattern.superTypes.filterNot(nodeTraits.map(_.name) contains _),
          neighbours(nodePattern, allRelationPatterns, nodePatterns, nodeTraitPatterns), rev_neighbours(nodePattern, allRelationPatterns, nodePatterns, nodeTraitPatterns),
          outRelationsToTrait = allRelationPatterns.filter(r => (patternToFlatSuperTypes(nodeTraitPatterns, nodePattern).map(_.name) contains r.startNode) && (nodeTraitPatterns.map(_.name) contains r.endNode)).map(r => (r.name, r.endNode)),
          inRelationsFromTrait = allRelationPatterns.filter(r => (patternToFlatSuperTypes(nodeTraitPatterns, nodePattern).map(_.name) contains r.endNode) && (nodeTraitPatterns.map(_.name) contains r.startNode)).map(r => (r.name, r.startNode)),
          flatSuperStatements(nodeTraitPatterns, nodePattern), findSuperFactoryParameterList(nodeTraitPatterns, nodePattern, nodeTraits))
      }}
      val relationTraits = relationTraitPatterns.map(relationTraitPattern =>
        RelationTrait(relationTraitPattern,
          flatSuperStatements(relationTraitPatterns, relationTraitPattern),
          traitCanHaveOwnFactory(allRelationPatterns ::: nodeTraitPatterns ::: relationTraitPatterns, relationTraitPattern))) //TODO: why nodeTraitPatterns
      val groups = groupPatterns.map { groupPattern =>
          val groupedElements = groupToNodes(groupPatterns, groupPattern)
          val groupedTraits = groupedElements.map(nameToPattern(nodePatterns ::: hyperRelationPatterns, _))
            .flatMap(_.superTypes).distinct
            .intersect(nodeTraitPatterns.map(_.name))
            .map(nameToPattern(nodeTraitPatterns, _))
          Group(groupPattern,
            nodes = groupedElements.map(nameToPattern(nodePatterns ::: hyperRelationPatterns, _)).collect { case n: NodePattern => n.name },
            nodesWithHyperNodes = groupedElements,
            relations = groupToRelations(groupPatterns, nodePatterns, hyperRelationPatterns, relationPatterns, groupPattern),
            relationsWithHyperRelations = groupToRelations(groupPatterns, nodePatterns, hyperRelationPatterns, allRelationPatterns, groupPattern),
            hyperRelations = groupToRelations(groupPatterns, nodePatterns, hyperRelationPatterns, hyperRelationPatterns, groupPattern),
            nodeTraits = groupedTraits.map(nodeTraitPattern =>
              NodeTrait(nodeTraitPattern, nodeTraitPatterns, relationTraitPatterns,
                selectedNodePatterns = groupedElements.filter(g => nodePatterns.exists(_.name == g)).map(nameToPattern(nodePatterns, _)),
                selectedHyperRelationPatterns = (nodeNamesToRelations(groupedElements, hyperRelationPatterns) ::: groupedElements.filter(g => hyperRelationPatterns.exists(_.name == g)).map(nameToPattern(hyperRelationPatterns, _))).distinct,
                relationPatterns, hyperRelationPatterns))
          )
        }
      val hyperRelations = hyperRelationPatterns.map(hyperRelationPattern => HyperRelation(hyperRelationPattern, filterSuperTypes(nodeTraitPatterns, hyperRelationPattern), filterSuperTypes(relationTraitPatterns, hyperRelationPattern), flatSuperStatements(nodeTraitPatterns ::: relationTraitPatterns, hyperRelationPattern), findSuperFactoryParameterList(nodeTraitPatterns ::: relationTraitPatterns, hyperRelationPattern, relationTraits)))
      val relations = relationPatterns.map(relationPattern => Relation(relationPattern, flatSuperStatements(relationTraitPatterns, relationPattern), findSuperFactoryParameterList(relationTraitPatterns, relationPattern, relationTraits)))
      Schema(schemaPattern, nodes, relations, hyperRelations, nodeTraits, relationTraits, groups, statements)
    }

    def findSuperFactoryParameterList[P <: NamePattern with SuperTypesPattern, Q <: Named with HasOwnFactory](patterns: List[_ <: P], pattern: P, nameClasses: List[Q]): Option[ParameterList] = patternToNameClasses(patternToFlatSuperTypes(patterns, pattern), nameClasses).find(_.hasOwnFactory).map(_.parameterList)

    def patternToNameClasses[P <: Named with HasOwnFactory](patterns: List[_ <: NamePattern], nameClasses: List[P]): List[P] = nameClasses.filter(nameClass => patterns.map(_.name).contains(nameClass.name))

    def filterSuperTypes(patterns: List[_ <: NamePattern], pattern: SuperTypesPattern): List[String] = {
      pattern.superTypes intersect patterns.map(_.name)
    }
    def flatSuperStatements[P <: NamePattern with SuperTypesPattern with StatementsPattern](superTypePatterns: List[NamePattern with SuperTypesPattern with StatementsPattern], pattern: P): List[Tree] = {
      val superTypes: List[StatementsPattern with NamePattern with SuperTypesPattern] = pattern.superTypes.map(superType => nameToPattern(superTypePatterns, superType))
      val flatSuperTypes: List[StatementsPattern] = pattern :: patternToFlatSuperTypes(superTypePatterns, pattern)
      flatSuperTypes.flatMap(_.statements)
    }
    val testNode = NodePattern("a", List("superx"), List(q"def title:String"))
    assertX(flatSuperStatements(
      List(
        NodeTraitPattern("superx", List("supery"), List(q"def titlex:String")),
        NodeTraitPattern("supery", Nil, List(q"def titley:String")),
        testNode
      ), testNode).map(_.toString).toSet,
      List(q"def titlex:String", q"def title:String", q"def titley:String").map(_.toString).toSet)
    def nameToPattern[P <: NamePattern](patterns: List[P], name: String): P = patterns.find(_.name == name).get
    def neighbours(nodePattern: NodePattern, relations: List[NamePattern with StartEndNodePattern], nodePatterns: List[NodePattern], nodeTraitPatterns: List[NodeTraitPattern]): List[(String, String, String)] = {
      val sources = (nodePattern :: patternToFlatSuperTypes(nodeTraitPatterns, nodePattern)).map(_.name)
      relations.filter(sources contains _.startNode).flatMap { r =>
        if(nodeTraitPatterns.map(_.name) contains r.endNode) {
          // if r.endNode is a trait
          // generate accessors for all childNodes
          val childNodes = childNodesOfNodeTrait(nodeTraitPatterns, nodePatterns, nameToPattern(nodeTraitPatterns, r.endNode))
          childNodes.map { childNode =>
            val accessorName = nameToPlural(r.name + childNode)
            (accessorName, r.name, childNode)
          }
        }
        else {
          val accessorName = nameToPlural(r.name)
          List((accessorName, r.name, r.endNode))
        }
      }
    }
    def rev_neighbours(nodePattern: NodePattern, relations: List[NamePattern with StartEndNodePattern], nodePatterns: List[NodePattern], nodeTraitPatterns: List[NodeTraitPattern]): List[(String, String, String)] = {
      val targets = (nodePattern :: patternToFlatSuperTypes(nodeTraitPatterns, nodePattern)).map(_.name)
      relations.filter(targets contains _.endNode).flatMap { r =>
        if(nodeTraitPatterns.map(_.name) contains r.startNode) {
          // if r.startNode is a trait
          val childNodes = childNodesOfNodeTrait(nodeTraitPatterns, nodePatterns, nameToPattern(nodeTraitPatterns, r.startNode))
          childNodes.map { childNode =>
            val accessorName = rev(nameToPlural(r.name + childNode))
            (accessorName, r.name, childNode)
          }
        }
        else {
          val accessorName = rev(nameToPlural(r.name))
          List((accessorName, r.name, r.startNode))
        }
      }
    }
    def isDeepSuperType[P <: NamePattern with SuperTypesPattern](patterns: List[P], subPattern: P, superPattern: P): Boolean = {
      // assert(subPattern.superTypes.forall(superType => patterns.map(_.name) contains superType), s"${ subPattern.superTypes } ## NOT IN ## ${ patterns.map(_.name) }")
      subPattern.superTypes match {
        case Nil        => false
        case superTypes => superTypes.exists { name =>
          superPattern.name == name || (patterns.exists(_.name == name) && isDeepSuperType(patterns, nameToPattern(patterns, name), superPattern))
        }
      }
    }
    assertX(isDeepSuperType(List(
      NodeTraitPattern("superx", List("supery"), Nil),
      NodeTraitPattern("supery", List("superz"), Nil),
      NodeTraitPattern("superz", Nil, Nil)
    ), subPattern = NodeTraitPattern("superx", List("supery"), Nil),
      superPattern = NodeTraitPattern("superz", Nil, Nil)), true)

    def patternToSuperTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = pattern.superTypes.map(nameToPattern(patterns, _))
    def patternToFlatSuperTypes[P <: NamePattern with SuperTypesPattern, SUPER <: P](patterns: List[SUPER], pattern: P): List[SUPER] = {
      patterns.filter { superPattern => isDeepSuperType(patterns, pattern, superPattern) }
    }
    def patternToSubTypes[P <: NamePattern with SuperTypesPattern](patterns: List[P], pattern: P): List[P] = patterns.filter(_.superTypes.contains(pattern.name))
    def patternToFlatSubTypes[P <: NamePattern with SuperTypesPattern, SUB <: P](patterns: List[SUB], pattern: P): List[SUB] = {
      patterns.filter { subPattern => isDeepSuperType(patterns, subPattern, pattern) }
    }

    // TODO: check usages if other callers also need intermediate traits
    def childNodesOfNodeTrait(nodeTraits: List[NodeTraitPattern], nodePatterns: List[NamePattern with SuperTypesPattern], nodeTrait: NodeTraitPattern): List[String] = {
      (nodeTrait :: patternToFlatSubTypes(nodeTraits, nodeTrait)).flatMap { subTrait =>
        nodePatterns.filter(_.superTypes contains subTrait.name)
      }.distinct.map(_.name)
    }
    assertX(childNodesOfNodeTrait(
      List(
        NodeTraitPattern("traitA", Nil, Nil),
        NodeTraitPattern("traitB", List("traitA"), Nil),
        NodeTraitPattern("traitC", Nil, Nil)
      ),
      List(
        NodePattern("nodeC", List("traitA"), Nil),
        NodePattern("nodeD", List("traitB"), Nil)
      ),
      NodeTraitPattern("traitA", Nil, Nil)
    ), List("nodeC", "nodeD"))

    def childNodesOfNodeTraitsWithTraits(nodeTraits: List[NodeTraitPattern], nodePatterns: List[NamePattern with SuperTypesPattern], nodeTrait: NodeTraitPattern): List[String] = {
      (nodeTrait :: patternToFlatSubTypes(nodeTraits, nodeTrait)).flatMap { subTrait =>
        subTrait :: nodePatterns.filter(_.superTypes contains subTrait.name)
      }.distinct.map(_.name)
    }

    def nodeNamesToRelations[R <: StartEndNodePattern](nodeNames: List[String], relations: List[R]): List[R] = {
      relations.filter(relation => nodeNames.contains(relation.startNode) && nodeNames.contains(relation.endNode))
    }

    def nodeTraitToCommonHyperNodeTraits[P <: NamePattern with SuperTypesPattern](nodeTraitPatterns: List[NodeTraitPattern], middleNodeTraitPatterns: List[P], nodePatterns: List[NodePattern], hyperRelationPatterns: List[HyperRelationPattern], nodeTrait: NodeTraitPattern): List[String] = {
      val nodes = childNodesOfNodeTraitsWithTraits(nodeTraitPatterns, nodePatterns ::: hyperRelationPatterns, nodeTrait)
      val subHyperRelations = nodeNamesToRelations(nodes, hyperRelationPatterns)
      val flatSuperTypes: List[List[String]] = subHyperRelations.map(hyperRelation => patternToFlatSuperTypes(middleNodeTraitPatterns, hyperRelation).map(_.name))
      if(flatSuperTypes.isEmpty) Nil
      else if(flatSuperTypes.size == 1) flatSuperTypes.head
      else flatSuperTypes.reduce(_ intersect _)
    }
    assertX(nodeTraitToCommonHyperNodeTraits(
      List(
        NodeTraitPattern("traitA", Nil, Nil),
        NodeTraitPattern("traitB", Nil, Nil),
        NodeTraitPattern("traitC", Nil, Nil)
      ),
      List(
        NodeTraitPattern("traitA", Nil, Nil),
        NodeTraitPattern("traitB", Nil, Nil),
        NodeTraitPattern("traitC", Nil, Nil)
      ),
      List(
        NodePattern("nodeC", List("traitA"), Nil),
        NodePattern("nodeD", List("traitA"), Nil)
      ),
      List(
        HyperRelationPattern("hyperX", "nodeC", "nodeD", List("traitB", "traitC"), Nil),
        HyperRelationPattern("hyperY", "nodeD", "nodeC", List("traitA", "traitB", "traitC"), Nil)
      ),
      NodeTraitPattern("traitA", Nil, Nil)
    ).toSet, List("traitB", "traitC").toSet)

    def groupToNodes(groupPatterns: List[GroupPattern], groupPattern: GroupPattern): List[String] = {
      (groupPattern :: patternToFlatSubTypes(groupPatterns, groupPattern)).flatMap(_.nodes)
    }
    assertX(groupToNodes(List(
      GroupPattern("groupA", Nil, List("nodeA", "nodeB")),
      GroupPattern("groupB", List("groupA"), List("nodeC", "nodeD"))
    ), GroupPattern("groupA", Nil, List("nodeA", "nodeB"))).toSet, List("nodeA", "nodeB", "nodeC", "nodeD").toSet)

    def groupToRelations(groupPatterns: List[GroupPattern], nodePatterns: List[NodePattern], hyperRelationPatterns: List[HyperRelationPattern], relations: List[NamePattern with StartEndNodePattern], groupPattern: GroupPattern): List[String] = {
      val nodes = groupToNodes(groupPatterns, groupPattern)
      val traits = nodes.map(nameToPattern(nodePatterns ::: hyperRelationPatterns, _)).flatMap(_.superTypes).distinct
      nodeNamesToRelations(nodes ::: traits ::: hyperRelationPatterns.map(_.name), relations).map(_.name)
    }

    def traitCanHaveOwnFactory(hierarchyPatterns: List[NamePattern with SuperTypesPattern with StatementsPattern], currentTrait: NamePattern with SuperTypesPattern): Boolean = {
      val children = patternToFlatSubTypes(hierarchyPatterns, currentTrait)
      // if we currently are at NodeTrait, we need to check whether one of its
      // children is a HyperRelation. If that is the case, a factory cannot be
      // generated, as the HyperRelation additionally needs Start-/EndNode in
      // its local method.
      val isNodeTrait = currentTrait.isInstanceOf[NodeTraitPattern]
      val hasHyperRelationChild = children.exists(_.isInstanceOf[HyperRelationPattern])
      if(isNodeTrait && hasHyperRelationChild)
        return false

      val statements = children.flatMap(_.statements)
      statements.find {
        case q"val $x:Option[$propertyType]" => false
        case q"var $x:Option[$propertyType]" => false
        case q"val $x:$propertyType = $y"    => false
        case q"var $x:$propertyType = $y"    => false
        case q"val $x:$propertyType"         => true
        case q"var $x:$propertyType"         => true
        case _                               => false
      }.isEmpty
    }
  }

  case class Schema(
                     pattern: SchemaPattern,
                     nodes: List[Node],
                     relations: List[Relation],
                     hyperRelations: List[HyperRelation],
                     nodeTraits: List[NodeTrait],
                     relationTraits: List[RelationTrait],
                     groups: List[Group],
                     statements: List[Tree]
                     ) extends Named with SuperTypes {
  }

  case class Group(
                    pattern: GroupPattern,
                    nodesWithHyperNodes: List[String],
                    nodes: List[String],
                    relationsWithHyperRelations: List[String],
                    relations: List[String],
                    hyperRelations: List[String],
                    nodeTraits: List[NodeTrait]
                    ) extends Named with SuperTypes


  case class NodeTrait(
                        pattern: NodeTraitPattern,
                        subNodes: List[String],
                        subRelations: List[String],
                        subHyperRelations: List[String],
                        commonHyperNodeNodeTraits: List[String],
                        commonHyperNodeRelationTraits: List[String],
                        flatStatements: List[Tree],
                        hasOwnFactory: Boolean
                        ) extends Named with SuperTypes with Statements with HasOwnFactory {
    if(pattern.superTypes.size > 1)
      context.abort(NoPosition, "Currently NodeTraits are restricted to only extend one trait")

    def commonHyperNodeNodeTraits_type = commonHyperNodeNodeTraits.map(TypeName(_))
    def commonHyperNodeRelationTraits_type = commonHyperNodeRelationTraits.map(TypeName(_))

    val parameterList = ParameterList.create(flatStatements)
  }

  object NodeTrait {
    def apply(
               nodeTraitPattern: NodeTraitPattern,
               nodeTraitPatterns: List[NodeTraitPattern],
               relationTraitPatterns: List[RelationTraitPattern],
               selectedNodePatterns: List[NodePattern],
               selectedHyperRelationPatterns: List[HyperRelationPattern],
               relationPatterns: List[RelationPattern],
               hyperRelationPatterns: List[HyperRelationPattern]
               ) = {
      import Schema.{childNodesOfNodeTrait, nodeNamesToRelations, nodeTraitToCommonHyperNodeTraits, flatSuperStatements, traitCanHaveOwnFactory}
      val childNodes = childNodesOfNodeTrait(nodeTraitPatterns, selectedNodePatterns ::: selectedHyperRelationPatterns, nodeTraitPattern)
      val childTraits = childNodesOfNodeTrait(nodeTraitPatterns, nodeTraitPatterns, nodeTraitPattern)
      new NodeTrait(
        nodeTraitPattern,
        subNodes = childNodes,
        subRelations = nodeNamesToRelations(nodeTraitPattern.name :: childNodes ::: childTraits, hyperRelationPatterns ::: relationPatterns).map(_.name),
        subHyperRelations = nodeNamesToRelations(nodeTraitPattern.name :: childNodes ::: childTraits, hyperRelationPatterns).map(_.name),
        commonHyperNodeNodeTraits = nodeTraitToCommonHyperNodeTraits(nodeTraitPatterns, nodeTraitPatterns, selectedNodePatterns, hyperRelationPatterns, nodeTraitPattern),
        commonHyperNodeRelationTraits = nodeTraitToCommonHyperNodeTraits(nodeTraitPatterns, relationTraitPatterns, selectedNodePatterns, hyperRelationPatterns, nodeTraitPattern),
        flatStatements = flatSuperStatements(nodeTraitPatterns, nodeTraitPattern),
        hasOwnFactory = traitCanHaveOwnFactory(selectedNodePatterns ::: hyperRelationPatterns ::: relationTraitPatterns ::: nodeTraitPatterns, nodeTraitPattern)
      )
    }
  }

  case class RelationTrait(
                            pattern: RelationTraitPattern,
                            flatStatements: List[Tree],
                            hasOwnFactory: Boolean
                            ) extends Named with SuperTypes with Statements with HasOwnFactory {
    if(pattern.superTypes.size > 1)
      context.abort(NoPosition, "Currently RelationTraits are restricted to only extend one trait")

    val parameterList = ParameterList.create(flatStatements)
  }

  case class Node(
                   pattern: NodePattern,
                   override val superTypes: List[String], // only nodeTraits
                   otherSuperTypes: List[String],
                   neighbours: List[(String, String, String)], // accessorName, relation, endNode
                   rev_neighbours: List[(String, String, String)], // accessorName, relation, startNode
                   outRelationsToTrait: List[(String, String)],
                   inRelationsFromTrait: List[(String, String)],
                   flatStatements: List[Tree],
                   traitFactoryParameterList: Option[ParameterList]
                   ) extends Named with SuperTypes with Statements {
    if(superTypes.size > 1)
      context.abort(NoPosition, "Currently Nodes are restricted to only extend one trait")

    val parameterList = ParameterList.create(flatStatements)
    def neighbours_terms = neighbours.map { case (accessorName, relation, endNode) =>
      (TermName(accessorName), TermName(relation), TypeName(endNode), TermName(endNode))
    }
    def rev_neighbours_terms = rev_neighbours.map { case (rev_accessorName, relation, startNode) =>
      (TermName(rev_accessorName), TermName(relation), TypeName(startNode), TermName(startNode))
    }
  }

  case class Relation(
                       pattern: RelationPattern,
                       flatStatements: List[Tree], // TODO: rename to flatSuperStatements (same for node etc)
                       traitFactoryParameterList: Option[ParameterList]
                       ) extends Named with StartEndNode with SuperTypes with Statements {
    if(superTypes.size > 1)
      context.abort(NoPosition, "Currently Relations are restricted to only extend one trait")

    val parameterList = ParameterList.create(flatStatements)
  }

  case class HyperRelation(
                            pattern: HyperRelationPattern,
                            superNodeTypes: List[String],
                            superRelationTypes: List[String],
                            flatSuperStatements: List[Tree],
                            traitFactoryParameterList: Option[ParameterList]
                            ) extends Named with SuperTypes with StartEndNode with Statements with StartEndRelation {
    if(superNodeTypes.size > 1 || superRelationTypes.size > 1)
      context.abort(NoPosition, "Currently HyperRelations are restricted to only extend one trait")

    val parameterList = ParameterList.create(flatSuperStatements)
  }

  case class Parameter(name: Tree, typeName: Tree, optional: Boolean, default: Option[Tree], mutable: Boolean) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Parameter]
    override def equals(other: Any): Boolean = other match {
      case that: Parameter =>
        (that canEqual this) &&
          this.name.toString == that.name.toString &&
          this.typeName.toString == that.typeName.toString &&
          this.optional == that.optional &&
          this.default.toString == that.default.toString &&
          this.mutable == that.mutable
      case _               => false
    }
    override def hashCode: Int = List(this.name.toString, this.typeName.toString, this.optional, this.default.toString, this.mutable).hashCode
    def toParamCode: Tree = this match {
      case Parameter(propertyName, propertyType, _, None, _)               => q"val ${ TermName(propertyName.toString) }:${ propertyType }"
      case Parameter(propertyName, propertyType, _, Some(defaultValue), _) => q"val ${ TermName(propertyName.toString) }:${ propertyType } = ${ defaultValue }"
    }
    def toAssignmentCode(schemaItem: Tree): Tree = this match {
      case Parameter(propertyName, propertyType, false, _, _) => q"$schemaItem.properties(${ propertyName.toString }) = $propertyName"
      case Parameter(propertyName, propertyType, true, _, _)  => q"if($propertyName.isDefined) $schemaItem.properties(${ propertyName.toString }) = $propertyName.get"
    }
  }

  case class ParameterList(parameters: List[Parameter]) {

    val (withDefault, nonDefault) = parameters.sortBy(_.name.toString).partition(_.default.isDefined)
    val (withDefaultOptional, withDefaultNonOptional) = withDefault.partition(_.optional)
    val ordered = nonDefault ::: withDefaultNonOptional ::: withDefaultOptional
    def toParamCode: List[Tree] = ordered.map(_.toParamCode)
    def toAssignmentCode(schemaItem: Tree): List[Tree] = ordered.map(_.toAssignmentCode(schemaItem))
    def supplementMissingParametersOf(that: ParameterList): List[Tree] = {
      this.ordered.map(p => (p, that.ordered.find(_.name.toString == p.name.toString))).map {
        case (_, Some(other))    => other.name
        case (mine, None)        => mine.default.get // we know that we only handle a default param at this point (put into typesystem?)
      }
    }
  }

  object ParameterList {
    def create(flatStatements: List[Tree]): ParameterList = new ParameterList(flatStatements.collect {
      case statement@(q"val $propertyName:Option[$propertyType] = $default") => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = false)
      case statement@(q"var $propertyName:Option[$propertyType] = $default") => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = true)
      case statement@(q"val $propertyName:Option[$propertyType]")            => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = false)
      case statement@(q"var $propertyName:Option[$propertyType]")            => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = true)
      case statement@(q"val $propertyName:$propertyType = $default")         => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = false)
      case statement@(q"var $propertyName:$propertyType = $default")         => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = true)
      case statement@(q"val $propertyName:$propertyType")                    => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = false)
      case statement@(q"var $propertyName:$propertyType")                    => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = true)
    })
  }

}
