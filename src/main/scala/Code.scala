package renesca.schema.macros

trait Code extends Context with Generators {

  import context.universe._
  import Helpers._

  def relationStart(schema: Schema, name: String): String = schema.relations.find(_.name == name).get.startNode
  def relationEnd(schema: Schema, name: String): String = schema.relations.find(_.name == name).get.endNode

  def propertyGetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name) }:$typeName = node.properties(${ name }).asInstanceOf[${ TypeName(typeName.toString + "PropertyValue") }] """
  def propertyOptionGetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name) }:Option[$typeName] = node.properties.get(${ name }).asInstanceOf[Option[${ TypeName(typeName.toString + "PropertyValue") }]].map(propertyValueToPrimitive) """
  def propertySetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name + "_$eq") }(newValue:$typeName){ node.properties(${ name }) = newValue} """
  def propertyOptionSetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name + "_$eq") }(newValue:Option[$typeName]){ if(newValue.isDefined) node.properties(${ name }) = newValue.get else node.properties -= ${ name } }"""
  def generatePropertyAccessors(statement: Tree): List[Tree] = statement match {
    case q"val $propertyName:Option[$propertyType]"      => List(propertyOptionGetter(propertyName.toString, propertyType))
    case q"var $propertyName:Option[$propertyType]"      => List(propertyOptionGetter(propertyName.toString, propertyType), propertyOptionSetter(propertyName.toString, propertyType))
    case q"val $propertyName:Option[$propertyType] = $x" => List(propertyOptionGetter(propertyName.toString, propertyType))
    case q"var $propertyName:Option[$propertyType] = $x" => List(propertyOptionGetter(propertyName.toString, propertyType), propertyOptionSetter(propertyName.toString, propertyType))
    case q"val $propertyName:$propertyType"              => List(propertyGetter(propertyName.toString, propertyType))
    case q"var $propertyName:$propertyType"              => List(propertyGetter(propertyName.toString, propertyType), propertySetter(propertyName.toString, propertyType))
    case q"val $propertyName:$propertyType = $x"         => List(propertyGetter(propertyName.toString, propertyType))
    case q"var $propertyName:$propertyType = $y"         => List(propertyGetter(propertyName.toString, propertyType), propertySetter(propertyName.toString, propertyType))
    case somethingElse                                   => List(somethingElse)
  }

  def generateIndirectNeighbourAccessors(schema: Schema, statement: Tree): Tree = statement match {
    case q"""def $chainName = $rel1 --> $rel2""" =>
      q"""def $chainName:Set[${ TypeName(relationEnd(schema, rel2.toString)) }] = ${ TermName(nameToPlural(rel1.toString)) }.flatMap(_.${ TermName(nameToPlural(rel2.toString)) })"""
    case q"""def $chainName = $rel1 <-- $rel2""" =>
      q"""def $chainName:Set[${ TypeName(relationStart(schema, rel1.toString)) }] = ${ TermName(rev(nameToPlural(rel2.toString))) }.flatMap(_.${ TermName(rev(nameToPlural(rel1.toString))) })"""
    case otherStatement                          => otherStatement
  }

  def nodeTraitFactories(schema: Schema): List[Tree] = schema.nodeTraits.map { nodeTrait => import nodeTrait._
    val factoryName = TypeName(traitFactoryName(name))
    val localInterface = if(hasOwnFactory) q"def local (...${ parameterList.toParamCode }): NODE" else q""
    val superTypeFactories = superTypes.map(traitFactoryName).map(TypeName(_)).map(fact => tq"$fact[NODE]")
    q"""
           trait $factoryName[NODE <: Node] extends NodeFactory[NODE] with ..$superTypeFactories {
            $localInterface
           }
           """
  }

  def relationTraitFactories(schema: Schema): List[Tree] = schema.relationTraits.map { relationTrait => import relationTrait._
    val startEndlocalParams = List(List(q"val startNode:START", q"val endNode:END") ::: parameterList.toParamCode.head)
    val factoryName = TypeName(traitFactoryName(name))
    val localInterface = if(hasOwnFactory) q" def local (...$startEndlocalParams): RELATION " else q""
    val superTypeFactories = superTypes.map(traitFactoryName).map(TypeName(_)).map(fact => tq"$fact[START,RELATION,END]")
    q"""
           trait $factoryName[START <: Node, +RELATION <: AbstractRelation[START,END], END <: Node] extends AbstractRelationFactory[START,RELATION,END]
            with ..$superTypeFactories {
            $localInterface
           }
           """
  }

  def forwardLocalMethod(parameterList: ParameterList, traitFactoryParameterList: Option[ParameterList], typeName: Tree) = {
    if(traitFactoryParameterList.isDefined
      && parameterList.parameters.size > traitFactoryParameterList.get.parameters.size) {
      val parentParameterList = traitFactoryParameterList.get
      val parentCaller = parameterList.supplementMissingParametersOf(parentParameterList)
      q""" def local (...${ parentParameterList.toParamCode }): $typeName = local(..$parentCaller) """
    } else
        q""
  }

  // TODO: duplicate code
  def forwardLocalMethodStartEnd(parameterList: ParameterList, traitFactoryParameterList: Option[ParameterList], typeName: Tree, startNodeType: Tree, endNodeType: Tree) = {
    if(traitFactoryParameterList.isDefined
      && parameterList.parameters.size > traitFactoryParameterList.get.parameters.size) {
      val parentParameterList = traitFactoryParameterList.get
      val parentCaller = parameterList.supplementMissingParametersOf(parentParameterList)
      val localParameters: List[List[Tree]] = List(List(q"val startNode:$startNodeType", q"val endNode:$endNodeType") ::: parentParameterList.toParamCode.head)
      q""" def local (...$localParameters): $typeName = local(..${ List(q"startNode", q"endNode") ::: parentCaller }) """
    } else
        q""
  }

  //TODO: what happens with name clashes?
  // @Node trait traitA { val name: String }; @Node trait traitB extends traitA { val name: String }
  def nodeFactories(schema: Schema): List[Tree] = schema.nodes.map { node => import node._
    val superFactory = TypeName(superTypes.headOption match {
      case Some(superType) => s"${ traitFactoryName(superType) }"
      case None            => s"NodeFactory"
    })

    // Extending superFactory is enough, because NodeFactory is pulled in every case.
    // This works because NodeFactory does not get any generics.
    q"""
           object $name_term extends $superFactory[$name_type] {
             def wrap(node: raw.Node) = new $name_type(node)
             val label = raw.Label($name_label)
             def local (...${ parameterList.toParamCode }):$name_type = {
              val node = wrap(raw.Node.local(List(label)))
              ..${ parameterList.toAssignmentCode(q"node.node") }
              node
             }
             ${ forwardLocalMethod(parameterList, traitFactoryParameterList, tq"$name_type") }
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
    val superNodeTraitTypesWithDefault = if(superTypes.isEmpty) List(TypeName("Node")) else superTypes_type
    val otherSuperTypes_type = otherSuperTypes.map(TypeName(_))

    q"""
    case class $name_type(node: raw.Node) extends ..$superNodeTraitTypesWithDefault with ..$otherSuperTypes_type {
        ..$directNeighbours
        ..$directRevNeighbours
        ..$nodeBody
      }
    """
  }

  def relationFactories(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
    val superRelationFactory = TypeName(superTypes.headOption match {
      case Some(superType) => s"${ traitFactoryName(superType) }"
      case None            => s"AbstractRelationFactory"
    })
    q"""
           object $name_term extends RelationFactory[$startNode_type, $name_type, $endNode_type]
            with $superRelationFactory[$startNode_type, $name_type, $endNode_type] {
               def startNodeFactory = $startNode_term
               def endNodeFactory = $endNode_term
               def relationType = raw.RelationType($name_label)
               def wrap(relation: raw.Relation) = $name_term(
                 $startNode_term.wrap(relation.startNode),
                 relation,
                 $endNode_term.wrap(relation.endNode))
              def local (...${ List(List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: parameterList.toParamCode.head) }):$name_type = {
                val relation = wrap(raw.Relation.local(startNode.node, relationType, endNode.node))
                ..${ parameterList.toAssignmentCode(q"relation.relation") }
                relation
              }
              ${ forwardLocalMethod(parameterList, traitFactoryParameterList, tq"$name_type") }
           }
           """
  }

  def relationClasses(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
    val superTypesWithDefault = "Relation" :: superTypes
    val superTypesWithDefaultGenerics = superTypesWithDefault.map(TypeName(_)).map(superType => tq"$superType[$startNode_type,$endNode_type]")
    q"""
           case class $name_type(startNode: $startNode_type, relation: raw.Relation, endNode: $endNode_type)
             extends ..$superTypesWithDefaultGenerics {
             ..$statements
           }
           """
  }


  def hyperRelationFactories(schema: Schema): List[Tree] = schema.hyperRelations.map { hyperRelation => import hyperRelation._
    val superRelationFactory = TypeName(superRelationTypes.headOption match {
      case Some(superType) => s"${ traitFactoryName(superType) }"
      case None            => s"AbstractRelationFactory"
    })
    q"""
           object $name_term extends HyperRelationFactory[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type]
             with $superRelationFactory[$startNode_type, $name_type, $endNode_type] {

             override def label = raw.Label($name_label)
             override def startRelationType = raw.RelationType($startRelation_label)
             override def endRelationType = raw.RelationType($endRelation_label)

             override def startNodeFactory = $startNode_term
             override def factory = $name_term
             override def endNodeFactory = $endNode_term

             override def wrap(node: raw.Node) = new $name_type(node)
             override def startRelationWrap(relation: raw.Relation) = $startRelation_term(startNodeFactory.wrap(relation.startNode), relation, factory.wrap(relation.endNode))
             override def endRelationWrap(relation: raw.Relation) = $endRelation_term(factory.wrap(relation.startNode), relation, endNodeFactory.wrap(relation.endNode))

             def local (...${ List(List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: parameterList.toParamCode.head) }):$name_type = {
              val middleNode = wrap(raw.Node.local(List(label)))
              ..${ parameterList.toAssignmentCode(q"middleNode.node") }
              wrap(startRelationLocal(startNode, middleNode).relation, middleNode.node, endRelationLocal(middleNode, endNode).relation)
             }
             ${ forwardLocalMethodStartEnd(parameterList, traitFactoryParameterList, tq"$name_type", tq"$startNode_type", tq"$endNode_type") }
           }
           """
  }

  def hyperRelationClasses(schema: Schema): List[Tree] = schema.hyperRelations.map { hyperRelation => import hyperRelation._
    //TODO: generate indirect neighbour-accessors based on hyperrelations
    //TODO: property accessors
    val superRelationTypesGenerics = superRelationTypes.map(TypeName(_)).map(superType => tq"$superType[$startNode_type,$endNode_type]")
    List( q"""
           case class $name_type(node: raw.Node)
              extends HyperRelation[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type]
              with ..${ superRelationTypesGenerics ::: superNodeTypes.map(t => tq"${ TypeName(t) }") } {
             ..$statements
           }
           """, q"""
           case class $startRelation_type(startNode: $startNode_type, relation: raw.Relation, endNode: $name_type)
             extends Relation[$startNode_type, $name_type]
           """, q"""
           case class $endRelation_type(startNode: $name_type, relation: raw.Relation, endNode: $endNode_type)
             extends Relation[$name_type, $endNode_type]
           """)
  }.flatten

  def nodeSuperTraits(schema: Schema): List[Tree] = schema.nodeTraits.map { nodeTrait => import nodeTrait._
    val superTypesWithDefault = (if(superTypes.isEmpty) List("Node") else superTypes).map(TypeName(_))
    val traitBody = statements.flatMap(generatePropertyAccessors(_))
    q""" trait $name_type extends ..$superTypesWithDefault { ..$traitBody } """
  }

  def relationSuperTraits(schema: Schema): List[Tree] = schema.relationTraits.map { relationTrait => import relationTrait._
    val superTypesWithDefault = if(superTypes.isEmpty) List("AbstractRelation") else superTypes
    val superTypesWithDefaultGenerics = superTypesWithDefault.map(TypeName(_)).map(superType => tq"$superType[START,END]")
    val traitBody = statements.flatMap(generatePropertyAccessors(_))
    q""" trait $name_type[+START <: Node,+END <: Node] extends ..$superTypesWithDefaultGenerics { ..$traitBody } """
  }

  def groupFactories(schema: Schema): List[Tree] = schema.groups.map { group => import group._
    q""" object $name_term {def empty = new $name_type(raw.Graph.empty) } """
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
      q"def ${ TermName(nameToPlural(name + "Relation")) }:Set[_ <: Relation[$name_type, $name_type]] = ${ allOf(subRelations) }"
    }
    val hyperRelationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
      val hyperNodeRelationTraits_type = commonHyperNodeRelationTraits_type.map(t => tq"$t[$name_type, $name_type]")
      q"""def ${ TermName(nameToPlural(name + "HyperRelation")) } :Set[HyperRelation[
                  $name_type,
                  _ <: Relation[$name_type, _],
                  _ <: HyperRelation[$name_type, _, _, _, $name_type] with ..$commonHyperNodeNodeTraits_type with ..$hyperNodeRelationTraits_type,
                  _ <: Relation[_, $name_type],
                  $name_type]
                  with ..$commonHyperNodeNodeTraits_type with ..$hyperNodeRelationTraits_type]
              = ${ allOf(subHyperRelations) }"""
    }

    q"""
           case class $name_type(graph: raw.Graph) extends Graph {
             ..$nodeSets
             ..$relationSets
             ..$hyperRelationSets

             ..$nodeTraitSets
             ..$relationTraitSets
             ..$hyperRelationTraitSets

             def nodes: Set[Node] = ${ allOf(nodes) }
             def relations: Set[_ <: Relation[_,_]] = ${ allOf(relations) }
             def hyperRelations: Set[_ <: HyperRelation[_,_,_,_,_]] = ${ allOf(hyperRelations) }
           }
           """
  }

  def otherStatements(schema: Schema): List[Tree] = schema.statements.filterNot { statement =>
    NodePattern.unapply(statement).isDefined ||
      RelationPattern.unapply(statement).isDefined ||
      HyperRelationPattern.unapply(statement).isDefined ||
      NodeTraitPattern.unapply(statement).isDefined ||
      RelationTraitPattern.unapply(statement).isDefined ||
      GroupPattern.unapply(statement).isDefined
  }


  def schema(schema: Schema): Tree = {
    import schema.{name_term, superTypes_type}
    q"""
           object $name_term extends ..$superTypes_type {
             import renesca.{graph => raw}
             import renesca.schema._
             import renesca.parameter.StringPropertyValue
             import renesca.parameter.implicits._

             ..${ nodeTraitFactories(schema) }
             ..${ nodeSuperTraits(schema) }

             ..${ nodeFactories(schema) }
             ..${ nodeClasses(schema) }

             ..${ relationTraitFactories(schema) }
             ..${ relationSuperTraits(schema) }

             ..${ relationFactories(schema) }
             ..${ relationClasses(schema) }

             ..${ hyperRelationFactories(schema) }
             ..${ hyperRelationClasses(schema) }

             ..${ groupFactories(schema) }
             ..${ groupClasses(schema) }

             ..${ otherStatements(schema) }
           }
           """
  }
}
