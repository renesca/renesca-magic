package renesca.schema.macros

trait Code extends Context with Generators {

  import Helpers._
  import context.universe._

  def propertyGetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name) }:$typeName = rawItem.properties(${ name }).asInstanceOf[${ TypeName(typeName.toString + "PropertyValue") }] """

  def propertyOptionGetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name) }:Option[$typeName] = rawItem.properties.get(${ name }).asInstanceOf[Option[${ TypeName(typeName.toString + "PropertyValue") }]].map(propertyValueToPrimitive) """

  def propertySetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name + "_$eq") }(newValue:$typeName){ rawItem.properties(${ name }) = newValue} """

  def propertyOptionSetter(name: String, typeName: Tree) =
    q""" def ${ TermName(name + "_$eq") }(newValue:Option[$typeName]){ if(newValue.isDefined) rawItem.properties(${ name }) = newValue.get else rawItem.properties -= ${ name } }"""

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

  def matchesFactoryMethodsInterface(parameterList: ParameterList): List[Tree] = {
    val typeName = tq"NODE"
    val optionalParameterList = parameterList.optional
    val optionalFactoryParameters = optionalParameterList.toParamCode

    List(
      q"""
              def ${ TermName(factoryMatchesMethod(parameterList.typeName)) } (..$optionalFactoryParameters, matches: Set[PropertyKey] = Set.empty): $typeName
              """
    )
  }

  def factoryMethodsInterface(parameterList: ParameterList): List[Tree] = {
    if(parameterList.hasOwnFactory.isDefined && parameterList.hasOwnFactory.get) {
      val typeName = tq"NODE"
      val factoryParameters = parameterList.toParamCode

      List(
        q"""
                def ${ TermName(factoryCreateMethod(parameterList.typeName)) } (..$factoryParameters): $typeName
                """,
        q"""
                def ${ TermName(factoryMergeMethod(parameterList.typeName)) } (..$factoryParameters, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): $typeName
                """
      )
    } else {
      List.empty
    }
  }

  def factoryMethodsInterfaceStartEnd(parameterList: ParameterList): List[Tree] = {
    val typeName = tq"RELATION"
    val optionalParameterList = parameterList.optional
    val optionalFactoryParameters = optionalParameterList.toParamCode

    parameterList.hasOwnFactory.map(ownFactory => {
      val optionalFactoryParametersStartEnd = List(q"val startNode:START", q"val endNode:END") ::: optionalFactoryParameters

      val matches =
        q"""
              def ${ TermName(factoryMatchesMethod(parameterList.typeName)) } (..$optionalFactoryParametersStartEnd, matches: Set[PropertyKey] = Set.empty): $typeName
              """

      val factories = if(ownFactory) {
        val factoryParameters = List(q"val startNode:START", q"val endNode:END") ::: parameterList.toParamCode

        List(
          q"""
                def ${ TermName(factoryCreateMethod(parameterList.typeName)) } (..$factoryParameters): $typeName
                """,
          q"""
                def ${ TermName(factoryMergeMethod(parameterList.typeName)) } (..$factoryParameters, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): $typeName
                """
        )
      } else {
        List.empty
      }

      matches :: factories
    }).getOrElse(List(
      //TODO: test!
      // hyperrelation extends nodetrait and therefore needs to implement a matches method without start-/endnode parameters
      q"""
              def ${ TermName(factoryMatchesMethod(parameterList.typeName)) } (..$optionalFactoryParameters, matches: Set[PropertyKey] = Set.empty): $typeName
              """
    ))
  }

  def forwardMatchesFactoryMethods(parameterList: ParameterList, traitFactoryParameterList: List[ParameterList], typeName: Tree): List[Tree] = {
    traitFactoryParameterList.map(parentParameterList => {
      val optionalParameterList = parameterList.optional
      val optionalParentParameterList = parentParameterList.optional
      val optionalParentCaller = optionalParameterList.supplementMissingParametersOf(optionalParentParameterList)
      val optionalFactoryParameters = optionalParentParameterList.toParamCode

      q"""
              def ${ TermName(factoryMatchesMethod(parentParameterList.typeName)) } (..$optionalFactoryParameters, matches: Set[PropertyKey] = Set.empty): $typeName = this.matches (..$optionalParentCaller, matches)
              """
    })
  }

  def forwardFactoryMethods(parameterList: ParameterList, traitFactoryParameterList: List[ParameterList], typeName: Tree): List[Tree] = {
    traitFactoryParameterList.flatMap(parentParameterList => {
      val optionalParameterList = parameterList.optional
      val optionalParentParameterList = parentParameterList.optional
      val optionalParentCaller = optionalParameterList.supplementMissingParametersOf(optionalParentParameterList)
      val optionalFactoryParameters = optionalParentParameterList.toParamCode

      if(parentParameterList.hasOwnFactory.isDefined && parentParameterList.hasOwnFactory.get) {
        val parentCaller = parameterList.supplementMissingParametersOf(parentParameterList)
        val factoryParameters = parentParameterList.toParamCode

        List(
          q"""
                  def ${ TermName(factoryCreateMethod(parentParameterList.typeName)) } (..$factoryParameters): $typeName = this.create (..$parentCaller)
                  """,
          q"""
                  def ${ TermName(factoryMergeMethod(parentParameterList.typeName)) } (..$factoryParameters, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): $typeName = this.merge (..$parentCaller, merge, onMatch)
                  """
        )
      } else {
        List.empty
      }
    })
  }

  def forwardFactoryMethodsStartEnd(parameterList: ParameterList, traitFactoryParameterList: List[ParameterList], typeName: Tree, startNodeType: Tree, endNodeType: Tree): List[Tree] = {
    traitFactoryParameterList.flatMap(parentParameterList => {
      val optionalParameterList = parameterList.optional
      val optionalParentParameterList = parentParameterList.optional
      val optionalParentCaller = optionalParameterList.supplementMissingParametersOf(optionalParentParameterList)
      val optionalFactoryParameters = optionalParentParameterList.toParamCode

      parentParameterList.hasOwnFactory.map(ownFactory => {
        val optionalFactoryParametersStartEnd = List(q"val startNode:$startNodeType", q"val endNode:$endNodeType") ::: optionalFactoryParameters
        val optionalParentCallerStartEnd = List(q"startNode", q"endNode") ::: optionalParentCaller

        val matches =
          q"""
                def ${ TermName(factoryMatchesMethod(parentParameterList.typeName)) } (..$optionalFactoryParametersStartEnd, matches: Set[PropertyKey] = Set.empty): $typeName = this.matches (..$optionalParentCallerStartEnd, matches)
                """

        val factories = if(ownFactory) {
          val parentCaller = List(q"startNode", q"endNode") ::: parameterList.supplementMissingParametersOf(parentParameterList)
          val factoryParameters = List(q"val startNode:$startNodeType", q"val endNode:$endNodeType") ::: parentParameterList.toParamCode

          List(
            q"""
                  def ${ TermName(factoryCreateMethod(parentParameterList.typeName)) } (..$factoryParameters): $typeName = this.create (..$parentCaller)
                  """,
            q"""
                  def ${ TermName(factoryMergeMethod(parentParameterList.typeName)) } (..$factoryParameters, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): $typeName = this.merge (..$parentCaller, merge, onMatch)
                  """
          )
        } else {
          List.empty
        }

        matches :: factories
      }).getOrElse(List(
        //TODO: test!
        q"""
                def ${ TermName(factoryMatchesMethod(parentParameterList.typeName)) } (..$optionalFactoryParameters, matches: Set[PropertyKey] = Set.empty): $typeName = this.matchesNode (..$optionalParentCaller, matches)
                """
      ))
    })
  }

  def accumulatedTraitNeighbours(r: String, neighbours: List[(String, String, String)], relationPlural: TermName, nodeTrait: String): Tree = {
    val traitName = TypeName(nodeTrait)
    val successors = neighbours.collect { case (accessorName, `r`, _) => accessorName }.map(s => q"${ TermName(s) }")
    val combined = (q"Set.empty" :: successors).reduce[Tree]((a, b) => q"$a ++ $b")
    q""" def $relationPlural:Set[$traitName] = $combined"""
  }

  def nodeTraitFactories(schema: Schema): List[Tree] = schema.nodeTraits.flatMap { nodeTrait => import nodeTrait._
    val factoryName = TypeName(traitFactoryName(name))
    val matchesFactoryName = TypeName(traitMatchesFactoryName(name))
    val matchesClassTerm = TermName(traitMatchesClassName(name))
    val matchesClassType = TypeName(traitMatchesClassName(name))
    val superFactories = if(superTypes.isEmpty) List(tq"NodeFactory[NODE]") else superTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[NODE]")
    val superMatchesFactories = if(superTypes.isEmpty) List(tq"NodeFactory[NODE]") else superTypes.map(t => tq"${ TypeName(traitMatchesFactoryName(t)) }[NODE]")
    val factoryInterface = factoryMethodsInterface(parameterList)
    val matchesFactoryInterface = matchesFactoryMethodsInterface(parameterList)
    val forwardMatchesFactories = forwardMatchesFactoryMethods(parameterList, parameterList :: traitFactoryParameterList, tq"$matchesClassType")
    val optionalParameterList = parameterList.optional

    List(
      q"""
            trait $matchesFactoryName [+NODE <: $name_type] extends ..$superMatchesFactories {

              ..$matchesFactoryInterface
            }
            """,
      q"""
            trait $factoryName [+NODE <: $name_type] extends ..$superFactories with $matchesFactoryName[NODE] {

              ..$factoryInterface
            }
            """,
      q"""
            object $name_term extends RootNodeTraitFactory[$name_type] with $matchesFactoryName[$name_type] {
              val label = $matchesClassTerm.label
              val labels = $matchesClassTerm.labels

              def matches(..${ optionalParameterList.toParamCode }, matches: Set[PropertyKey] = Set.empty):$matchesClassType = $matchesClassTerm.matches(..${ optionalParameterList.toCallerCode }, matches)

              ..$forwardMatchesFactories
            }
            """
    )
  }

  def relationTraitFactories(schema: Schema): List[Tree] = schema.relationTraits.map { relationTrait => import relationTrait._
    val factoryName = TypeName(traitFactoryName(name))
    val superFactories = if(superTypes.isEmpty) List(tq"AbstractRelationFactory[START,RELATION,END]") else superTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[START,RELATION,END]")
    val factoryInterface = factoryMethodsInterfaceStartEnd(parameterList)

    q"""
          trait $factoryName [START <: Node, +RELATION <: AbstractRelation[START,END], END <: Node] extends ..$superFactories {

            ..$factoryInterface
          }
         """
  }

  //TODO: what happens with name clashes?
  // @Node trait traitA { val name: String }; @Node trait traitB extends traitA { val name: String }
  def nodeFactories(schema: Schema): List[Tree] = schema.nodes.map { node => import node._
    val forwardMatchesFactories = forwardMatchesFactoryMethods(parameterList, traitFactoryParameterList, tq"$name_type")
    val optionalParameterList = parameterList.optional

    val commonCode =
      q"""
             def wrap(node: raw.Node) = new $name_type(node)

             def matches (..${ optionalParameterList.toParamCode }, matches: Set[PropertyKey] = Set.empty):$name_type = {
              val wrapped = wrap(raw.Node.matches(labels, matches = matches))
              ..${ optionalParameterList.toAssignmentCode(q"wrapped.rawItem") }
              wrapped
             }

             ..$forwardMatchesFactories
             """

    implementedTrait.map(implTrait => {
      val superMatchesFactories = if(superTypes.isEmpty) List(tq"NodeFactory[$name_type]") else superTypes.map(t => tq"${ TypeName(traitMatchesFactoryName(t)) }[$name_type]")
      val labels = implTrait.flatSuperTypesWithSelf.map(nameToLabel(_)).map(l => q"raw.Label($l)")
      val label = implTrait.name_label

      q"""
             object $name_term extends ..$superMatchesFactories {
               val label = raw.Label($label)
               val labels = Set(..$labels)

               ..$commonCode
             }
             """
    }).getOrElse {
      val superFactories = if(superTypes.isEmpty) List(tq"NodeFactory[$name_type]") else superTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[$name_type]")
      val forwardFactories = forwardFactoryMethods(parameterList, traitFactoryParameterList, tq"$name_type")
      val labels = flatSuperTypesWithSelf.map(nameToLabel(_)).map(l => q"raw.Label($l)")

      // Extending superFactory is enough, because NodeFactory is pulled in every case.
      // This works because NodeFactory does not get any generics.
      q"""
             object $name_term extends ..$superFactories {
               val label = raw.Label($name_label)
               val labels = Set(..$labels)

               ..$commonCode

               def create (..${ parameterList.toParamCode }):$name_type = {
                val wrapped = wrap(raw.Node.create(labels))
                ..${ parameterList.toAssignmentCode(q"wrapped.rawItem") }
                wrapped
               }

               def merge (..${ parameterList.toParamCode }, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty):$name_type = {
                val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch))
                ..${ parameterList.toAssignmentCode(q"wrapped.rawItem") }
                wrapped
               }

               ..$forwardFactories
             }
             """
    }
  }

  def nodeClasses(schema: Schema): List[Tree] = schema.nodes.map { node => import node._
    val directNeighbours = node.neighbours_terms.map {
      case (accessorName, relation_term, endNode_type, endNode_term) =>
        q"""def $accessorName:Set[$endNode_type] = successorsAs($endNode_term,$relation_term)"""
    }

    val successorTraits = outRelationsToTrait.map { case (r, nodeTrait) =>
      val relationPlural = TermName(nameToPlural(r))
      accumulatedTraitNeighbours(r, node.neighbours, relationPlural, nodeTrait)
    }

    val directRevNeighbours = node.rev_neighbours_terms.map {
      case (accessorName, relation_term, startNode_type, startNode_term) =>
        q"""def $accessorName:Set[$startNode_type] = predecessorsAs($startNode_term, $relation_term)"""
    }

    val predecessorTraits = inRelationsFromTrait.map { case (r, nodeTrait) =>
      val relationPlural = TermName(rev(nameToPlural(r)))
      accumulatedTraitNeighbours(r, node.rev_neighbours, relationPlural, nodeTrait)
    }

    val nodeBody = statements.flatMap(generatePropertyAccessors(_))
    val superNodeTraitTypesWithDefault = if(superTypes.isEmpty) List(TypeName("Node")) else superTypes_type
    val externalSuperTypes_type = externalSuperTypes.map(TypeName(_))

    // if this is an implementedTrait, we should refer to the trait for labels
    val label = implementedTrait.map(_.name_label).getOrElse(name_label)
    val labels = implementedTrait.map(_.flatSuperTypesWithSelf).getOrElse(flatSuperTypesWithSelf).map(nameToLabel(_)).map(l => q"raw.Label($l)")

    q"""
            case class $name_type(rawItem: raw.Node) extends ..$superNodeTraitTypesWithDefault with ..$externalSuperTypes_type {
              override val label = raw.Label($label)
              override val labels = Set(..$labels)
              ..$directNeighbours
              ..$successorTraits
              ..$directRevNeighbours
              ..$predecessorTraits
              ..$nodeBody
            }
            """
  }

  def relationFactories(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
    val superFactories = if(superTypes.isEmpty) List(tq"AbstractRelationFactory[$startNode_type, $name_type, $endNode_type]") else superTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[$startNode_type, $name_type, $endNode_type]")
    val forwardFactories = forwardFactoryMethodsStartEnd(parameterList, traitFactoryParameterList, tq"$name_type", tq"$startNode_type", tq"$endNode_type")
    val parameterCode = List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: parameterList.toParamCode
    val optionalParameterList = parameterList.optional
    val optionalParameterCode = List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: optionalParameterList.toParamCode

    q"""
            object $name_term extends RelationFactory[$startNode_type, $name_type, $endNode_type]
             with ..$superFactories {
              val relationType = raw.RelationType($name_label)
              def wrap(relation: raw.Relation) = $name_term(
                $startNode_term.wrap(relation.startNode),
                relation,
                $endNode_term.wrap(relation.endNode))

              def create (..${ parameterCode }):$name_type = {
                val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem))
                ..${ parameterList.toAssignmentCode(q"wrapped.rawItem") }
                wrapped
              }

              def merge (..${ parameterCode }, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty):$name_type = {
                val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch))
                ..${ parameterList.toAssignmentCode(q"wrapped.rawItem") }
                wrapped
              }

              def matches (..${ optionalParameterCode }, matches: Set[PropertyKey] = Set.empty):$name_type = {
                val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches))
                ..${ optionalParameterList.toAssignmentCode(q"wrapped.rawItem") }
                wrapped
              }

              ..$forwardFactories
            }
            """
  }

  //TODO: external supertypes on relations
  def relationClasses(schema: Schema): List[Tree] = schema.relations.map { relation => import relation._
    val superTypesWithDefault = "Relation" :: superTypes
    val superTypesWithDefaultGenerics = superTypesWithDefault.map(TypeName(_)).map(superType => tq"$superType[$startNode_type,$endNode_type]")
    val relationBody = statements.flatMap(generatePropertyAccessors(_))

    q"""
            case class $name_type(startNode: $startNode_type, rawItem: raw.Relation, endNode: $endNode_type)
              extends ..$superTypesWithDefaultGenerics {

              ..$relationBody
            }
           """
  }


  def hyperRelationFactories(schema: Schema): List[Tree] = schema.hyperRelations.map { hyperRelation => import hyperRelation._
    val superRelationFactories = superRelationTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[$startNode_type, $name_type, $endNode_type]")
    val superNodeFactories = superNodeTypes.map(t => tq"${ TypeName(traitFactoryName(t)) }[$name_type]")
    val forwardFactories = forwardFactoryMethodsStartEnd(parameterList, traitFactoryParameterList, tq"$name_type", tq"$startNode_type", tq"$endNode_type")
    val parameterCode = List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: parameterList.toParamCode
    val optionalParameterList = parameterList.optional
    val optionalParameterCode = optionalParameterList.toParamCode
    val optionalParameterCodeStartEnd = List(q"val startNode:$startNode_type", q"val endNode:$endNode_type") ::: optionalParameterCode
    val labels = flatSuperNodeTypesWithSelf.map(nameToLabel(_)).map(l => q"raw.Label($l)")

    q"""
           object $name_term extends HyperRelationFactory[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type] with ..$superRelationFactories with ..$superNodeFactories {
             override val label = raw.Label($name_label)
             override val labels = Set(..$labels)
             override val startRelationType = raw.RelationType($startRelation_label)
             override val endRelationType = raw.RelationType($endRelation_label)

             override def wrap(node: raw.Node) = new $name_type(node)

             override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation) = {
               val hyperRelation = wrap(middleNode)
               hyperRelation._startRelation = $startRelation_term(
                  $startNode_term.wrap(startRelation.startNode),
                  startRelation,
                  hyperRelation
                )
                hyperRelation._endRelation = $endRelation_term(
                  hyperRelation,
                  endRelation,
                  $endNode_term.wrap(endRelation.endNode)
                )
                hyperRelation
             }

             def create (..${ parameterCode }):$name_type = {
                val middleNode = raw.Node.create(labels)
                ..${ parameterList.toAssignmentCode(q"middleNode") }
                wrap(
                  raw.Relation.create(startNode.rawItem, startRelationType, middleNode),
                  middleNode,
                  raw.Relation.create(middleNode, endRelationType, endNode.rawItem)
                )
             }

             def merge (..${ parameterCode }, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty):$name_type = {
                val middleNode = raw.Node.merge(labels, merge = merge, onMatch = onMatch)
                ..${ parameterList.toAssignmentCode(q"middleNode") }
                wrap(
                  raw.Relation.merge(startNode.rawItem, startRelationType, middleNode),
                  middleNode,
                  raw.Relation.merge(middleNode, endRelationType, endNode.rawItem)
                )
             }

             def matches (..${ optionalParameterCodeStartEnd }, matches: Set[PropertyKey] = Set.empty):$name_type = {
                val middleNode = raw.Node.matches(labels, matches = matches)
                ..${ optionalParameterList.toAssignmentCode(q"middleNode") }
                wrap(
                  raw.Relation.matches(startNode.rawItem, startRelationType, middleNode),
                  middleNode,
                  raw.Relation.matches(middleNode, endRelationType, endNode.rawItem)
                )
             }

             def matchesNode (..${ optionalParameterCode }, matches: Set[PropertyKey] = Set.empty):$name_type = {
                val middleNode = raw.Node.matches(labels, matches = matches)
                ..${ optionalParameterList.toAssignmentCode(q"middleNode") }
                wrap(middleNode)
             }

             ..$forwardFactories
           }
           """
  }

  def hyperRelationClasses(schema: Schema): List[Tree] = schema.hyperRelations.flatMap { hyperRelation => import hyperRelation._
    //TODO: generate indirect neighbour-accessors based on hyperrelations
    //TODO: property accessors
    val superRelationTypesGenerics = superRelationTypes.map(TypeName(_)).map(superType => tq"$superType[$startNode_type,$endNode_type]")
    val labels = flatSuperNodeTypesWithSelf.map(nameToLabel(_)).map(l => q"raw.Label($l)")
    val relationBody = statements.flatMap(generatePropertyAccessors(_))

    List(
      q"""
             case class $name_type(rawItem: raw.Node)
                extends HyperRelation[$startNode_type, $startRelation_type, $name_type, $endRelation_type, $endNode_type]
                with ..${ superRelationTypesGenerics ::: superNodeTypes.map(t => tq"${ TypeName(t) }") } {
                override val label = raw.Label($name_label)
                override val labels = Set(..$labels)
               ..$relationBody
             }
             """,
      q"""
             case class $startRelation_type(startNode: $startNode_type, rawItem: raw.Relation, endNode: $name_type)
               extends Relation[$startNode_type, $name_type]
             """,
      q"""
             case class $endRelation_type(startNode: $name_type, rawItem: raw.Relation, endNode: $endNode_type)
               extends Relation[$name_type, $endNode_type]
             """
    )
  }

  def nodeSuperTraits(schema: Schema): List[Tree] = schema.nodeTraits.map { nodeTrait => import nodeTrait._
    val superTypesWithDefault = (if(superTypes.isEmpty) List("Node") else superTypes).map(TypeName(_))
    val traitBody = statements.flatMap(generatePropertyAccessors(_))
    val matchesClassName = TypeName(traitMatchesClassName(name))

    q""" trait $name_type extends ..$superTypesWithDefault { ..$traitBody } """
  }

  def relationSuperTraits(schema: Schema): List[Tree] = schema.relationTraits.map { relationTrait => import relationTrait._
    val superTypesWithDefault = if(superTypes.isEmpty) List("AbstractRelation") else superTypes
    val superTypesWithDefaultGenerics = superTypesWithDefault.map(TypeName(_)).map(superType => tq"$superType[START,END]")
    val traitBody = statements.flatMap(generatePropertyAccessors(_))
    q""" trait $name_type[+START <: Node,+END <: Node] extends ..$superTypesWithDefaultGenerics { ..$traitBody } """
  }

  def graphFactories(schema: Schema): List[Tree] = schema.graphs.map { graph => import graph._
    q"""
          object $name_term {
            def empty = new $name_type(raw.Graph.empty)

            def apply(items: Item*) = {
              val wrapper = empty
              wrapper.add(items: _*)
              wrapper
            }
          }
          """
  }

  def graphClasses(schema: Schema): List[Tree] = schema.graphs.map { graph => import graph._
    // TODO: create subgraphs

    def itemSets(nameAs: String, names: List[String]) = names.map { name => q""" def ${ TermName(nameToPlural(name)) }: Set[${ TypeName(name) }] = ${ TermName(nameAs) }(${ TermName(name) }) """ }
    def allOf(items: List[String]) = (q"Set.empty" :: items.map(s => q"${ TermName(nameToPlural(s)) }")).reduce[Tree]((a, b) => q"$a ++ $b")

    val nodeSets = itemSets("nodesAs", nodes)
    val relationSets = itemSets("relationsAs", relations)
    val hyperRelationSets = itemSets("hyperRelationsAs", hyperRelations)

    val nodeTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
      q"def $name_plural_term:Set[$name_type] = ${ allOf(subNodes) }"
    }
    val relationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
      q"def ${ TermName(nameToPlural(name + "Relation")) }:Set[_ <: Relation[$name_type, $name_type]] = ${ allOf(subRelations.intersect(relations)) }"
    }
    val abstractRelationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
      q"def ${ TermName(nameToPlural(name + "AbstractRelation")) }:Set[_ <: AbstractRelation[$name_type, $name_type]] = ${ allOf(subRelations.intersect(relationsWithHyperRelations)) }"
    }
    val hyperRelationTraitSets = nodeTraits.map { nodeTrait => import nodeTrait._
      val hyperNodeRelationTraits_type = commonHyperNodeRelationTraits_type.map(t => tq"$t[$name_type, $name_type]")
      q"""
            def ${ TermName(nameToPlural(name + "HyperRelation")) } :Set[HyperRelation[
                    $name_type,
                    _ <: Relation[$name_type, _],
                    _ <: HyperRelation[$name_type, _, _, _, $name_type] with ..$commonHyperNodeNodeTraits_type with ..$hyperNodeRelationTraits_type,
                    _ <: Relation[_, $name_type],
                    $name_type]
                    with ..$commonHyperNodeNodeTraits_type with ..$hyperNodeRelationTraits_type]
                = ${ allOf(subHyperRelations.intersect(hyperRelations)) }
            """
    }

    //TODO: Common traits for nodes, relations, abstractRelations, hyperRelations
    q"""
           case class $name_type(graph: raw.Graph) extends Graph {
             ..$nodeSets
             ..$relationSets
             ..$hyperRelationSets

             ..$nodeTraitSets
             ..$relationTraitSets
             ..$abstractRelationTraitSets
             ..$hyperRelationTraitSets

             def nodes: Set[Node] = ${ allOf(nodes) }
             def relations: Set[_ <: Relation[_,_]] = ${ allOf(relations) }
             def abstractRelations: Set[_ <: AbstractRelation[_,_]] = ${ allOf(relationsWithHyperRelations) }
             def hyperRelations: Set[_ <: HyperRelation[_,_,_,_,_]] = ${ allOf(hyperRelations) }
           }
           """
  }

  def nodeLabelToFactoryMap(schema: Schema): Tree = {
    //TODO: node trait labels?
    val tuples = schema.nodes.map { node =>
      // if this is an implemented trait, we should refer to the trait itself
      val traitTerm = node.implementedTrait.getOrElse(node).name_term
      val term = node.name_term
      q""" ($traitTerm.labels, $term) """
    } ++ schema.hyperRelations.map(n => q""" (${ n.name_term }.labels, ${ n.name_term }) """)

    q""" Map[Set[raw.Label],NodeFactory[Node]](..$tuples) """
  }

  def otherStatements(schema: Schema): List[Tree] = schema.statements.filterNot { statement =>
    NodePattern.unapply(statement).isDefined ||
      RelationPattern.unapply(statement).isDefined ||
      HyperRelationPattern.unapply(statement).isDefined ||
      NodeTraitPattern.unapply(statement).isDefined ||
      RelationTraitPattern.unapply(statement).isDefined ||
      GraphPattern.unapply(statement).isDefined
  }


  //TODO: RootNodeTraitFactory.wrap can fail with unknown labels
  def schema(schema: Schema): Tree = {
    import schema.{name_term, superTypes_type}
    q"""
           object $name_term extends ..$superTypes_type {
             import renesca.{graph => raw}
             import renesca.schema._
             import renesca.parameter._
             import renesca.parameter.implicits._

             val nodeLabelToFactory = ${ nodeLabelToFactoryMap(schema) }

             trait RootNodeTraitFactory[+NODE <: Node] {
               def factory(node: raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
               def wrap(node: raw.Node) = factory(node).wrap(node)
             }

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

             ..${ graphFactories(schema) }
             ..${ graphClasses(schema) }

             ..${ otherStatements(schema) }
           }
           """
  }
}
