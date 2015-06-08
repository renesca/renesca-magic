import org.specs2.mutable.Specification

class GenerationSpec extends Specification with CodeComparison {

  import contextMock.universe._

  "Empty Schema" >> {
    generatedContainsCode(
      q"object A",
      q"""
      object A {
       import renesca.{graph => raw}
       import renesca.schema._
       import renesca.parameter._
       import renesca.parameter.implicits._

       val nodeLabelToFactory = Map[raw.Label,NodeFactory[_ <: Node]]()

       trait RootNodeTraitFactory[NODE <: Node] {
         def wrap(node: raw.Node) = {
           val factory = nodeLabelToFactory(node.labels.head).asInstanceOf[NodeFactory[NODE]]
           factory.wrap(node)
         }
       }
      }
         """)
  }


  // TODO: "node trait field inheritance"
  "Properties" >> {
    "immutable property getter" >> {
      generatedContainsCode(
        q"object A {@Node class N {val p:String}}",
        q"""def p: String = node.properties("p").asInstanceOf[StringPropertyValue]""")
    }

    "optional immutable property getter" >> {
      generatedContainsCode(
        q"object A {@Node class N {val p:Option[String]}}",
        q"""def p:Option[String] = node.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""")
    }

    "mutable property getter and setter" >> {
      generatedContainsCode(
        q"object A {@Node class N {var p:String}}",
        q"""def p: String = node.properties("p").asInstanceOf[StringPropertyValue]""",
        q"""def `p_=`(newValue: String): scala.Unit = node.properties.update("p", newValue)"""
      )
    }

    "optional mutable property getter and setter" >> {
      generatedContainsCode(
        q"object A {@Node class N {var p:Option[String]}}",
        q"""def p:Option[String] = node.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""",
        q"""def `p_=`(newValue:Option[String]): scala.Unit = { if(newValue.isDefined) node.properties("p") = newValue.get else node.properties -= "p" }"""
      )
    }
  }
  "Node class" >> {
    "preserve custom code" >> {
      generatedContainsCode(
        q"object A {@Node class N {def custom = 0}}",
        q"""def custom = 0"""
      )
    }
  }


  "Node trait factory" >> {
    "simple factory trait" >> {
      generatedContainsCode(
        q"object A {@Node trait T}",
        q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] { def localT(): NODE }"""
      )
    }
    "with own factory" >> {
      generatedContainsCode(
        q"object A {@Node trait T; @Node class N extends T}",
        q"""object T extends RootNodeTraitFactory[T]"""
      )
    }
    //TODO: no own factory, when there is no node extending the trait
    "with local-interface" >> {
      generatedContainsCode(
        q"object A {@Node trait T {val p:String}}",
        q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] { def localT(p: String): NODE }"""
      )
    }
    "with superType factories" >> {
      generatedContainsCode(
        q"object A {@Node trait T ; @Node trait X extends T {val p:String} }",
        q"""trait XFactory[NODE <: X] extends NodeFactory[NODE] with TFactory[NODE] { def localX(p: String): NODE }"""
      )
    }
  }

  "relation trait factory" >> {
    "simple relation trait" >> {
      generatedContainsCode(
        q"object A {@Relation trait T}",
        q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END): RELATION
            }"""
      )
    }
    "with local interface" >> {
      generatedContainsCode(
        q"object A {@Relation trait T {val p:String}}",
        q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END, p: String): RELATION
            };"""
      )
    }

    //TODO: "with own factory" >> { }

    "with superType factories" >> {
      generatedContainsCode(
        q"object A {@Relation trait T; @Relation trait X extends T}",
        q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TFactory[START, RELATION, END] {
              def localX(startNode: START, endNode: END): RELATION
            }"""
      )
    }
  }
}
