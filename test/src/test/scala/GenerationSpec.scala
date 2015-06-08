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


  // "node trait field inheritance"
  "primitive accessor" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:String}}",
      q""" case class N(node: raw.Node) extends Node {
              def p: String = node.properties("p").asInstanceOf[StringPropertyValue]
            } """)
  }
}
