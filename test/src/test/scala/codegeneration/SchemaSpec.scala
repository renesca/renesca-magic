package codegeneration

import helpers.CodeComparisonSpec

class SchemaSpec extends CodeComparisonSpec {


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
         val label:raw.Label
         val labels:Set[raw.Label]
         lazy val factory = nodeLabelToFactory(label).asInstanceOf[NodeFactory[NODE]];
         def wrap(node: raw.Node) = factory.wrap(node)
       }
      } """)

    //TODO: custom code
  }
}
