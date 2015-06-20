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
         val nodeLabels:Set[raw.Label] = Set()
         def nodeLabel(node:raw.Node):raw.Label = (nodeLabels intersect node.labels).head
         def factory(node:raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
         def wrap(node: raw.Node) = factory(node).wrap(node)
       }
      } """)
  }
  "custom code" >> {
    generatedContainsCode(
      q"object A {def custom = 0}",
      q"""def custom = 0""")
  }
  "RootNodeTraitFactory with one Node" >> {
    generatedContainsCode(
      q"object A {@Node class A}",
      q"""val nodeLabelToFactory = Map[raw.Label,NodeFactory[_ <: Node]](("A", A))""",
      q"""trait RootNodeTraitFactory[NODE <: Node] {
            val nodeLabels:Set[raw.Label] = Set("A")
            def nodeLabel(node:raw.Node):raw.Label = (nodeLabels intersect node.labels).head
            def factory(node:raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
            def wrap(node: raw.Node) = factory(node).wrap(node)
          }""")
  }
  "RootNodeTraitFactory with Nodes, Relation and traits" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait X;
            @Relation trait Y;
            @Node class A extends X; @Node class B extends X;
            @Relation class R(startNode:A, endNode:B) extends Y
          }""",
      q"""val nodeLabelToFactory = Map[raw.Label,NodeFactory[_ <: Node]](("A",A), ("B", B))""",
      q"""trait RootNodeTraitFactory[NODE <: Node] {
            val nodeLabels:Set[raw.Label] = Set("A", "B")
            def nodeLabel(node:raw.Node):raw.Label = (nodeLabels intersect node.labels).head
            def factory(node:raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
            def wrap(node: raw.Node) = factory(node).wrap(node)
          }""")
  }
  "RootNodeTraitFactory with HyperNode" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait X;
            @Relation trait Y;
            @Node class A extends X; @Node class B extends X;
            @HyperRelation class R(startNode:A, endNode:B) extends Y
          }""",
      q"""val nodeLabelToFactory = Map[raw.Label,NodeFactory[_ <: Node]](("A",A), ("B", B), ("R", R))""",
      q"""trait RootNodeTraitFactory[NODE <: Node] {
            val nodeLabels:Set[raw.Label] = Set("A", "B", "R")
            def nodeLabel(node:raw.Node):raw.Label = (nodeLabels intersect node.labels).head
            def factory(node:raw.Node) = nodeLabelToFactory(nodeLabel(node)).asInstanceOf[NodeFactory[NODE]];
            def wrap(node: raw.Node) = factory(node).wrap(node)
          }""")
  }
}
