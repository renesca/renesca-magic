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

       val nodeLabelToFactory = Map[Set[raw.Label],NodeFactory[Node]]()

       trait RootNodeTraitFactory[+NODE <: Node] {
         def factory(node:raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
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
      q"""val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(A.labels, A));""")
  }
  "RootNodeTraitFactory with Nodes, Relation and traits" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait X;
            @Relation trait Y;
            @Node class A extends X; @Node class B extends X;
            @Relation class R(startNode:A, endNode:B) extends Y
          }""",
      q"""val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(X.labels, XMatches), scala.Tuple2(A.labels, A), scala.Tuple2(B.labels, B));""")
  }
  "RootNodeTraitFactory with HyperNode" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait X;
            @Relation trait Y;
            @Node class A extends X; @Node class B extends X;
            @HyperRelation class R(startNode:A, endNode:B) extends Y
          }""",
      q"""val nodeLabelToFactory = Map[Set[raw.Label], NodeFactory[Node]](scala.Tuple2(X.labels, XMatches), scala.Tuple2(A.labels, A), scala.Tuple2(B.labels, B), scala.Tuple2(R.labels, R));""")
  }
}
