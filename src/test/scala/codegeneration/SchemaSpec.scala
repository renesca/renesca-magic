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
       import renesca.QueryHandler
       import renesca.schema._
       import renesca.parameter._
       import renesca.parameter.implicits._

       val nodeLabelToFactory = Map[Set[raw.Label],NodeFactory[Node]]()

       trait RootNodeTraitFactory[+NODE <: Node] {
         def factory(node:raw.Node) = nodeLabelToFactory(node.labels.toSet).asInstanceOf[NodeFactory[NODE]];
         def wrap(node: raw.Node) = factory(node).wrap(node)
       }

       def setupDbConstraints(queryHandler: QueryHandler) = ()
      } """)
  }

  "Schema with uniqueness constraints" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait S { @unique var w: Long; var noW: Long }
            @Node trait T { @unique var x: Long = 1; var noX: Long }
            @Node class N extends T { @unique val y: Boolean; val noY: Boolean }
            @HyperRelation class R(startNode: T, endNode: N) extends T { @unique val z: String = "a"; val noZ: String }
          }
          """,
      q"""def setupDbConstraints(queryHandler: QueryHandler) = {
            queryHandler.query("CREATE CONSTRAINT ON (n:N) ASSERT n.y IS UNIQUE");
            queryHandler.query("CREATE CONSTRAINT ON (n:S) ASSERT n.w IS UNIQUE");
            queryHandler.query("CREATE CONSTRAINT ON (n:T) ASSERT n.x IS UNIQUE");
            queryHandler.query("CREATE CONSTRAINT ON (n:R) ASSERT n.z IS UNIQUE")
          }
          """)
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
