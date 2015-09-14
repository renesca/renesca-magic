package codegeneration

import helpers.CodeComparisonSpec

class SchemaSpec extends CodeComparisonSpec {


  import contextMock.universe._

  "Empty Schema" >> {
    generatedContainsCode(
      q"object Empty",
      q"""
      object Empty {
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


        object WholeEmpty {
          def empty = new WholeEmpty(raw.Graph.empty);
          def remove(items: Item*) = {
            val wrapper = empty;
            wrapper.remove(((items): _*));
            wrapper
          }
          def apply(items: Item*) = {
            val wrapper = empty;
            wrapper.add(((items): _*));
            wrapper
          }
        };
        case class WholeEmpty(graph: raw.Graph) extends Graph {
          def nodes: Seq[Node] = Seq.empty;
          def relations: (Seq[_$$3] forSome { 
            type _$$3 <: (Relation[_$$12, _$$11] forSome { 
              type _$$12;
              type _$$11
            })
          }) = Seq.empty;
          def abstractRelations: (Seq[_$$9] forSome { 
            type _$$9 <: (AbstractRelation[_$$6, _$$10] forSome { 
              type _$$6;
              type _$$10
            })
          }) = Seq.empty;
          def hyperRelations: (Seq[_$$8] forSome { 
            type _$$8 <: (HyperRelation[_$$5, _$$4, _$$7, _$$2, _$$1] forSome { 
              type _$$5;
              type _$$4;
              type _$$7;
              type _$$2;
              type _$$1
            })
          }) = Seq.empty
        }
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

  "One Graph which covers the whole Schema" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait T
            @Relation trait R
            @Node class N extends T
            @Node class M
            @HyperRelation class H(startNode:M, endNode:T) extends T
          }""",
      q"""
          object WholeA {
            def empty = new WholeA(raw.Graph.empty);
            def remove(items: Item*) = {
              val wrapper = empty;
              wrapper.remove(((items): _*));
              wrapper
            }
            def apply(items: Item*) = {
              val wrapper = empty;
              wrapper.add(((items): _*));
              wrapper
            }
          };
          """,
          q"""
          case class WholeA(graph: raw.Graph) extends Graph {
            def ns: Seq[N] = nodesAs(N);
            def ms: Seq[M] = nodesAs(M);
            def hs: Seq[H] = hyperRelationsAs(H);
            def ts: Seq[T] = Seq.empty.++(ns).++(hs);
            def tRelations: (Seq[_$$1] forSome {
              type _$$1 <: Relation[T, T]
            }) = Seq.empty;
            def tAbstractRelations: (Seq[_$$2] forSome {
              type _$$2 <: AbstractRelation[T, T]
            }) = Seq.empty;
            def tHyperRelations: Seq[(HyperRelation[T, _$$3, _$$9, _$$7, T] forSome {
              type _$$3 <: (Relation[T, _$$10] forSome {
                type _$$10
              });
              type _$$9 <: (HyperRelation[T, _$$6, _$$5, _$$8, T] forSome {
                type _$$6;
                type _$$5;
                type _$$8
              });
              type _$$7 <: (Relation[_$$4, T] forSome {
                type _$$4
              })
            })] = Seq.empty;
            def nodes: Seq[Node] = Seq.empty.++(ns).++(ms).++(hs);
            def relations: (Seq[_$$13] forSome {
              type _$$13 <: (Relation[_$$22, _$$21] forSome {
                type _$$22;
                type _$$21
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$19] forSome {
              type _$$19 <: (AbstractRelation[_$$16, _$$20] forSome {
                type _$$16;
                type _$$20
              })
            }) = Seq.empty.++(hs);
            def hyperRelations: (Seq[_$$18] forSome {
              type _$$18 <: (HyperRelation[_$$15, _$$14, _$$17, _$$12, _$$11] forSome {
                type _$$15;
                type _$$14;
                type _$$17;
                type _$$12;
                type _$$11
              })
            }) = Seq.empty.++(hs)
          }
      """)
  }
}
