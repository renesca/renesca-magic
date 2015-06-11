package codegeneration

import org.specs2.mutable.Specification

class GroupClassSpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Group trait G}",
      q"""case class G(graph: raw.Graph) extends Graph {
            def nodes: Set[Node] = Set.empty;
            def relations: (Set[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Set.empty;
            def abstractRelations: (Set[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Set.empty;
            def hyperRelations: (Set[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Set.empty
          } """
    )
  }
  "with nodes" >> {
    generatedContainsCode(
      q"object A {@Group trait G {List(N,M)}; @Node class N; @Node class M}",
      q"""def ns: Set[N] = nodesAs(N);""",
      q"""def ms: Set[M] = nodesAs(M);""",
      q"""def nodes: Set[Node] = ns.++(ms.++(Set.empty));"""
    )
  }
  "with relations" >> {
    generatedContainsCode(
      q"object A {@Group trait G {List(N,M)}; @Node class N; @Node class M; @Relation class R(startNode:N, endNode: M); @Relation class S(startNode:M, endNode: N)}",
      q"""def rs: Set[R] = relationsAs(R);""",
      q"""def s: Set[S] = relationsAs(S);""",
      q"""def relations: (Set[_$$629] forSome { 
            type _$$629 <: (Relation[_$$637, _$$635] forSome { 
              type _$$637;
              type _$$635
            })
          }) = rs++(s.++(Set.empty));""",
      q"""def abstractRelations: (Set[_$$563] forSome { 
            type _$$563 <: (AbstractRelation[_$$566, _$$564] forSome { 
              type _$$566;
              type _$$564
            })
          }) = rs.++(s.++(Set.empty));"""
    )
  }
  "with hyperRelations" >> {
    generatedContainsCode(
      q"object A {@Group trait G {List(N,M)}; @Node class N; @Node class M; @HyperRelation class R(startNode:N, endNode: M);}",
      q"""def rs: Set[R] = hyperRelationsAs(R);""",
      q"""def hyperRelations: (Set[_$$1386] forSome { 
            type _$$1386 <: (HyperRelation[_$$1385, _$$1381, _$$1380, _$$1384, _$$1382] forSome { 
              type _$$1385;
              type _$$1381;
              type _$$1380;
              type _$$1384;
              type _$$1382
            })
          }) = rs.++(Set.empty) """,
      q"""def abstractRelations: (Set[_$$551] forSome { 
            type _$$551 <: (AbstractRelation[_$$554, _$$552] forSome { 
              type _$$554;
              type _$$552
            })
          }) = rs.++(Set.empty); """
    )
  }

  "with node trait and one node" >> {
    generatedContainsCode(
      q"object A {@Group trait G {List(N)}; @Node trait T; @Node class N extends T;}",
      q"""def ts: Set[T] = ns.++(Set.empty);""", // tNodes
      q"""def tRelations: (Set[_$$194] forSome { 
            type _$$194 <: Relation[T, T]
          }) = Set.empty;""",
      q"""def tAbstractRelations: (Set[_$$195] forSome { 
            type _$$195 <: AbstractRelation[T, T]
          }) = Set.empty;""",
      q"""def tHyperRelations: Set[(HyperRelation[T, _$$196, _$$201, _$$199, T] forSome { 
            type _$$196 <: (Relation[T, _$$203] forSome { 
              type _$$203
            });
            type _$$201 <: (HyperRelation[T, _$$198, _$$202, _$$200, T] forSome { 
              type _$$198;
              type _$$202;
              type _$$200
            });
            type _$$199 <: (Relation[_$$197, T] forSome { 
              type _$$197
            })
          })] = Set.empty;"""
    )
  }

  "with node trait with relations" >> {
    generatedContainsCode(
      q"""object A {@Group trait G {List(N,M)}; @Node trait T;
        @Node class N extends T;
        @Node class M extends T;
        @Relation class R(startNode:N, endNode:M)
      }""",
      q"""def tRelations: (Set[_$$1272] forSome { 
            type _$$1272 <: Relation[T, T]
          }) = rs.++(Set.empty);""",
      q"""def tAbstractRelations: (Set[_$$1273] forSome { 
            type _$$1273 <: AbstractRelation[T, T]
          }) = rs.++(Set.empty);"""
    )
  }
  "list trait relations only if in group and trait" >> {
    generatedContainsCodePrint(
      q"""object A {@Group trait G {List(M,N,O,P)};
        @Node trait T;
        @Node trait S;
        @Node class M extends T;
        @Node class N extends T;
        @Node class O extends S;
        @Node class P
        @Node class Q
        @Relation class R(startNode:N, endNode:M)
        @Relation class R2(startNode:N, endNode:O)
        @Relation class R3(startNode:N, endNode:P)
        @Relation class R4(startNode:N, endNode:Q)
      }""",
      q"""def tRelations: (Set[_$$1272] forSome { 
            type _$$1272 <: Relation[T, T]
          }) = rs.++(Set.empty);""",
      q"""def tAbstractRelations: (Set[_$$1273] forSome { 
            type _$$1273 <: AbstractRelation[T, T]
          }) = rs.++(Set.empty);"""
    )
  }

}
