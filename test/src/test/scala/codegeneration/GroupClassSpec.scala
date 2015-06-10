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
      q"""def nodes: Set[Node] = Set.empty.++(ns).++(ms);"""
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
    }) = Set.empty.++(rs)++(s);"""
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
    }) = Set.empty.++(rs) """
    )
  }
}
