package codegeneration

import helpers.CodeComparisonSpec

class GraphClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "Empty Graph" >> {
    generatedContainsCode(
      q"object A {@Graph trait G}",
      q"""case class G(graph: raw.Graph) extends Graph {
            def nodes: Seq[Node] = Seq.empty;
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          } """
    )
  }

  "with nodes" >> {
    generatedContainsCode(
      q"object A {@Graph trait G {Nodes(N,M)}; @Node class N; @Node class M}",
      q"""def ns: Seq[N] = nodesAs(N);""",
      q"""def ms: Seq[M] = nodesAs(M);""",
      """def nodes: Seq[Node] = Seq.empty.++(ns).++(ms);"""
    )
  }
  "with nodes inserted by traits" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N
            @Node trait T
            @Node class M extends T
            @Node class O extends T
            @Graph trait G {Nodes(N,T)}
          }""",
      q"""def nodes: Seq[Node] = Seq.empty.++(ns).++(ms).++(os)"""
    )
  }

  "with relations inserted by inherited traits" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait T
            @Node trait S
            @Node class N extends T
            @Node class M extends S
            @Relation class R1(startNode:N, endNode:M)
            @Relation class R2(startNode:T, endNode:S)
            @Relation class R3(startNode:N, endNode:S)
            @Graph trait G {Nodes(T,S)}
          }""",
      q"""def r1s: Seq[R1] = relationsAs(R1);""",
      q"""def r2s: Seq[R2] = relationsAs(R2);""",
      q"""def r3s: Seq[R3] = relationsAs(R3);"""
    )
  }

  "with nodes inserted by inherited traits" >> {
    generatedContainsCode(
      q"""object A {
            @Node trait T
            @Node trait S extends T
            @Node trait X extends T
            @Node class M extends S
            @Node class O extends S
            @Node class N
            @Graph trait G {Nodes(N,S)}
          }""",
      q"""def ns: Seq[N] = nodesAs(N);""",
      q"""def ms: Seq[M] = nodesAs(M);""",
      q"""def os: Seq[O] = nodesAs(O);"""
    )
  }

  "with duplicate nodes" >> {
    generatedContainsCode(
      q"object A {@Graph trait G {Nodes(N,M,N)}; @Node class N; @Node class M}",
      q"""def ns: Seq[N] = nodesAs(N);""",
      q"""def ms: Seq[M] = nodesAs(M);""",
      """def nodes: Seq[Node] = Seq.empty.++(ns).++(ms);"""
    )
  }

  "with inherited nodes" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N; @Node class M; @Node class O
            @Graph trait T{Nodes(O)}
            @Graph trait G extends T {Nodes(N,M)}
          }""",
      q"""case class T(graph: raw.Graph) extends Graph {
            def os: Seq[O] = nodesAs(O);
            def nodes: Seq[Node] = Seq.empty.++(os);
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          };""",
      q"""case class G(graph: raw.Graph) extends Graph {
            def ns: Seq[N] = nodesAs(N);
            def ms: Seq[M] = nodesAs(M);
            def os: Seq[O] = nodesAs(O);
            def nodes: Seq[Node] = Seq.empty.++(ns).++(ms).++(os);
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          }"""
    )
  }

  "with inherited nodes from two graphs" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N; @Node class M; @Node class O
            @Graph trait T{Nodes(O)}
            @Graph trait S{Nodes(M)}
            @Graph trait G extends T with S {Nodes(N)}
          }""",
      q"""case class G(graph: raw.Graph) extends Graph {
            def ns: Seq[N] = nodesAs(N);
            def os: Seq[O] = nodesAs(O);
            def ms: Seq[M] = nodesAs(M);
            def nodes: Seq[Node] = Seq.empty.++(ns).++(os).++(ms);
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          }"""
    )
  }

  "with same inherited node from two graphs" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N
            @Graph trait T{Nodes(N)}
            @Graph trait S{Nodes(N)}
            @Graph trait G extends T with S {Nodes()}
          }""",
      q"""case class G(graph: raw.Graph) extends Graph {
            def ns: Seq[N] = nodesAs(N);
            def nodes: Seq[Node] = Seq.empty.++(ns);
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          }"""
    )
  }

  "with same inherited node from diamond inheritance" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N
            @Graph trait A{Nodes(N)}
            @Graph trait T extends A
            @Graph trait S extends A
            @Graph trait G extends T with S
          }""",
      q"""case class G(graph: raw.Graph) extends Graph {
            def ns: Seq[N] = nodesAs(N);
            def nodes: Seq[Node] = Seq.empty.++(ns);
            def relations: (Seq[_$$18] forSome { 
              type _$$18 <: (Relation[_$$26, _$$24] forSome { 
                type _$$26;
                type _$$24
              })
            }) = Seq.empty;
            def abstractRelations: (Seq[_$$22] forSome { 
              type _$$22 <: (AbstractRelation[_$$25, _$$23] forSome { 
                type _$$25;
                type _$$23
              })
            }) = Seq.empty;
            def hyperRelations: (Seq[_$$21] forSome { 
              type _$$21 <: (HyperRelation[_$$20, _$$16, _$$15, _$$19, _$$17] forSome { 
                type _$$20;
                type _$$16;
                type _$$15;
                type _$$19;
                type _$$17
              })
            }) = Seq.empty
          }"""
    )
  }

  "with relations" >> {
    generatedContainsCode(
      q"object A {@Graph trait G {Nodes(N,M)}; @Node class N; @Node class M; @Relation class R(startNode:N, endNode: M); @Relation class S(startNode:M, endNode: N)}",
      q"""def rs: Seq[R] = relationsAs(R);""",
      q"""def s: Seq[S] = relationsAs(S);""",
      """def relations: (Seq[_] forSome { 
            type _ <: (Relation[_, _] forSome { 
              type _;
              type _
            })
          }) = Seq.empty.++(rs).++(s);""",
      """def abstractRelations: (Seq[_] forSome { 
            type _ <: (AbstractRelation[_, _] forSome { 
              type _;
              type _
            })
          }) = Seq.empty.++(rs).++(s);"""
    )
  }

  "with relations between chained super trait" >> {
    generatedContainsCode(
      q"""object A {
            @Graph trait G {Nodes(N,M)}
            @Node class N
            @Node trait S
            @Node trait T extends S
            @Node class M extends T
            @Relation class R(startNode:N, endNode: S)
          }""",
      q"""def rs: Seq[R] = relationsAs(R);""",
      """def relations: (Seq[_] forSome { 
            type _ <: (Relation[_, _] forSome { 
              type _;
              type _
            })
          }) = Seq.empty.++(rs);""",
      """def abstractRelations: (Seq[_] forSome { 
            type _ <: (AbstractRelation[_, _] forSome { 
              type _;
              type _
            })
          }) = Seq.empty.++(rs);"""
    )
  }

  "with hyperRelations" >> {
    generatedContainsCode(
      q"object A {@Graph trait G {Nodes(N,M)}; @Node class N; @Node class M; @HyperRelation class R(startNode:N, endNode: M);}",
      q"""def rs: Seq[R] = hyperRelationsAs(R);""",
      """def hyperRelations: (Seq[_] forSome { 
            type _ <: (HyperRelation[_, _, _, _, _] forSome { 
              type _;
              type _;
              type _;
              type _;
              type _
            })
          }) = Seq.empty.++(rs)""",
      """def abstractRelations: (Seq[_] forSome { 
            type _ <: (AbstractRelation[_, _] forSome { 
              type _;
              type _
            })
          }) = Seq.empty.++(rs); """
    )
  }

  "with node trait and one node" >> {
    generatedContainsCode(
      q"object A {@Graph trait G {Nodes(N)}; @Node trait T; @Node class N extends T;}",
      """def ts: Seq[T] = Seq.empty.++(ns);""", // tNodes
      """def tRelations: (Seq[_] forSome { 
            type _ <: Relation[T, T]
          }) = Seq.empty;""",
      """def tAbstractRelations: (Seq[_] forSome { 
            type _ <: AbstractRelation[T, T]
          }) = Seq.empty;""",
      """def tHyperRelations: Seq[(HyperRelation[T, _, _, _, T] forSome { 
            type _ <: (Relation[T, _] forSome { 
              type _
            });
            type _ <: (HyperRelation[T, _, _, _, T] forSome { 
              type _;
              type _;
              type _
            });
            type _ <: (Relation[_, T] forSome { 
              type _
            })
          })] = Seq.empty;"""
    )
  }

  "with node trait with relations" >> {
    generatedContainsCode(
      q"""object A {@Graph trait G {Nodes(N,M)}; @Node trait T;
        @Node class N extends T;
        @Node class M extends T;
        @Relation class R(startNode:N, endNode:M)
      }""",
      """def tRelations: (Seq[_] forSome { 
            type _ <: Relation[T, T]
          }) = Seq.empty.++(rs);""",
      """def tAbstractRelations: (Seq[_] forSome {
            type _ <: AbstractRelation[T, T]
          }) = Seq.empty.++(rs);"""
    )
  }

  "list trait relations only if in Graph and trait" >> {
    generatedContainsCode(
      q"""object A {@Graph trait G {Nodes(M,N,O,P)};
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
      """def tRelations: (Seq[_] forSome { 
            type _ <: Relation[T, T]
          }) = Seq.empty.++(rs);""",
      """def tAbstractRelations: (Seq[_] forSome { 
            type _ <: AbstractRelation[T, T]
       }) = Seq.empty.++(rs);"""
    )
  }

  "list trait hyperrelations only if in Graph and trait" >> {
    generatedContainsCode(
      q"""object A {@Graph trait G {Nodes(M,N,O,P)};
        @Node trait T;
        @Node trait S;
        @Node class M extends T;
        @Node class N extends T;
        @Node class O extends S;
        @Node class P
        @Node class Q
        @HyperRelation class R(startNode:N, endNode:M)
        @HyperRelation class R2(startNode:N, endNode:O)
        @HyperRelation class R3(startNode:N, endNode:P)
        @HyperRelation class R4(startNode:N, endNode:Q)
      }""",
      """def tAbstractRelations: (Seq[_] forSome {
          type _ <: AbstractRelation[T, T]
        }) = Seq.empty.++(rs);""",
      """def tHyperRelations: Seq[(HyperRelation[T, _, _, _, T] forSome {
          type _ <: (Relation[T, _] forSome { 
            type _
          });
          type _ <: (HyperRelation[T, _, _, _, T] forSome { 
            type _;
            type _;
            type _
          });
          type _ <: (Relation[_, T] forSome { 
            type _
          })
     })] = Seq.empty.++(rs);"""
    )
  }

  "common hyperRelation traits between nodes of trait" >> {
    generatedContainsCode(
      q"""object A {@Graph trait G {Nodes(M,N,O,P)};
        @Node trait T;
        @Node class N extends T;
        @Node class M extends T;
        @Node class O extends T;
        @Node class P extends T;
        @Node trait X
        @HyperRelation class R(startNode:N, endNode:M) extends X
        @HyperRelation class R2(startNode:N, endNode:O) extends X
        @HyperRelation class R3(startNode:N, endNode:P) extends X
      }""",
      """def tHyperRelations: Seq[(HyperRelation[T, _, _, _, T] forSome {
          type _ <: (Relation[T, _] forSome { 
            type _
          });
          type _ <: (HyperRelation[T, _, _, _, T] forSome { 
            type _;
            type _;
            type _
          }) with X;
          type _ <: (Relation[_, T] forSome { 
            type _
          })
        }) with X] = Seq.empty.++(rs).++(r2s).++(r3s);"""

    )
  }

  "common hyperRelation traits between nodes of trait (multiple inheritance)" >> {
    generatedContainsCode(
      q"""object A {@Graph trait G {Nodes(M,N,O,P)};
        @Node trait T;
        @Node class N extends T;
        @Node class M extends T;
        @Node class O extends T;
        @Node class P extends T;
        @Node trait X
        @Node trait Y
        @Node trait Z
        @HyperRelation class R(startNode:N, endNode:M) extends X with Y
        @HyperRelation class R2(startNode:N, endNode:O) extends X
        @HyperRelation class R3(startNode:N, endNode:P) extends Z with X
      }""",
      """def tHyperRelations: Seq[(HyperRelation[T, _, _, _, T] forSome {
          type _ <: (Relation[T, _] forSome { 
            type _
          });
          type _ <: (HyperRelation[T, _, _, _, T] forSome { 
            type _;
            type _;
            type _
          }) with X;
          type _ <: (Relation[_, T] forSome { 
            type _
          })
        }) with X] = Seq.empty.++(rs).++(r2s).++(r3s);"""
    )
  }
}
