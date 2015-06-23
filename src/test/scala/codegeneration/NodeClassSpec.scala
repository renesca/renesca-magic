package codegeneration

import helpers.CodeComparisonSpec

class NodeClassSpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Node class N}",
      q"""case class N(node: raw.Node) extends Node {
        override val label = raw.Label("N")
        override val labels = Set(raw.Label("N"))
      }"""
    )
  }
  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Node class N {def custom = 0}}",
      q""" case class N(node: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
            def custom = 0
          } """)
  }
  "with super types" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      q"""case class N(node: raw.Node) extends T  {
          override val label = raw.Label("N")
          override val labels = Set(raw.Label("N"), raw.Label("T"))
        }"""
    )
  }
  "with multiple super types" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S; @Node class N extends T with S}",
      q"""case class N(node: raw.Node) extends T with S  {
          override val label = raw.Label("N")
          override val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))
         }
      """
    )
  }
  //TODO: which other supertype constellations can appear?
  "with external super types (no nodeTraits)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T with Ext}",
      q"""case class N(node: raw.Node) extends T with Ext  {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"), raw.Label("T"))
          }
      """
    )
  }
  "direct neighbour accessors" >> {
    generatedContainsCode(
      q"object A {@Node class N; @Node class M; @Relation class R(startNode:N,endNode:M)}",
      q"""case class N(node: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
              def rs: Set[M] = successorsAs(M, R)
            };""",
      q"""case class M(node: raw.Node) extends Node {
            override val label = raw.Label("M")
            override val labels = Set(raw.Label("M"))
              def rev_rs: Set[N] = predecessorsAs(N, R)
            };"""
    )
  }
  "direct neighbour accessors over hyperrelations" >> {
    generatedContainsCode(
      q"object A {@Node class N; @Node class M; @HyperRelation class R(startNode:N,endNode:M)}",
      q"""case class N(node: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
              def rs: Set[M] = successorsAs(M, R)
            };""",
      q"""case class M(node: raw.Node) extends Node {
            override val label = raw.Label("M")
            override val labels = Set(raw.Label("M"))
              def rev_rs: Set[N] = predecessorsAs(N, R)
            };"""
    )
  }
  "accessors for successor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait T;
            @Node class N extends T; @Node class M extends T;
            @Node class L;
            @Relation class R(startNode:L,endNode:T);
        }""",
      q"""case class L(node: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rNs: Set[N] = successorsAs(N, R)
              def rMs: Set[M] = successorsAs(M, R)
              def rs: Set[T] = Set.empty.++(rNs).++(rMs)
            }"""
    )
  }
  "accessors for super successor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait V; @Node trait T extends V;
            @Node class N extends T; @Node class M extends T
            @Node class L
            @Relation class R(startNode:L,endNode:V)
        }""",
      q"""case class L(node: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rNs: Set[N] = successorsAs(N, R)
              def rMs: Set[M] = successorsAs(M, R)
              def rs: Set[V] = Set.empty.++(rNs).++(rMs)
            };"""
    )
  }
  "accessors for predecessor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait T;
            @Node class N extends T; @Node class M extends T;
            @Node class L;
            @Relation class R(startNode:T,endNode:L);
        }""",
      q"""case class L(node: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[T] = Set.empty.++(rev_rNs).++(rev_rMs)
            };"""
    )
  }
  "accessors for super predecessor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait V; @Node trait T extends V;
            @Node class N extends T; @Node class M extends T;
            @Node class L;
            @Relation class R(startNode:V,endNode:L);
        }""",
      q"""case class L(node: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[V] = Set.empty.++(rev_rNs).++(rev_rMs)
            };"""
    )
  }
  "accessors for predecessor and successor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait T;
            @Node class N extends T; @Node class M extends T;
            @Node class L;
            @Relation class R(startNode:T,endNode:T);
        }""",
      q"""case class N(node: raw.Node) extends T {
              override val label = raw.Label("N")
              override val labels = Set(raw.Label("N"),raw.Label("T"))
              def rNs: Set[N] = successorsAs(N, R);
              def rMs: Set[M] = successorsAs(M, R);
              def rs: Set[T] = Set.empty.++(rNs).++(rMs);
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[T] = Set.empty.++(rev_rNs).++(rev_rMs)
            };"""
    )
  }
  "accessors for super predecessor and successor traits" >> {
    generatedContainsCode(
      q"""object A {@Node trait V; @Node trait T extends V;
            @Node class N extends T; @Node class M extends T;
            @Node class L;
            @Relation class R(startNode:V,endNode:V);
        }""",
      q"""case class N(node: raw.Node) extends T {
              override val label = raw.Label("N")
              override val labels = Set(raw.Label("N"), raw.Label("V"), raw.Label("T"))
              def rNs: Set[N] = successorsAs(N, R);
              def rMs: Set[M] = successorsAs(M, R);
              def rs: Set[V] = Set.empty.++(rNs).++(rMs);
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[V] = Set.empty.++(rev_rNs).++(rev_rMs)
            };"""
    )
  }
  "property accessors" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:Int}}",
      q"""def p: Int = node.properties("p").asInstanceOf[IntPropertyValue]""")
  }
}
