package codegeneration

import helpers.CodeComparisonSpec

class NodeClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Node class N}",
      q"""case class N(rawItem: raw.Node) extends Node {
        override val label = raw.Label("N")
        override val labels = Set(raw.Label("N"))
      }"""
    )
  }

  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Node class N {def custom = 0}}",
      q""" case class N(rawItem: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
            def custom = 0
          } """)
  }

  "with super types" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      q"""case class N(rawItem: raw.Node) extends T  {
          override val label = raw.Label("N")
          override val labels = Set(raw.Label("N"), raw.Label("T"))
        }"""
    )
  }

  "with multiple super types" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S; @Node class N extends T with S}",
      q"""case class N(rawItem: raw.Node) extends T with S  {
          override val label = raw.Label("N")
          override val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))
         }
      """
    )
  }

  //TODO: which other supertype constellations can appear?
  "with external super types (no nodeTraits)" >> {
    generatedContainsCode(
      q"object A { @Node trait T; @Node class N extends T with Immutable}",
      q"""case class N(rawItem: raw.Node) extends T with Immutable  {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"), raw.Label("T"))
          }
      """
    )
  }

  "direct neighbour accessors" >> {
    generatedContainsCode(
      q"object A {@Node class N; @Node class M; @Relation class R(startNode:N,endNode:M)}",
      q"""case class N(rawItem: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
              def rs: Seq[M] = successorsAs(M, R)
            };""",
      q"""case class M(rawItem: raw.Node) extends Node {
            override val label = raw.Label("M")
            override val labels = Set(raw.Label("M"))
              def rev_rs: Seq[N] = predecessorsAs(N, R)
            };"""
    )
  }

  "direct neighbour accessors over hyperrelations" >> {
    generatedContainsCode(
      q"object A {@Node class N; @Node class M; @HyperRelation class R(startNode:N,endNode:M)}",
      q"""case class N(rawItem: raw.Node) extends Node {
            override val label = raw.Label("N")
            override val labels = Set(raw.Label("N"))
              def rs: Seq[M] = successorsAs(M, R)
            };""",
      q"""case class M(rawItem: raw.Node) extends Node {
            override val label = raw.Label("M")
            override val labels = Set(raw.Label("M"))
              def rev_rs: Seq[N] = predecessorsAs(N, R)
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
      q"""case class L(rawItem: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rNs: Seq[N] = successorsAs(N, R)
              def rMs: Seq[M] = successorsAs(M, R)
              def rs: Seq[T] = Seq.empty.++(rNs).++(rMs)
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
      q"""case class L(rawItem: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rNs: Seq[N] = successorsAs(N, R)
              def rMs: Seq[M] = successorsAs(M, R)
              def rs: Seq[V] = Seq.empty.++(rNs).++(rMs)
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
      q"""case class L(rawItem: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rev_rNs: Seq[N] = predecessorsAs(N, R);
              def rev_rMs: Seq[M] = predecessorsAs(M, R);
              def rev_rs: Seq[T] = Seq.empty.++(rev_rNs).++(rev_rMs)
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
      q"""case class L(rawItem: raw.Node) extends Node {
              override val label = raw.Label("L")
              override val labels = Set(raw.Label("L"))
              def rev_rNs: Seq[N] = predecessorsAs(N, R);
              def rev_rMs: Seq[M] = predecessorsAs(M, R);
              def rev_rs: Seq[V] = Seq.empty.++(rev_rNs).++(rev_rMs)
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
      q"""case class N(rawItem: raw.Node) extends T {
              override val label = raw.Label("N")
              override val labels = Set(raw.Label("N"),raw.Label("T"))
              def rNs: Seq[N] = successorsAs(N, R);
              def rMs: Seq[M] = successorsAs(M, R);
              def rs: Seq[T] = Seq.empty.++(rNs).++(rMs);
              def rev_rNs: Seq[N] = predecessorsAs(N, R);
              def rev_rMs: Seq[M] = predecessorsAs(M, R);
              def rev_rs: Seq[T] = Seq.empty.++(rev_rNs).++(rev_rMs)
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
      q"""case class N(rawItem: raw.Node) extends T {
              override val label = raw.Label("N")
              override val labels = Set(raw.Label("N"), raw.Label("V"), raw.Label("T"))
              def rNs: Seq[N] = successorsAs(N, R);
              def rMs: Seq[M] = successorsAs(M, R);
              def rs: Seq[V] = Seq.empty.++(rNs).++(rMs);
              def rev_rNs: Seq[N] = predecessorsAs(N, R);
              def rev_rMs: Seq[M] = predecessorsAs(M, R);
              def rev_rs: Seq[V] = Seq.empty.++(rev_rNs).++(rev_rMs)
            };"""
    )
  }

  "property accessors" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:Long}}",
      q"""def p: Long = rawItem.properties("p").asInstanceOf[LongPropertyValue]""")
  }
}
