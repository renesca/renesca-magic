import org.specs2.mutable.Specification

class GenerationSpec extends Specification with CodeComparison {
  // sequential 

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
         def wrap(node: raw.Node) = {
           val factory = nodeLabelToFactory(node.labels.head).asInstanceOf[NodeFactory[NODE]]
           factory.wrap(node)
         }
       }
      } """)
  }


  "Properties" >> {
    "immutable property getter" >> {
      generatedContainsCode(
        q"object A {@Node class N {val p:String}}",
        q"""def p: String = node.properties("p").asInstanceOf[StringPropertyValue]""")
    }

    "optional immutable property getter" >> {
      generatedContainsCode(
        q"object A {@Node class N {val p:Option[String]}}",
        q"""def p:Option[String] = node.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""")
    }

    "mutable property getter and setter" >> {
      generatedContainsCode(
        q"object A {@Node class N {var p:String}}",
        q"""def p: String = node.properties("p").asInstanceOf[StringPropertyValue]""",
        q"""def `p_=`(newValue: String): scala.Unit = node.properties.update("p", newValue)"""
      )
    }

    "optional mutable property getter and setter" >> {
      generatedContainsCode(
        q"object A {@Node class N {var p:Option[String]}}",
        q"""def p:Option[String] = node.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""",
        q"""def `p_=`(newValue:Option[String]): scala.Unit = { if(newValue.isDefined) node.properties("p") = newValue.get else node.properties -= "p" }"""
      )
    }
  }


  "Node trait factory" >> {
    "simple factory trait" >> {
      generatedContainsCode(
        q"object A {@Node trait T}",
        q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] { def localT(): NODE }"""
      )
    }
    "with own factory" >> {
      generatedContainsCode(
        q"object A {@Node trait T; @Node class N extends T}",
        q"""object T extends RootNodeTraitFactory[T]"""
      )
    }
    //TODO: no own factory, when there is no node extending the trait
    "with local-interface" >> {
      generatedContainsCode(
        q"object A {@Node trait T {val p:String}}",
        q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] { def localT(p: String): NODE }"""
      )
    }
    "with superType factories" >> {
      generatedContainsCode(
        q"object A {@Node trait T ; @Node trait X extends T {val p:String} }",
        q"""trait XFactory[NODE <: X] extends NodeFactory[NODE] with TFactory[NODE] { def localX(p: String): NODE }"""
      )
    }
  }

  "relation trait factory" >> {
    "simple relation trait" >> {
      generatedContainsCode(
        q"object A {@Relation trait T}",
        q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END): RELATION
            }"""
      )
    }
    "with local interface" >> {
      generatedContainsCode(
        q"object A {@Relation trait T {val p:String}}",
        q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END, p: String): RELATION
            };"""
      )
    }

    //TODO: "with own factory" >> { }

    "with superType factories" >> {
      generatedContainsCode(
        q"object A {@Relation trait T; @Relation trait X extends T}",
        q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TFactory[START, RELATION, END] {
              def localX(startNode: START, endNode: END): RELATION
            }"""
      )
    }
  }

  "Node factory" >> {
    "simple node factory" >> {
      generatedContainsCode(
        q"object A {@Node class N}",
        q"""object N extends NodeFactory[N] {
              def wrap(node: raw.Node) = new N(node);
              val label = raw.Label("N");
              def local(): N = {
                val node = wrap(raw.Node.local(List(label)));
                node
              }
            }"""
      )
    }
    "with super factory" >> {
      generatedContainsCode(
        q"object A {@Node trait T; @Node class N extends T}",
        q"""object N extends TFactory[N] {
              def wrap(node: raw.Node) = new N(node);
              val label = raw.Label("N");
              def local(): N = {
                val node = wrap(raw.Node.local(List(label)));
                node
              };
              def localT(): N = local()
            } """
      )
    }
    "with properties" >> {
      generatedContainsCode(
        q"object A {@Node class N {val p:String; var x:Int}}",
        q"""object N extends NodeFactory[N] {
              def wrap(node: raw.Node) = new N(node);
              val label = raw.Label("N");
              def local(p: String, x: Int): N = {
                val node = wrap(raw.Node.local(List(label)));
                node.node.properties.update("p", p);
                node.node.properties.update("x", x);
                node
              }
            }"""
      )
    }
    "with inherited properties" >> {
      generatedContainsCode(
        q"object A {@Node trait T {val p:String; var x:Int}; @Node class N extends T}",
        q"""object N extends TFactory[N] {
              def wrap(node: raw.Node) = new N(node);
              val label = raw.Label("N");
              def local(p: String, x: Int): N = {
                val node = wrap(raw.Node.local(List(label)));
                node.node.properties.update("p", p);
                node.node.properties.update("x", x);
                node
              };
              def localT(p: String, x: Int): N = local(p, x)
            }; """
      )
    }
    "with indirectly inherited properties" >> {
      generatedContainsCode(
        q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T; @Node class N extends X}",
        q"""object N extends XFactory[N] {
              def wrap(node: raw.Node) = new N(node);
              val label = raw.Label("N");
              def local(p: String, x: Int): N = {
                val node = wrap(raw.Node.local(List(label)));
                node.node.properties.update("p", p);
                node.node.properties.update("x", x);
                node
              };
              def localX(p: String, x: Int): N = local(p, x)
            };"""
            //TOOD: concrete implementation of def localT?
      )
    }
    //TODO: inherit custom code
  }

  "Node class" >> {
    "simple class" >> {
      generatedContainsCode(
        q"object A {@Node class N}",
        q"""case class N(node: raw.Node) extends Node"""
      )
    }
    "preserve custom code" >> {
      generatedContainsCode(
        q"object A {@Node class N {def custom = 0}}",
        q"""def custom = 0"""
      )
    }
    "with super types" >> {
      generatedContainsCode(
        q"object A {@Node trait T; @Node class N extends T}",
        q"""case class N(node: raw.Node) extends T"""
      )
    }
    //TODO: which other supertype constellations can appear?
    "with external super types (no nodeTraits)" >> {
      generatedContainsCode(
        q"object A {@Node trait T; @Node class N extends T with Ext}",
        q"""case class N(node: raw.Node) extends T with Ext"""
      )
    }
    "direct neighbour accessors" >> {
      generatedContainsCode(
        q"object A {@Node class N; @Node class M; @Relation class R(startNode:N,endNode:M)}",
        q"""case class N(node: raw.Node) extends Node {
              def rs: Set[M] = successorsAs(M, R)
            };""",
        q"""case class M(node: raw.Node) extends Node {
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
              def rNs: Set[N] = successorsAs(N, R);
              def rMs: Set[M] = successorsAs(M, R)
              def rs: Set[T] = Set.empty.++(rNs).++(rMs);
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
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[T] = Set.empty.++(rev_rNs).++(rev_rMs)
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
              def rNs: Set[N] = successorsAs(N, R);
              def rMs: Set[M] = successorsAs(M, R);
              def rs: Set[T] = Set.empty.++(rNs).++(rMs);
              def rev_rNs: Set[N] = predecessorsAs(N, R);
              def rev_rMs: Set[M] = predecessorsAs(M, R);
              def rev_rs: Set[T] = Set.empty.++(rev_rNs).++(rev_rMs)
            };"""
      )
    }
    //TODO: trait accessors with inheritance
  }

}
