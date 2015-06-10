package codegeneration

import org.specs2.mutable.Specification

class NodeFactorySpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

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
}
