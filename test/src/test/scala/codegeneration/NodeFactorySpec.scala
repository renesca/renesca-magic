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
      q"""object N extends TFactory[N] """,
      q"""def localT(): N = local()"""
    )
  }
  "with multiple super factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S; @Node class N extends T with S}",
      q"""object N extends TFactory[N] with SFactory[N]""",
      q"""def localT(): N = local()""",
      q"""def localS(): N = local()"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:String; var x:Int}}",
      q"""def local(p: String, x: Int): N = {
            val node = wrap(raw.Node.local(List(label)));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          } """
    )
  }
  "with properties - parameter order of local" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N {
              var y:Option[Boolean]
              val q:Option[Double]
              var x:Int
              val p:String
            }
          }""",
      q"""def local(p: String, x: Int, q: Option[Double] = None, y: Option[Boolean] = None): N"""
    )
  }
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node class N extends T}",
      q"""def local(p: String, x: Int): N = {
            val node = wrap(raw.Node.local(List(label)));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }""",
      q""" def localT(p: String, x: Int): N = local(p, x) """
    )
  }
  "with inherited properties by two traits" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Int}; @Node class N extends T with S}",
      q"""def local(p: String, x: Int): N = {
            val node = wrap(raw.Node.local(List(label)));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }""",
      q""" def localT(p: String, x: Int): N = local(p, x) """
    )
  }.pendingUntilFixed("does localT/localS make sense here?")
  //TODO: different local/localT
  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T; @Node class N extends X}",
      q"""def local(p: String, x: Int): N = {
            val node = wrap(raw.Node.local(List(label)));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }""",
      q""" def localX(p: String, x: Int): N = local(p, x) """
      //TOOD: why no localT?
    )
  }
  "with indirectly inherited properties by two traits" >> {
    generatedContainsCodePrint(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Int}; @Node trait X extends T with S; @Node class N extends X}",
      q"""def local(p: String, x: Int): N = {
            val node = wrap(raw.Node.local(List(label)));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }""",
      q""" def localX(p: String, x: Int): N = local(p, x) """
      //TOOD: why no localT?
    )
  }.pendingUntilFixed("does localT/localS make sense here?")
  // TODO one direct + one indirect
}
