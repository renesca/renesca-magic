package codegeneration

import helpers.CodeComparisonSpec

class NodeFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple node factory" >> {
    generatedContainsCode(
      q"object A {@Node class N}",
      q"""object N extends NodeFactory[N] {
              val label = raw.Label("N");
              val labels = Set(raw.Label("N"));
              def wrap(node: raw.Node) = new N(node);
              def create(): N = {
                val node = wrap(raw.Node.create(labels));
                node
              }
            }"""
    )
  }
  "with super factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      """object N extends TFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"))""",
      q"""def createT(): N = create()"""
    )
  }
  "with super factory with external superType" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T with E}",
      """object N extends TFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"))""",
      q"""def createT(): N = create()"""
    )
  }
  "with multiple super factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S; @Node class N extends T with S}",
      """object N extends TFactory[N] with SFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))""",
      q"""def createT(): N = create()""",
      q"""def createS(): N = create()"""
    )
  }
  "with multiple super factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      """object N extends SFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))""",
      q"""def createS(): N = create()"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:String; var x:Int}}",
      q"""def create(p: String, x: Int): N = {
            val node = wrap(raw.Node.create(labels));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          } """
    )
  }
  "with properties - parameter order of create" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N {
              var y:Option[Boolean]
              val q:Option[Double]
              var x:Int
              val p:String
            }
          }""",
      q"""def create(p: String, x: Int, q: Option[Double] = None, y: Option[Boolean] = None): N"""
    )
  }
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node class N extends T}",
      q"""def create(p: String, x: Int): N = {
            val node = wrap(raw.Node.create(labels));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }""",
      q""" def createT(p: String, x: Int): N = create(p, x) """
    )
  }
  "with inherited properties by two traits" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Int}; @Node class N extends T with S}",
      q"""def create(p: String, x: Int): N = {
            val node = wrap(raw.Node.create(labels));
            node.node.properties.update("p", p);
            node.node.properties.update("x", x);
            node
          }"""
    )
  }

  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T; @Node class N extends X}",
      q""" def createX(p: String, x: Int): N = create(p, x) """,
      q""" def createT(p: String, x: Int): NODE = createX(p, x) """
    )
  }
  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q""" def createX(p: String, x: Int, q: Boolean = true): N = create(p, x, q) """,
      q""" def createT(p: String, x: Int): NODE = createX(p, x, true) """
    )
  }
  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q""" def createX(p: String, x: Int, q: Option[Boolean] = None): N = create(p, x, q) """,
      q""" def createT(p: String, x: Int): NODE = createX(p, x, None) """
    )
  }
  "with indirectly inherited properties by two traits" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Int}; @Node trait X extends T with S; @Node class N extends X}",
      q""" def createX(p: String, x: Int): N = create(p, x) """,
      Not("def createS("),
      Not("def createT(")
    )
  }
  "diamond inheritance" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait L extends T; @Node trait R extends T; @Node trait X extends L with R; @Node class N extends X}",
      q""" def create(p: String): N"""
    )
  }
  // TODO one direct + one indirect
}
