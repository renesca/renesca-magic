package codegeneration

import org.specs2.mutable.Specification

class NodeTraitFactorySpec extends Specification with CodeComparison {
  

  import contextMock.universe._

  "simple factory trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] { def localT(): NODE }"""
    )
  }
  "with own factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      """object T extends RootNodeTraitFactory[T];"""
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
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] { def localX(p: String): NODE }"""
    )
  }

  "with multiple superType factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      q"""trait SFactory[NODE <: S] extends TFactory[NODE] {
            def localS(): NODE;
            def localT(): NODE = localS()
          };
      """
    )
  }
  "with superType factories with inherited local method" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node trait X extends T }",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def localX(p: String): NODE
            def localT(p: String): NODE = localX(p)
      }"""
    )
  }

  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def localT(p: String, x: Int): NODE
      }""",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def localX(p: String, x: Int, q: Boolean = true): NODE
            def localT(p: String, x: Int): NODE = localX(p, x, true)
      }"""
    )
  }
  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def localT(p: String, x: Int): NODE
      }""",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def localX(p: String, x: Int, q: Option[Boolean] = None): NODE
            def localT(p: String, x: Int): NODE = localX(p, x, None)
      }"""
    )
  }
}
