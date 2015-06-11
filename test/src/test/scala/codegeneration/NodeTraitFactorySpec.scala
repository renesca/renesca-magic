package codegeneration

import org.specs2.mutable.Specification

class NodeTraitFactorySpec extends Specification with CodeComparison {
  // sequential

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
  "with superType factories with inherited local method" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node trait X extends T }",
      q"""trait XFactory[NODE <: X] extends NodeFactory[NODE] with TFactory[NODE] {
            def localX(p: String): NODE
            def localT(p: String): NODE = localX(p)
      }"""
    )
  }
}
