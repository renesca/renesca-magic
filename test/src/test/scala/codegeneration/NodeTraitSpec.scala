package codegeneration

import org.specs2.mutable.Specification

class NodeTraitSpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      q"""trait T extends Node"""
    )
  }
  "with super trait" >> {
    generatedContainsCode(
      q"object A { @Node trait K; @Node trait T extends K}",
      q"""trait T extends K"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:Int}}",
      q"""trait T extends Node { def p: Int = node.properties("p").asInstanceOf[IntPropertyValue] }"""
    )
  }
  "custom code" >> {
    generatedContainsCode(
      q"object A {@Node trait T {def custom = 5}}",
      q"""trait T extends Node { def custom = 5 }"""
    )
  }
}
