package codegeneration

import helpers.CodeComparisonSpec

class NodeTraitSpec extends CodeComparisonSpec {
   

  import contextMock.universe._

  "simple trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      """trait T extends Node  }"""
    )
  }
  "with super trait" >> {
    generatedContainsCode(
      q"object A { @Node trait K; @Node trait T extends K}",
      """trait T extends K  }"""
    )
  }
  "with multiple super traits" >> {
    generatedContainsCode(
      q"object A { @Node trait K;@Node trait L; @Node trait T extends K with L}",
      """trait T extends K with L  }"""
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
