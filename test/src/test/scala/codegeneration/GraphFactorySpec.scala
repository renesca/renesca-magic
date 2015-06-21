package codegeneration

import helpers.CodeComparisonSpec


class GraphFactorySpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple Graph" >> {
    generatedContainsCode(
      q"object A {@Graph trait G}",
      q"""object G { def empty = new G(raw.Graph.empty) }"""
    )
  }
}
