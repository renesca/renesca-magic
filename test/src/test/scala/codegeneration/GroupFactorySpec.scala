package codegeneration

import helpers.CodeComparisonSpec


class GroupFactorySpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple group" >> {
    generatedContainsCode(
      q"object A {@Group trait G}",
      q"""object G { def empty = new G(raw.Graph.empty) }"""
    )
  }
}
