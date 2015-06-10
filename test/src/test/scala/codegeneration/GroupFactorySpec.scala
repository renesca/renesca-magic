package codegeneration

import org.specs2.mutable.Specification

class GroupFactorySpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple group" >> {
    generatedContainsCode(
      q"object A {@Group trait G}",
      q"""object G { def empty = new G(raw.Graph.empty) }"""
    )
  }
}
