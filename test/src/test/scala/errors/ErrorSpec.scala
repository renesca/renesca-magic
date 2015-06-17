package errors

import helpers.CodeComparisonSpec

class ErrorSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "Node inherits Relation trait" >> {
    generate(
      q"object A {@Relation trait T; @Node class N extends T}"
    )
    1 mustEqual 1
    there was one(magic.aborter).abort("Nodes cannot inherit from Relation traits.")
  }
  //TODO: nonexistant, node trait extends relation trait
}
