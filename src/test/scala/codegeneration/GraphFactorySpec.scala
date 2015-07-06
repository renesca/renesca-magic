package codegeneration

import helpers.CodeComparisonSpec


class GraphFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple Graph" >> {
    generatedContainsCode(
    q"""
        object A {
          @Graph trait G
        }
        """,
    q"""
          object G {
            def empty = new G(raw.Graph.empty);
            def apply(items: Item*) = {
              val wrapper = empty;
              wrapper.add(((items): _*));
              wrapper
            }
          };
          """
    )
  }
}
