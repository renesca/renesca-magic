package codegeneration

import helpers.CodeComparisonSpec

class HyperRelationClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple class, helper relations" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B)}",
      q"""case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] {
          override val label = raw.Label("R");
          override val labels = Set(raw.Label("R"))
      }""",
      """case class AToR(startNode: A, relation: raw.Relation, endNode: R) extends Relation[A, R];""",
      """case class RToB(startNode: R, relation: raw.Relation, endNode: B) extends Relation[R, B]  }"""
    )
  }

  "simple class with property accessors" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B) { val p: Int } }",
      q"""def p: Int = item.properties("p").asInstanceOf[IntPropertyValue]"""
    )
  }

  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] {
            override val label = raw.Label("R");
            override val labels = Set(raw.Label("R"))
            def custom = 0
          }"""
    )
  }

  "with super relation types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B] {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"))
      }"""
    )
  }

  "with super node types" >> {
    generatedContainsCode(
      q"object A {@Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends K}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with K {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"), raw.Label("K"))
      }"""
    )
  }

  "with super relation and node types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends T with K}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B] with K {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"), raw.Label("K"))
      }"""
    )
  }
}
