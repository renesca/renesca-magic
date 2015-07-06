package codegeneration

import helpers.CodeComparisonSpec

class HyperRelationClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple class, helper relations" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @HyperRelation class R(startNode:A, endNode:B)}",
      q"""case class R(rawItem: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] {
          override val label = raw.Label("R");
          override val labels = Set(raw.Label("R"))
      }""",
      """case class AToR(startNode: A, rawItem: raw.Relation, endNode: R) extends Relation[A, R];""",
      """case class RToB(startNode: R, rawItem: raw.Relation, endNode: B) extends Relation[R, B]  }"""
    )
  }

  "simple class with property accessors" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @HyperRelation class R(startNode:A, endNode:B) { val p: Long } }",
      q"""def p: Long = rawItem.properties("p").asInstanceOf[LongPropertyValue]"""
    )
  }

  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @HyperRelation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(rawItem: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] {
            override val label = raw.Label("R");
            override val labels = Set(raw.Label("R"))
            def custom = 0
          }"""
    )
  }

  "with super relation types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """case class R(rawItem: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B] {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"))
      }"""
    )
  }

  "with super node types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends K}",
      """case class R(rawItem: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with K {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"), raw.Label("K"))
      }"""
    )
  }

  "with super relation and node types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T; @Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends T with K}",
      """case class R(rawItem: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B] with K {
        override val label = raw.Label("R");
        override val labels = Set(raw.Label("R"), raw.Label("K"))
      }"""
    )
  }
}
