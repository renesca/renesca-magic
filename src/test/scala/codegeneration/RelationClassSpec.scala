package codegeneration

import helpers.CodeComparisonSpec

class RelationClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B)}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B]  }"""
    )
  }

  "simple class with property accessors" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B) {val p:Int} }",
      q"""def p: Int = rawItem.properties("p").asInstanceOf[IntPropertyValue]"""
    )
  }

  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] { def custom = 0 }"""
    )
  }

  "with super types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation class R(startNode:A, endNode:B) extends T}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B]  }"""
    )
  }

  "with multiple super types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait S; @Relation class R(startNode:A, endNode:B) extends T with S}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B] with S[A, B] }"""
    )
  }
}
