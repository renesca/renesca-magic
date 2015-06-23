package codegeneration

import helpers.CodeComparisonSpec

class RelationClassSpec extends CodeComparisonSpec {
   

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B)}",
      """case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B]  }"""
    )
  }
  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B] { def custom = 0 }"""
    )
  }
  "with super types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation class R(startNode:A, endNode:B) extends T}",
      """case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B]  }"""
    )
  }
  "with multiple super types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait S; @Relation class R(startNode:A, endNode:B) extends T with S}",
      """case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B] with S[A, B] }"""
    )
  }
}
