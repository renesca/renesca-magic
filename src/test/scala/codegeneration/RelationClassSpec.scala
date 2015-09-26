package codegeneration

import helpers.CodeComparisonSpec

class RelationClassSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation class R(startNode:A, endNode:B)}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B];"""
    )
  }

  "simple class with property accessors" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation class R(startNode:A, endNode:B) {val p:Long}}",
      q"""def p: Long = rawItem.properties("p").asInstanceOf[LongPropertyValue]"""
    )
  }

  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] { def custom = 0 } ;"""
    )
  }

  "with super types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T; @Relation class R(startNode:A, endNode:B) extends T}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B]  ;"""
    )
  }

  "with external super types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T; @Relation class R(startNode:A, endNode:B) extends T with Immutable}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B] with Immutable;"""
    )
  }

  "with multiple super types" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T; @Relation trait S; @Relation class R(startNode:A, endNode:B) extends T with S}",
      """case class R(startNode: A, rawItem: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B] with S[A, B] ;"""
    )
  }
}
