package codegeneration

import org.specs2.mutable.Specification

class RelationClassSpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple class" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B)}",
      q"""case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B]"""
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
      q"""case class R(startNode: A, relation: raw.Relation, endNode: B) extends Relation[A, B] with T[A, B]"""
    )
  }
}
