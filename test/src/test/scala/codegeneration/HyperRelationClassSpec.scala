package codegeneration

import org.specs2.mutable.Specification

class HyperRelationClassSpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple class, helper relations" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B)}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B];""",
      """case class AToR(startNode: A, relation: raw.Relation, endNode: R) extends Relation[A, R];""",
      """case class RToB(startNode: R, relation: raw.Relation, endNode: B) extends Relation[R, B]  }"""
    )
  }
  "preserve custom code" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B) {def custom = 0}}",
      q"""case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] {def custom = 0}"""
    )
  }
  "with super relation types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B];"""
    )
  }
  "with super node types" >> {
    generatedContainsCode(
      q"object A {@Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends K}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with K;"""
    )
  }
  "with super relation and node types" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Node trait K; @HyperRelation class R(startNode:A, endNode:B) extends T with K}",
      """case class R(node: raw.Node) extends HyperRelation[A, AToR, R, RToB, B] with T[A, B] with K;"""
    )
  }
}
