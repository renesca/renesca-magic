package codegeneration

import org.specs2.mutable.Specification

class RelationFactorySpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple relation factory" >> {
    generatedContainsCode(
      // TODO: fail with compile error when start or endNode does not exist
      q"object A {@Relation class R(startNode:A, endNode:B)}",
      q"""object R extends RelationFactory[A, R, B] with AbstractRelationFactory[A, R, B] {
            def relationType = raw.RelationType("R");
            def wrap(relation: raw.Relation) = R(A.wrap(relation.startNode), relation, B.wrap(relation.endNode));
            def local(startNode: A, endNode: B): R = {
              val relation = wrap(raw.Relation.local(startNode.node, relationType, endNode.node));
              relation
            }
          } """
    )
  }
  "with super factory" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation class R(startNode:A, endNode:B) extends T}",
      q"""object R extends RelationFactory[A, R, B] with TFactory[A, R, B] """,
      q"""def localT(startNode: A, endNode: B): R = local(startNode, endNode)"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B) {val p:String; var x:Int}}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val relation = wrap(raw.Relation.local(startNode.node, relationType, endNode.node));
            relation.relation.properties.update("p", p);
            relation.relation.properties.update("x", x);
            relation
          } """
    )
  }
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation class R(startNode:A, endNode:B) extends T}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val relation = wrap(raw.Relation.local(startNode.node, relationType, endNode.node));
            relation.relation.properties.update("p", p);
            relation.relation.properties.update("x", x);
            relation
          }""",
      q""" def localT(p: String, x: Int): R = local(p, x) """
    )
  }
  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation trait X extends T; @Relation class R(startNode:A, endNode:B) extends T}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val relation = wrap(raw.Relation.local(startNode.node, relationType, endNode.node));
            relation.relation.properties.update("p", p);
            relation.relation.properties.update("x", x);
            relation
          } """,
      q""" def localT(p: String, x: Int): R = local(p, x)"""
      //TOOD: concrete implementation of def localT?
    )
  }
}
