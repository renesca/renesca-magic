package codegeneration

import helpers.CodeComparisonSpec

class RelationFactorySpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple relation factory" >> {
    generatedContainsCode(
      // TODO: fail with compile error when start or endNode does not exist
      q"object A {@Relation class R(startNode:A, endNode:B)}",
      q"""object R extends RelationFactory[A, R, B] with AbstractRelationFactory[A, R, B] {
            val relationType = raw.RelationType("R");
            def wrap(relation: raw.Relation) = R(A.wrap(relation.startNode), relation, B.wrap(relation.endNode));
            def create(startNode: A, endNode: B): R = {
              val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
              relation
            }
          } """
    )
  }
  "with super factory" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation class R(startNode:A, endNode:B) extends T}",
      """object R extends RelationFactory[A, R, B] with TFactory[A, R, B] { """,
      q"""def createT(startNode: A, endNode: B): R = create(startNode, endNode)"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Relation class R(startNode:A, endNode:B) {val p:String; var x:Int}}",
      q"""def create(startNode: A, endNode: B, p: String, x: Int): R = {
            val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
            relation.relation.properties.update("p", p);
            relation.relation.properties.update("x", x);
            relation
          } """
    )
  }
  "with properties - parameter order of create" >> {
    generatedContainsCode(
      q"""object A {
            @Relation class R(startNode:A, endNode:B) {
              var y:Option[Boolean]
              val q:Option[Double]
              var x:Int
              val p:String
            }
          }""",
      q"""def create(startNode: A, endNode: B, p: String, x: Int, q: Option[Double] = None, y: Option[Boolean] = None):R"""
    )
  }
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation class R(startNode:A, endNode:B) extends T}",
      q"""def create(startNode: A, endNode: B, p: String, x: Int): R = {
            val relation = wrap(raw.Relation.create(startNode.node, relationType, endNode.node));
            relation.relation.properties.update("p", p);
            relation.relation.properties.update("x", x);
            relation
          }""",
      q""" def createT(startNode: A, endNode: B, p: String, x: Int): R = create(startNode, endNode, p, x) """
    )
  }
  "with inherited properties from two traits" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}; @Relation trait S {var x:Int}; @Relation class R(startNode:A, endNode:B) extends T with S}",
      Not( """ def createT(startNode: A, endNode: B, p: String"""),
      Not( """ def createS(startNode: A, endNode: B, p: String""")
    )
  }
  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation trait X extends T; @Relation class R(startNode:A, endNode:B) extends X}",
      q""" def createT(startNode: START, endNode: END, p: String, x: Int): RELATION = createX(startNode, endNode, p, x)""",
      q""" def createX(startNode: A, endNode: B, p: String, x: Int): R = create(startNode, endNode, p, x)"""
    )
  }
  "with indirectly inherited properties in chain" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String;}; @Relation trait X extends T {var x:Int}; @Relation class R(startNode:A, endNode:B) extends X}",
      q""" def createX(startNode: A, endNode: B, p: String, x: Int): R = create(startNode, endNode, p, x)""",
      Not( """ def createT(startNode: A, endNode: B, p: String""")
    )
  }
}
