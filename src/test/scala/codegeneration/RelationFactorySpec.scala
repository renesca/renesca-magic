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
          val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
          wrapped
        }
        def merge(startNode: A, endNode: B, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): R = {
          val wrapped = wrap(raw.Relation.merge(startNode.rawItem, relationType, endNode.rawItem, merge = merge, onMatch = onMatch));
          wrapped
        }
        def matches(startNode: A, endNode: B, matches: Set[PropertyKey] = Set.empty): R = {
          val wrapped = wrap(raw.Relation.matches(startNode.rawItem, relationType, endNode.rawItem, matches = matches));
          wrapped
        }
      } """
    )
  }.pendingUntilFixed

  "with super factory" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Node class A; @Node class B;@Relation class R(startNode:A, endNode:B) extends T}",
      """object R extends RelationFactory[A, R, B] with TFactory[A, R, B] { """,
      q"""def createT(startNode: A, endNode: B): R = this.create(startNode, endNode)"""
    )
  }
  "with properties" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B;@Relation class R(startNode:A, endNode:B) {val p:String; var x:Long}}",
      q"""def create(startNode: A, endNode: B, p: String, x: Long): R = {
            val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
            wrapped.rawItem.properties.update("p", p);
            wrapped.rawItem.properties.update("x", x);
            wrapped
          } """
    )
  }

  "with properties - parameter order of create" >> {
    generatedContainsCode(
      q"""object A {@Node class A; @Node class B
            @Relation class R(startNode:A, endNode:B) {
              var y:Option[Boolean]
              val q:Option[Double]
              var x:Long
              val p:String
            }
          }""",
      q"""def create(startNode: A, endNode: B, p: String, x: Long, q: Option[Double] = None, y: Option[Boolean] = None):R""",
      q"""def merge(startNode: A, endNode: B, p: String, x: Long, q: Option[Double] = None, y: Option[Boolean] = None, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty):R""",
      q"""def matches(startNode: A, endNode: B, p: Option[String] = None, q: Option[Double] = None, x: Option[Long] = None, y: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty):R"""
    )
  }

  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B;@Relation trait T {val p:String; var x:Long}; @Relation class R(startNode:A, endNode:B) extends T}",
      q"""def create(startNode: A, endNode: B, p: String, x: Long): R = {
            val wrapped = wrap(raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem));
            wrapped.rawItem.properties.update("p", p);
            wrapped.rawItem.properties.update("x", x);
            wrapped
          }""",
      q""" def createT(startNode: A, endNode: B, p: String, x: Long): R = this.create(startNode, endNode, p, x) """
    )
  }

  "with inherited properties from two traits" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B;@Relation trait T {val p:String}; @Relation trait S {var x:Long}; @Relation class R(startNode:A, endNode:B) extends T with S}",
      Not( """ def createT(startNode: A, endNode: B, p: String"""),
      Not( """ def createS(startNode: A, endNode: B, p: String""")
    )
  }

  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T {val p:String; var x:Long}; @Relation trait X extends T; @Relation class R(startNode:A, endNode:B) extends X}",
      q""" def createT(startNode: A, endNode: B, p: String, x: Long): R = this.create(startNode, endNode, p, x)""",
      q""" def createX(startNode: A, endNode: B, p: String, x: Long): R = this.create(startNode, endNode, p, x)"""
    )
  }

  "with indirectly inherited properties in chain" >> {
    generatedContainsCode(
      q"object A {@Node class A; @Node class B; @Relation trait T {val p:String;}; @Relation trait X extends T {var x:Long}; @Relation class R(startNode:A, endNode:B) extends X}",
      q""" def createX(startNode: A, endNode: B, p: String, x: Long): R = this.create(startNode, endNode, p, x)""",
      q""" def matchesT(startNode: A, endNode: B, p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): R = this.matches(startNode, endNode, p, None, matches)""",
      Not( """ def createT(startNode: A, endNode: B, p: String""")
    )
  }
}
