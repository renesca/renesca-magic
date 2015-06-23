package codegeneration

import helpers.CodeComparisonSpec

class HyperRelationFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple hyperrelation factory" >> {
    generatedContainsCode(
      // TODO: fail with compile error when start or endNode does not exist
      q"object A {@HyperRelation class R(startNode:A, endNode:B)}",
      q"""object R extends HyperRelationFactory[A, AToR, R, RToB, B] {
            override val label = raw.Label("R");
            override val labels = Set(raw.Label("R"))
            override val startRelationType = raw.RelationType("ATOR");
            override val endRelationType = raw.RelationType("RTOB");
            override def wrap(node: raw.Node) = new R(node);
            override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation) = {
              val hyperRelation = wrap(middleNode);
              hyperRelation._startRelation = AToR(A.wrap(startRelation.startNode), startRelation, hyperRelation);
              hyperRelation._endRelation = RToB(hyperRelation, endRelation, B.wrap(endRelation.endNode));
              hyperRelation
            };
            def create(startNode: A, endNode: B): R = {
              val middleNode = raw.Node.create(labels);
              wrap(raw.Relation.create(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.node))
            }
            def merge(startNode: A, endNode: B, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): R = {
              val middleNode = raw.Node.merge(labels, merge = merge, onMatch = onMatch);
              wrap(raw.Relation.merge(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.merge(middleNode, endRelationType, endNode.node))
            }
            def matches(startNode: A, endNode: B, matches: Set[PropertyKey] = Set.empty): R = {
              val middleNode = raw.Node.matches(labels, matches = matches);
              wrap(raw.Relation.matches(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.matches(middleNode, endRelationType, endNode.node))
            }
          } """
    )
  }
  "with node super factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """object R extends HyperRelationFactory[A, AToR, R, RToB, B] with TFactory[R] {"""
    )
  }

  "with relation super factory" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """object R extends HyperRelationFactory[A, AToR, R, RToB, B] with TFactory[A, R, B] {""",
      q"""def createT(startNode: A, endNode: B): R = create(startNode, endNode)"""
    )
  }

  "with properties" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B) {val p:String; var x:Int}}",
      q"""def create(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.create(labels);
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.create(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.node))
          }""",
      q"""def merge(startNode: A, endNode: B, p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): R = {
            val middleNode = raw.Node.merge(labels, merge = merge, onMatch = onMatch);
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.merge(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.merge(middleNode, endRelationType, endNode.node))
          }""",
      q"""def matches(startNode: A, endNode: B, p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): R = {
            val middleNode = raw.Node.matches(labels, matches = matches);
            if (p.isDefined)
              middleNode.properties.update("p", p.get)
            else
              ();
            if (x.isDefined)
              middleNode.properties.update("x", x.get)
            else
              ();
            wrap(raw.Relation.matches(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.matches(middleNode, endRelationType, endNode.node))
          }"""
    )
  }
  //TODO abort with error when inheriting from myself. Right now this produces a stack overflow error 
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      q"""def create(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.create(labels);
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.create(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.node))
          }""",
      q"""def createT(startNode: A, endNode: B, p: String, x: Int): R = create(startNode, endNode, p, x)"""
    )
  }
  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation trait X extends T; @HyperRelation class R(startNode:A, endNode:B) extends X}",
      q"""def create(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.create(labels);
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.create(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.create(middleNode, endRelationType, endNode.node))
          }""",
      q"""def createX(startNode: A, endNode: B, p: String, x: Int): R = create(startNode, endNode, p, x)"""
    )
  }
}