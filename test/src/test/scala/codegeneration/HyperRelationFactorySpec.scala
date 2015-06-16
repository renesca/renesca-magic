package codegeneration

import org.specs2.mutable.Specification

class HyperRelationFactorySpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple hyperrelation factory" >> {
    generatedContainsCode(
      // TODO: fail with compile error when start or endNode does not exist
      q"object A {@HyperRelation class R(startNode:A, endNode:B)}",
      q"""object R extends HyperRelationFactory[A, AToR, R, RToB, B] {
            override def label = raw.Label("R");
            override def startRelationType = raw.RelationType("ATOR");
            override def endRelationType = raw.RelationType("RTOB");
            override def wrap(node: raw.Node) = new R(node);
            override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation) = {
              val hyperRelation = wrap(middleNode);
              hyperRelation._startRelation = AToR(A.wrap(startRelation.startNode), startRelation, hyperRelation);
              hyperRelation._endRelation = RToB(hyperRelation, endRelation, B.wrap(endRelation.endNode));
              hyperRelation
            };
            def local(startNode: A, endNode: B): R = {
              val middleNode = raw.Node.local(List(label));
              wrap(raw.Relation.local(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.local(middleNode, endRelationType, endNode.node))
            }
          } """
    )
  }
  "with node super factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """not implemented"""
    )
  }.pendingUntilFixed("does it make sense to inherit from node trait factories?")

  "with relation super factory" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      """object R extends TFactory[A, R, B] {""",
      q"""def localT(startNode: A, endNode: B): R = local(startNode, endNode)"""
    )
  }

  "with properties" >> {
    generatedContainsCode(
      q"object A {@HyperRelation class R(startNode:A, endNode:B) {val p:String; var x:Int}}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.local(List(label));
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.local(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.local(middleNode, endRelationType, endNode.node))
          }"""
    )
  }
  //TODO abort with error when inheriting from myself. Right now this produces a stack overflow error 
  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @HyperRelation class R(startNode:A, endNode:B) extends T}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.local(List(label));
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.local(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.local(middleNode, endRelationType, endNode.node))
          }""",
      q"""def localT(startNode: A, endNode: B, p: String, x: Int): R = local(startNode, endNode, p, x)"""
    )
  }
  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String; var x:Int}; @Relation trait X extends T; @HyperRelation class R(startNode:A, endNode:B) extends X}",
      q"""def local(startNode: A, endNode: B, p: String, x: Int): R = {
            val middleNode = raw.Node.local(List(label));
            middleNode.properties.update("p", p);
            middleNode.properties.update("x", x);
            wrap(raw.Relation.local(startNode.node, startRelationType, middleNode), middleNode, raw.Relation.local(middleNode, endRelationType, endNode.node))
          }""",
      q"""def localX(startNode: A, endNode: B, p: String, x: Int): R = local(startNode, endNode, p, x)"""
    )
  }
}
