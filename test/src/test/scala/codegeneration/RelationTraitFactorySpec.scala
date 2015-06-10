package codegeneration

import org.specs2.mutable.Specification

class RelationTraitFactorySpec extends Specification with CodeComparison {
  // sequential 

  import contextMock.universe._

  "simple relation trait" >> {
    generatedContainsCode(
      q"object A {@Relation trait T}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END): RELATION
            }"""
    )
  }
  "with local interface" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def localT(startNode: START, endNode: END, p: String): RELATION
            };"""
    )
  }

  //TODO: "with own factory" >> { }

  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait X extends T}",
      q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TFactory[START, RELATION, END] {
              def localX(startNode: START, endNode: END): RELATION
            }"""
    )
  }
}
