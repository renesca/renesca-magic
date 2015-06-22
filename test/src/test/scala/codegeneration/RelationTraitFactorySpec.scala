package codegeneration

import helpers.CodeComparisonSpec

class RelationTraitFactorySpec extends CodeComparisonSpec {
  

  import contextMock.universe._

  "simple relation trait" >> {
    generatedContainsCode(
      q"object A {@Relation trait T}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def createT(startNode: START, endNode: END): RELATION
      }"""
    )
  }
  "with create interface" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def createT(startNode: START, endNode: END, p: String): RELATION
      }"""
    )
  }

  //TODO: "with own factory" >> { }

  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait X extends T}",
      q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TFactory[START, RELATION, END] {
              def createX(startNode: START, endNode: END): RELATION
              def createT(startNode: START, endNode: END): RELATION = createX(startNode, endNode)
      }"""
    )
  }
}
