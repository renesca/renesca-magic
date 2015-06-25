package codegeneration

import helpers.CodeComparisonSpec

class RelationTraitFactorySpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple relation trait" >> {
    generatedContainsCode(
      q"object A {@Relation trait T}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION
              def createT(startNode: START, endNode: END): RELATION
              def mergeT(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }

  "with factory interface" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): RELATION
              def createT(startNode: START, endNode: END, p: String): RELATION
              def mergeT(startNode: START, endNode: END, p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }

  "without factory interface (only matches)" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}; @Relation class R(startNode: Node, endNode: Node) extends T { val t: String }}",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q""
    )
  }

  //TODO: "with own factory" >> { }

  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait X extends T}",
      q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TFactory[START, RELATION, END] {
              def matchesX(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION
              def createX(startNode: START, endNode: END): RELATION
              def mergeX(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
              def matchesT(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION = this.matchesX(startNode, endNode, matches)
              def createT(startNode: START, endNode: END): RELATION = this.createX(startNode, endNode)
              def mergeT(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION = this.mergeX(startNode, endNode, merge, onMatch)
          }"""
    )
  }
}
