package codegeneration

import helpers.CodeComparisonSpec

class RelationTraitFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple relation trait" >> {
    generatedContainsCode(
      q"object A {@Relation trait T}",
      q"""trait TMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TMatchesFactory[START,RELATION,END] {
              def createT(startNode: START, endNode: END): RELATION
              def mergeT(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }

  "with factory interface" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:String}}",
      q"""trait TMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TMatchesFactory[START,RELATION,END] {
              def createT(startNode: START, endNode: END, p: String): RELATION
              def mergeT(startNode: START, endNode: END, p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }

  "without factory interface (only matches)" >> {
    generatedContainsCode(
      q"object A {@Node class N; @Relation trait T {val p:String}; @Relation class R(startNode: N, endNode: N) extends T { val t: String }}",
      q"""trait TMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] {
              def matchesT(startNode: START, endNode: END, p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q"""trait TFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends AbstractRelationFactory[START, RELATION, END] with TMatchesFactory[START,RELATION,END]"""
    )
  }

  //TODO: "with own factory" >> { }
  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait X extends T}",
      q"""trait XMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TMatchesFactory[START, RELATION, END] {
              def matchesX(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TFactory[START, RELATION, END] with XMatchesFactory[START,RELATION,END] {
              def createX(startNode: START, endNode: END): RELATION
              def mergeX(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }

  "with external superType" >> {
    generatedContainsCode(
      q"object A {@Relation trait T; @Relation trait X extends T with Immutable}",
      q"""trait XMatchesFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TMatchesFactory[START, RELATION, END] {
              def matchesX(startNode: START, endNode: END, matches: Set[PropertyKey] = Set.empty): RELATION
          }""",
      q"""trait XFactory[START <: Node, +RELATION <: AbstractRelation[START, END], END <: Node]
                extends TFactory[START, RELATION, END] with XMatchesFactory[START,RELATION,END] {
              def createX(startNode: START, endNode: END): RELATION
              def mergeX(startNode: START, endNode: END, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): RELATION
          }"""
    )
  }
}
