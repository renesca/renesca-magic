package codegeneration

import helpers.CodeComparisonSpec

class RelationTraitSpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple trait" >> {
    generatedContainsCode(
      q"object A {@Relation trait T}",
      """trait T[+START <: Node, +END <: Node] extends AbstractRelation[START, END]  ;"""
    )
  }

  "with super trait" >> {
    generatedContainsCode(
      q"object A { @Relation trait K; @Relation trait T extends K}",
      """trait T[+START <: Node, +END <: Node] extends K[START, END]  ;"""
    )
  }

  "with external super trait" >> {
    generatedContainsCode(
      q"object A { @Relation trait K; @Relation trait T extends K with Immutable}",
      """trait T[+START <: Node, +END <: Node] extends K[START, END] with Immutable;"""
    )
  }

  "with properties" >> {
    generatedContainsCode(
      q"object A {@Relation trait T {val p:Long}}",
      q"""trait T[+START <: Node, +END <: Node] extends AbstractRelation[START, END] {
            def p: Long = rawItem.properties("p").asInstanceOf[LongPropertyValue]
          }"""
    )
  }

  "custom code" >> {
    generatedContainsCode(
      q"object A {@Node trait T {def custom = 5}}",
      q"""trait T extends Node { def custom = 5 }"""
    )
  }
}
