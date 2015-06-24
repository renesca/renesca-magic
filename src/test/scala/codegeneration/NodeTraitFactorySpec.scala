package codegeneration

import helpers.CodeComparisonSpec

class NodeTraitFactorySpec extends CodeComparisonSpec {


  import contextMock.universe._

  "simple factory trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def createT(): NODE
            def mergeT(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesT(matches: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }
  "with own factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] {
            val label = raw.Label("T")
            val labels = Set(raw.Label("T"))
          }"""
    )
  }
  "with own factory with superTraits" >> {
    generatedContainsCode(
      q"object A {@Node trait S; @Node trait T extends S; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] {
            val label = raw.Label("T")
            val labels = Set(raw.Label("T"), raw.Label("S"))
          }"""
    )
  }
  "with own factory with superTraits and external traits" >> {
    generatedContainsCode(
      q"object A {@Node trait S; @Node trait T extends S; @Node class N extends T with E}",
      q"""object T extends RootNodeTraitFactory[T] {
            val label = raw.Label("T")
            val labels = Set(raw.Label("T"), raw.Label("S"))
          }"""
    )
  }
  "with own factory with multiple superTraits" >> {
    generatedContainsCode(
      q"object A {@Node trait S;@Node trait X; @Node trait T extends S with X; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] {
            val label = raw.Label("T")
            val labels = Set(raw.Label("T"), raw.Label("S"), raw.Label("X"))
          }"""
    )
  }
  "with own factory with multiple superTraits (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait Y;@Node trait S;@Node trait X extends Y; @Node trait T extends S with X; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] {
            val label = raw.Label("T")
            val labels = Set(raw.Label("T"), raw.Label("Y"), raw.Label("S"), raw.Label("X"))
          }"""
    )
  }
  //TODO: no own factory, when there is no node extending the trait
  "with factory interface" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}}",
      q"""
          trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def createT(p: String): NODE
            def mergeT(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }
  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T ; @Node trait X extends T {val p:String} }",
      q"""
          trait XFactory[NODE <: X] extends TFactory[NODE] {
            def createX(p: String): NODE
            def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }

  "with multiple superType factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      q"""trait SFactory[NODE <: S] extends TFactory[NODE] {
            def createS(): NODE
            def mergeS(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesS(matches: Set[PropertyKey] = Set.empty): NODE
            def createT(): NODE = this.createS()
            def mergeT(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE = this.mergeS(merge, onMatch)
            def matchesT(matches: Set[PropertyKey] = Set.empty): NODE = this.matchesS(matches)
          };
      """
    )
  }
  "with superType factories with inherited factory methods" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node trait X extends T }",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def createX(p: String): NODE
            def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createT(p: String): NODE = this.createX(p)
            def mergeT(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE = this.mergeX(p, merge, onMatch)
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE = this.matchesX(p, matches)
          }"""
    )
  }

  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def createX(p: String, x: Int, q: Boolean = true): NODE;
            def mergeX(p: String, x: Int, q: Boolean = true, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE;
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE;
            def createT(p: String, x: Int): NODE = this.createX(p, x, true);
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE = this.mergeX(p, x, true, merge, onMatch);
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE = this.matchesX(p, None, x, matches)
          }"""
    )
  }
  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q"""trait TFactory[NODE <: T] extends NodeFactory[NODE] {
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[NODE <: X] extends TFactory[NODE] {
            def createX(p: String, x: Int, q: Option[Boolean] = None): NODE
            def mergeX(p: String, x: Int, q: Option[Boolean] = None, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createT(p: String, x: Int): NODE = this.createX(p, x, None)
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE = this.mergeX(p, x, None, merge, onMatch)
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE = this.matchesX(p, None, x, matches)
          }"""
    )
  }
}
