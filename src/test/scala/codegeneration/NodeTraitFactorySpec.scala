package codegeneration

import helpers.CodeComparisonSpec

class NodeTraitFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple factory trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(matches: Set[PropertyKey] = Set.empty): NODE
            def createT(): NODE
            def mergeT(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
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
          trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createT(p: String): NODE
            def mergeT(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }

  "without factory interface (only matches)" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node class N extends T { val t: String }}",
      q"""
          trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }

  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T ; @Node trait X extends T {val p:String} }",
      q"""
          trait XFactory[+NODE <: X] extends TFactory[NODE] {
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createX(p: String): NODE
            def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""def matchesT(matches: Set[PropertyKey] = Set.empty): NODE;"""
    )
  }

  "with multiple superType factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      q"""trait SFactory[+NODE <: S] extends TFactory[NODE] {
            def matchesS(matches: Set[PropertyKey] = Set.empty): NODE
            def createS(): NODE
            def mergeS(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          };
      """,
      q"""def matchesT(matches: Set[PropertyKey] = Set.empty): N = this.matches(matches)""",
      q"""def createT(): N = this.create()""",
      q"""def mergeT(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N = this.merge(merge, onMatch)"""
    )
  }

  "with superType factories with inherited factory methods" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node trait X extends T; @Node class N extends X }",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] {
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createX(p: String): NODE
            def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): N = this.matches(p, matches)""",
      q"""def createT(p: String): N = this.create(p)""",
      q"""def mergeT(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N = this.merge(p, merge, onMatch)""",
      q"""def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): N = this.matches(p, matches)""",
      q"""def createX(p: String): N = this.create(p)""",
      q"""def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N = this.merge(p, merge, onMatch)"""
    )
  }

  "with indirectly inherited properties and no factory trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean }; @Node class N extends X}",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] {
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE;
            def createX(p: String, q: Boolean, x: Int): NODE;
            def mergeX(p: String, q: Boolean, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE;
          }""",
      q"""def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): N = this.matches(p, None, x, matches)"""
    )
  }

  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] {
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE;
            def createX(p: String, x: Int, q: Boolean = true): NODE;
            def mergeX(p: String, x: Int, q: Boolean = true, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE;
          }"""
    )
  }

  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] {
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
            def createX(p: String, x: Int, q: Option[Boolean] = None): NODE
            def mergeX(p: String, x: Int, q: Option[Boolean] = None, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): N = this.matches(p, None, x, matches)""",
      q"""def createT(p: String, x: Int): N = this.create(p, x, None)""",
      q"""def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N = this.merge(p, x, None, merge, onMatch)"""
    )
  }

  "no factory methods if HyperRelation inherits from NodeTrait" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @HyperRelation class X(startNode: Node, endNode: Node) extends T}",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE];"""
    )
  }
}
