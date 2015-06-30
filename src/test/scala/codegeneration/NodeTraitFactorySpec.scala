package codegeneration

import helpers.CodeComparisonSpec

class NodeTraitFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple factory trait" >> {
    generatedContainsCode(
      q"object A {@Node trait T}",
      q"""trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE] {
            def createT(): NODE
            def mergeT(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }

  "with own factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] with TMatchesFactory[T] {
            val label = TMatches.label
            val labels = TMatches.labels
            def matches(matches: Set[PropertyKey] = Set.empty) = TMatches.matches(matches)
            def matchesT(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
          }"""
    )
  }

  "with own factory with superTraits" >> {
    generatedContainsCode(
      q"object A {@Node trait S; @Node trait T extends S; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] with TMatchesFactory[T] {
            val label = TMatches.label
            val labels = TMatches.labels
            def matches(matches: Set[PropertyKey] = Set.empty) = TMatches.matches(matches)
            def matchesT(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
            def matchesS(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
          }"""
    )
  }

  "with own factory with superTraits and external traits" >> {
    generatedContainsCode(
      q"object A {@Node trait S; @Node trait T extends S; @Node class N extends T with E}",
      q"""object T extends RootNodeTraitFactory[T] with TMatchesFactory[T] {
            val label = TMatches.label
            val labels = TMatches.labels
            def matches(matches: Set[PropertyKey] = Set.empty) = TMatches.matches(matches)
            def matchesT(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
            def matchesS(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
          }"""
    )
  }

  "with own factory with multiple superTraits" >> {
    generatedContainsCode(
      q"object A {@Node trait S;@Node trait X; @Node trait T extends S with X; @Node class N extends T}",
      q"""object T extends RootNodeTraitFactory[T] with TMatchesFactory[T] {
            val label = TMatches.label
            val labels = TMatches.labels
            def matches(matches: Set[PropertyKey] = Set.empty) = TMatches.matches(matches)
            def matchesT(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
            def matchesS(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
            def matchesX(matches: Set[PropertyKey] = Set.empty):T = this.matches(matches)
          }"""
    )
  }

  "with own factory with multiple superTraits (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait Y;@Node trait S;@Node trait X extends Y; @Node trait T extends S with X; @Node class N extends T}",
      q"""
        object S extends RootNodeTraitFactory[S] with SMatchesFactory[S] {
          val label = SMatches.label;
          val labels = SMatches.labels;
          def matches(matches: Set[PropertyKey] = Set.empty) = SMatches.matches(matches);
          def matchesS(matches: Set[PropertyKey] = Set.empty): S = this.matches(matches)
        };""",
      q"""
        object T extends RootNodeTraitFactory[T] with TMatchesFactory[T] {
          val label = TMatches.label;
          val labels = TMatches.labels;
          def matches(matches: Set[PropertyKey] = Set.empty) = TMatches.matches(matches);
          def matchesT(matches: Set[PropertyKey] = Set.empty): T = this.matches(matches);
          def matchesY(matches: Set[PropertyKey] = Set.empty): T = this.matches(matches)
          def matchesS(matches: Set[PropertyKey] = Set.empty): T = this.matches(matches)
          def matchesX(matches: Set[PropertyKey] = Set.empty): T = this.matches(matches)
        }""")
  }

  //TODO: no own factory, when there is no node extending the trait
  "with factory interface" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}}",
      q"""
          trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
    q"""
          trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE] {
            def createT(p: String): NODE
            def mergeT(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }"""
    )
  }

  "without factory interface (only matches)" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String}; @Node class N extends T { val t: String }}",
      q"""
          trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE];"""
    )
  }

  "with superType factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T ; @Node trait X extends T {val p:String} }",
      q"""
          trait XMatchesFactory[+NODE <: X] extends TMatchesFactory[NODE] {
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""
          trait XFactory[+NODE <: X] extends TFactory[NODE] with XMatchesFactory[NODE] {
            def createX(p: String): NODE
            def mergeX(p: String, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""def matchesT(matches: Set[PropertyKey] = Set.empty): NODE;"""
    )
  }

  "with multiple superType factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      q"""trait SMatchesFactory[+NODE <: S] extends TMatchesFactory[NODE] {
            def matchesS(matches: Set[PropertyKey] = Set.empty): NODE
          };
      """,
      q"""trait SFactory[+NODE <: S] extends TFactory[NODE] with SMatchesFactory[NODE] {
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
      q"""trait XMatchesFactory[+NODE <: X] extends TMatchesFactory[NODE] {
            def matchesX(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] with XMatchesFactory[NODE] {
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
      q"""trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XMatchesFactory[+NODE <: X] extends TMatchesFactory[NODE] {
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE;
          }""",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE];""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] with XMatchesFactory[NODE] {
            def createX(p: String, q: Boolean, x: Int): NODE;
            def mergeX(p: String, q: Boolean, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE;
          }""",
      q"""def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): N = this.matches(p, None, x, matches)"""
    )
  }

  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q"""trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
            def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XMatchesFactory[+NODE <: X] extends TMatchesFactory[NODE] {
            def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE;
          }""",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE] {
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
        q"""trait XFactory[+NODE <: X] extends TFactory[NODE] with XMatchesFactory[NODE] {
            def createX(p: String, x: Int, q: Boolean = true): NODE;
            def mergeX(p: String, x: Int, q: Boolean = true, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE;
          }"""
    )
  }

  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Int}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q"""trait TMatchesFactory[+NODE <: T] extends NodeFactory[NODE] {
           def matchesT(p: Option[String] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
         };
         """,
      q"""trait XMatchesFactory[+NODE <: X] extends TMatchesFactory[NODE] {
           def matchesX(p: Option[String] = None, q: Option[Boolean] = None, x: Option[Int] = None, matches: Set[PropertyKey] = Set.empty): NODE
         }""",
      q"""trait TFactory[+NODE <: T] extends NodeFactory[NODE] with TMatchesFactory[NODE] {
            def createT(p: String, x: Int): NODE
            def mergeT(p: String, x: Int, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): NODE
          }""",
      q"""trait XFactory[+NODE <: X] extends TFactory[NODE] with XMatchesFactory[NODE] {
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

  "generates according matches classes and factories" >> {
    generatedContainsCode(
      q"object A {@Node trait Y;@Node trait S;@Node trait X extends Y; @Node trait T extends S with X; @Node class N extends T}",
      q"""
        object YMatches extends YMatchesFactory[YMatches] {
          val label = raw.Label("Y");
          val labels = Set(raw.Label("Y"));
          def wrap(node: raw.Node) = new YMatches(node);
          def matches(matches: Set[PropertyKey] = Set.empty): YMatches = {
          val wrapped = wrap(raw.Node.matches(labels, matches = matches));
            wrapped
          };
          def matchesY(matches: Set[PropertyKey] = Set.empty): YMatches = this.matches(matches)
        };""",
      q"""
        object SMatches extends SMatchesFactory[SMatches] {
          val label = raw.Label("S");
          val labels = Set(raw.Label("S"));
          def wrap(node: raw.Node) = new SMatches(node);
          def matches(matches: Set[PropertyKey] = Set.empty): SMatches = {
          val wrapped = wrap(raw.Node.matches(labels, matches = matches));
            wrapped
          };
          def matchesS(matches: Set[PropertyKey] = Set.empty): SMatches = this.matches(matches)
        };""",
      q"""
        object XMatches extends XMatchesFactory[XMatches] {
          val label = raw.Label("X");
          val labels = Set(raw.Label("X"), raw.Label("Y"));
          def wrap(node: raw.Node) = new XMatches(node);
          def matches(matches: Set[PropertyKey] = Set.empty): XMatches = {
          val wrapped = wrap(raw.Node.matches(labels, matches = matches));
              wrapped
          };
          def matchesY(matches: Set[PropertyKey] = Set.empty): XMatches = this.matches(matches);
          def matchesX(matches: Set[PropertyKey] = Set.empty): XMatches = this.matches(matches)
        };""",
      q"""
        object TMatches extends TMatchesFactory[TMatches] {
          val label = raw.Label("T");
          val labels = Set(raw.Label("T"), raw.Label("Y"), raw.Label("S"), raw.Label("X"));
          def wrap(node: raw.Node) = new TMatches(node);
          def matches(matches: Set[PropertyKey] = Set.empty): TMatches = {
          val wrapped = wrap(raw.Node.matches(labels, matches = matches));
            wrapped
          };
          def matchesY(matches: Set[PropertyKey] = Set.empty): TMatches = this.matches(matches);
          def matchesS(matches: Set[PropertyKey] = Set.empty): TMatches = this.matches(matches);
          def matchesX(matches: Set[PropertyKey] = Set.empty): TMatches = this.matches(matches);
          def matchesT(matches: Set[PropertyKey] = Set.empty): TMatches = this.matches(matches)
        };""",
      q"""
        case class YMatches(rawItem: raw.Node) extends Y {
          override val label = raw.Label("Y");
          override val labels = Set(raw.Label("Y"))
        };""",
      q"""
        case class SMatches(rawItem: raw.Node) extends S {
          override val label = raw.Label("S");
          override val labels = Set(raw.Label("S"))
        };""",
      q"""
        case class XMatches(rawItem: raw.Node) extends X {
          override val label = raw.Label("X");
          override val labels = Set(raw.Label("X"), raw.Label("Y"))
        };""", 
      q"""
        case class TMatches(rawItem: raw.Node) extends T {
          override val label = raw.Label("T");
          override val labels = Set(raw.Label("T"), raw.Label("Y"), raw.Label("S"), raw.Label("X"))
        }""")
  }
}
