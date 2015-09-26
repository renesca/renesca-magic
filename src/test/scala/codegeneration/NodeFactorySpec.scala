package codegeneration

import helpers.CodeComparisonSpec

class NodeFactorySpec extends CodeComparisonSpec {

  import contextMock.universe._

  "simple node factory" >> {
    generatedContainsCode(
      q"object A {@Node class N}",
      q"""object N extends NodeFactory[N] {
        val label = raw.Label("N");
        val labels = Set(raw.Label("N"));
        def wrap(node: raw.Node) = new N(node);
        def matches(matches: Set[PropertyKey] = Set.empty): N = {
          val wrapped = wrap(raw.Node.matches(labels, matches = matches));
          wrapped
        }
        def create(): N = {
          val wrapped = wrap(raw.Node.create(labels));
          wrapped
        }
        def merge(merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N = {
          val wrapped = wrap(raw.Node.merge(labels, merge = merge, onMatch = onMatch));
          wrapped
        }
      }"""
    )
  }

  "with super factory" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T}",
      """object N extends TFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"))""",
      q"""def createT(): N = this.create()"""
    )
  }

  "with super factory with external superType" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node class N extends T with Immutable}",
      """object N extends TFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"))""",
      q"""def createT(): N = this.create()"""
    )
  }

  "with multiple super factories" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S; @Node class N extends T with S}",
      """object N extends TFactory[N] with SFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))""",
      q"""def createT(): N = this.create()""",
      q"""def createS(): N = this.create()"""
    )
  }

  "with multiple super factories (chain)" >> {
    generatedContainsCode(
      q"object A {@Node trait T; @Node trait S extends T; @Node class N extends S}",
      """object N extends SFactory[N] {""",
      q"""val label = raw.Label("N")""",
      q"""val labels = Set(raw.Label("N"), raw.Label("T"), raw.Label("S"))""",
      q"""def createS(): N = this.create()"""
    )
  }

  "with properties" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:String; var x:Long}}",
      q"""def create(p: String, x: Long): N = {
            val wrapped = wrap(raw.Node.create(labels));
            wrapped.rawItem.properties.update("p", p);
            wrapped.rawItem.properties.update("x", x);
            wrapped
          } """
    )
  }

  "with properties - parameter order of create" >> {
    generatedContainsCode(
      q"""object A {
            @Node class N {
              var y:Option[Boolean]
              val q:Option[Double]
              var x:Long
              val p:String
            }
          }""",
      q"""def create(p: String, x: Long, q: Option[Double] = None, y: Option[Boolean] = None): N""",
      q"""def merge(p: String, x: Long, q: Option[Double] = None, y: Option[Boolean] = None, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): N""",
      q"""def matches(p: Option[String] = None, q: Option[Double] = None, x: Option[Long] = None, y: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): N"""
    )
  }

  "with inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node class N extends T}",
      q"""def create(p: String, x: Long): N = {
            val wrapped = wrap(raw.Node.create(labels));
            wrapped.rawItem.properties.update("p", p);
            wrapped.rawItem.properties.update("x", x);
            wrapped
          }""",
      q""" def createT(p: String, x: Long): N = this.create(p, x) """
    )
  }

  "with inherited properties by two traits" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Long}; @Node class N extends T with S}",
      q"""def create(p: String, x: Long): N = {
            val wrapped = wrap(raw.Node.create(labels));
            wrapped.rawItem.properties.update("p", p);
            wrapped.rawItem.properties.update("x", x);
            wrapped
          }"""
    )
  }

  "with indirectly inherited properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T; @Node class N extends X}",
      q""" def createX(p: String, x: Long): N = this.create(p, x) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x) """
    )
  }

  "with indirectly inherited properties and default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T { val q: Boolean = true }; @Node class N extends X}",
      q""" def createX(p: String, x: Long, q: Boolean = true): N = this.create(p, x, q) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x, true) """
    )
  }

  "with indirectly inherited properties and optional properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T { val q: Option[Boolean] }; @Node class N extends X}",
      q""" def createX(p: String, x: Long, q: Option[Boolean] = None): N = this.create(p, x, q) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x, None) """
    )
  }

  "with indirectly inherited properties and optional default properties" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T { val q: Option[Boolean] = Some(true) }; @Node class N extends X}",
      q""" def createX(p: String, x: Long, q: Option[Boolean] = Some(true)): N = this.create(p, x, q) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x, Some(true)) """
    )
  }

  "with indirectly inherited properties and default properties (var)" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T { var q: Boolean = true }; @Node class N extends X}",
      q""" def createX(p: String, x: Long, q: Boolean = true): N = this.create(p, x, q) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x, true) """
    )
  }

  "with indirectly inherited properties and optional properties (var)" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String; var x:Long}; @Node trait X extends T { var q: Option[Boolean] = Some(true) }; @Node class N extends X}",
      q""" def createX(p: String, x: Long, q: Option[Boolean] = Some(true)): N = this.create(p, x, q) """,
      q""" def createT(p: String, x: Long): N = this.create(p, x, Some(true)) """
    )
  }

  "with indirectly inherited properties by two traits" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait S {var x:Long}; @Node trait X extends T with S; @Node class N extends X}",
      q""" def createX(p: String, x: Long): N = this.create(p, x) """,
      Not("def createS("),
      Not("def createT(")
    )
  }

  "with unique matches factory methods in node" >> {
    generatedContainsCode(
      q"object A {@Node trait T {@unique val p:String}; @Node class N extends T {@unique var q:Boolean }}",
      q"""def matchesT(p: Option[String] = None, matches: Set[PropertyKey] = Set.empty): NODE""",
      q"""def matchesOnP(p: String): NODE = this.matchesT(p = Some(p), matches = Set("p"))""",
      q"""def matches(p: Option[String] = None, q: Option[Boolean] = None, matches: Set[PropertyKey] = Set.empty): N""",
      q"""def matchesOnQ(q: Boolean): N = this.matches(q = Some(q), matches = Set("q"))"""
    )
  }

  "diamond inheritance" >> {
    generatedContainsCode(
      q"object A {@Node trait T {val p:String }; @Node trait L extends T; @Node trait R extends T; @Node trait X extends L with R; @Node class N extends X}",
      q""" def create(p: String): N"""
    )
  }
  // TODO one direct + one indirect
}
