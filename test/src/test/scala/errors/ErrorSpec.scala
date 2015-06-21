package errors

import helpers.CodeComparisonSpec
import org.mockito.Mockito.reset

class ErrorSpec extends CodeComparisonSpec {

  import contextMock.universe._
  import scala.util.Try

  def generatedAborts(source: Tree, msg: String) = {
    reset(magic.aborter)
    Try { generate(source) }
    there was one(magic.aborter).abort(msg)
  }

  "Invalid inheritance" >> {
    "Node" >> {
      "class" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Node class N extends T}",
                "Node class `N` cannot inherit from Node class `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Node class N extends T}",
                "Node class `N` cannot inherit from Relation class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Relation trait T; @Node class N extends T}",
                "Node class `N` cannot inherit from Relation trait `T`.")
            }
          }
          "Graph trait" >> {
            generatedAborts(q"object A {@Graph trait T; @Node class N extends T}",
              "Node class `N` cannot inherit from Graph trait `T`.")
          }
        }

      }
      "trait" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Node trait N extends T}",
                "Node trait `N` cannot inherit from Node class `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Node trait N extends T}",
                "Node trait `N` cannot inherit from Relation class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Relation trait T; @Node trait N extends T}",
                "Node trait `N` cannot inherit from Relation trait `T`.")
            }
          }
          "Graph trait" >> {
            generatedAborts(q"object A {@Graph trait T; @Node trait N extends T}",
              "Node trait `N` cannot inherit from Graph trait `T`.")
          }
          "Itself" >> todo
        }
      }
    }
    "Relation" >> {
      "class" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Relation class R(startNode:A, endNode:B) extends T}",
                "Relation class `R` cannot inherit from Node class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Node trait T; @Relation class R(startNode:A, endNode:B) extends T}",
                "Relation class `R` cannot inherit from Node trait `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Relation class N(startNode:A, endNode:B) extends T}",
                "Relation class `N` cannot inherit from Relation class `T`.")
            }
          }
          "Graph trait" >> {
            generatedAborts(q"object A {@Graph trait T; @Relation class N(startNode:A, endNode:B) extends T}",
              "Relation class `N` cannot inherit from Graph trait `T`.")
          }
        }

      }
      "trait" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Relation trait R extends T}",
                "Relation trait `R` cannot inherit from Node class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Node trait T; @Relation trait R extends T}",
                "Relation trait `R` cannot inherit from Node trait `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Relation trait R extends T}",
                "Relation trait `R` cannot inherit from Relation class `T`.")
            }
          }
          "Graph trait" >> {
            generatedAborts(q"object A {@Graph trait T; @Relation trait N extends T}",
              "Relation trait `N` cannot inherit from Graph trait `T`.")
          }
          "Itself" >> todo
        }
      }
    }
    "HyperRelation" >> {
      "class" >> {
        "inherits from" >> {
          "Node class" >> {
            generatedAborts(q"object A {@Node class T; @HyperRelation class R(startNode:A, endNode:B) extends T}",
              "HyperRelation class `R` cannot inherit from Node class `T`.")
          }
          "Relation class" >> {
            generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @HyperRelation class N(startNode:A, endNode:B) extends T}",
              "HyperRelation class `N` cannot inherit from Relation class `T`.")
          }
          "Graph trait" >> {
            generatedAborts(q"object A {@Graph trait T; @HyperRelation class N(startNode:A, endNode:B) extends T}",
              "HyperRelation class `N` cannot inherit from Graph trait `T`.")
          }
        }

      }
    }
    "Graph" >> {
      "trait" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Graph trait N extends T}",
                "Graph trait `N` cannot inherit from Node class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Node trait T; @Graph trait N extends T}",
                "Graph trait `N` cannot inherit from Node trait `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Graph trait N extends T}",
                "Graph trait `N` cannot inherit from Relation class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Relation trait T; @Graph trait N extends T}",
                "Graph trait `N` cannot inherit from Relation trait `T`.")
            }
          }
        }
      }
    }
  }

  "needs startNode and endNode" >> {
    "Relation" >> {
      generatedAborts(q"object A {@Relation class R}",
        "Relation class `R` needs startNode and endNode.")
    }
    "HyperRelation" >> {
      generatedAborts(q"object A {@HyperRelation class R}",
        "HyperRelation class `R` needs startNode and endNode.")
    }

    "Relation only allows nodes,node traits and hyperRelations" >> {
      generatedAborts(q"object A {@Relation class A(startNode:X, endNode:Y); @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Relation.")
      generatedAborts(q"object A {@Relation trait A; @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Relation trait.")
      generatedAborts(q"object A {@Graph trait A; @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Graph.")
      generatedAborts(q"object A {@Relation class B(startNode:X, endNode:Y); @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Relation.")
      generatedAborts(q"object A {@Relation trait B; @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Relation trait.")
      generatedAborts(q"object A {@Graph trait B; @Relation class R(startNode:A, endNode:B)}",
        "Relation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Graph.")
    }
    "HyperRelation only allows nodes,node traits and hyperRelations" >> {
      generatedAborts(q"object A {@Relation class A(startNode:X, endNode:Y); @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Relation.")
      generatedAborts(q"object A {@Relation trait A; @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Relation trait.")
      generatedAborts(q"object A {@Graph trait A; @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs startNode `A` to be a Node, Node trait, or HyperRelation. Not a Graph.")
      generatedAborts(q"object A {@Relation class B(startNode:X, endNode:Y); @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Relation.")
      generatedAborts(q"object A {@Relation trait B; @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Relation trait.")
      generatedAborts(q"object A {@Graph trait B; @HyperRelation class R(startNode:A, endNode:B)}",
        "HyperRelation class `R` needs endNode `B` to be a Node, Node trait, or HyperRelation. Not a Graph.")
    }

  }

  "not allowed class/object/trait" >> {
    "Graph class" >> {
      generatedAborts(q"object A {@Graph class G}",
        "Graph class `G` is not allowed. Use a trait instead.")
    }
    "Graph object" >> {
      generatedAborts(q"object A {@Graph object G}",
        "Graph object `G` is not allowed. Use a trait instead.")
    }
    "Node object" >> {
      generatedAborts(q"object A {@Node object N}",
        "Node object `N` is not allowed. Use a class or trait instead.")
    }
    "Relation object" >> {
      generatedAborts(q"object A {@Relation object R}",
        "Relation object `R` is not allowed. Use a class or trait instead.")
    }
    "HyperRelation object" >> {
      generatedAborts(q"object A {@HyperRelation object R}",
        "HyperRelation object `R` is not allowed. Use a class instead.")
    }
    "HyperRelation trait" >> {
      generatedAborts(q"object A {@HyperRelation trait R}",
        "HyperRelation trait `R` is not allowed. Use a class instead.")
    }

  }

  //TODO: generics are not allowed
  //TODO: Graph list items have to be nodes
}
