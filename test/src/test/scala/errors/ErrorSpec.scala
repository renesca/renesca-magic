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
        }
      }
    }
    "Group" >> {
      "trait" >> {
        "inherits from" >> {
          "Node" >> {
            "class" >> {
              generatedAborts(q"object A {@Node class T; @Group trait N extends T}",
                "Group trait `N` cannot inherit from Node class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Node trait T; @Group trait N extends T}",
                "Group trait `N` cannot inherit from Node trait `T`.")
            }
          }
          "Relation" >> {
            "class" >> {
              generatedAborts(q"object A {@Relation class T(startNode:A, endNode:B); @Group trait N extends T}",
                "Group trait `N` cannot inherit from Relation class `T`.")
            }
            "trait" >> {
              generatedAborts(q"object A {@Relation trait T; @Group trait N extends T}",
                "Group trait `N` cannot inherit from Relation trait `T`.")
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
    //TODO: start/endNode can only be nodes/nodeTraits
  }

  "not allowed class/object/trait" >> {
    "Group class" >> {
      generatedAborts(q"object A {@Group class G}",
        "Group class `G` is not allowed. Use a trait instead.")
    }
    "Group object" >> {
      generatedAborts(q"object A {@Group object G}",
        "Group object `G` is not allowed. Use a trait instead.")
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
}