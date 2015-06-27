package codegeneration

import helpers.CodeComparisonSpec

class PropertiesSpec extends CodeComparisonSpec {
   
  import contextMock.universe._

  "immutable property getter" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:String}}",
      q"""def p: String = item.properties("p").asInstanceOf[StringPropertyValue]""")
  }

  "optional immutable property getter" >> {
    generatedContainsCode(
      q"object A {@Node class N {val p:Option[String]}}",
      q"""def p:Option[String] = item.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""")
  }

  "mutable property getter and setter" >> {
    generatedContainsCode(
      q"object A {@Node class N {var p:String}}",
      q"""def p: String = item.properties("p").asInstanceOf[StringPropertyValue]""",
      q"""def `p_=`(newValue: String): scala.Unit = item.properties.update("p", newValue)"""
    )
  }

  "optional mutable property getter and setter" >> {
    generatedContainsCode(
      q"object A {@Node class N {var p:Option[String]}}",
      q"""def p:Option[String] = item.properties.get("p").asInstanceOf[Option[StringPropertyValue]].map(propertyValueToPrimitive)""",
      q"""def `p_=`(newValue:Option[String]): scala.Unit = { if(newValue.isDefined) item.properties("p") = newValue.get else item.properties -= "p" }"""
    )
  }
}
