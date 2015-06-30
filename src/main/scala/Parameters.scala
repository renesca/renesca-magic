package renesca.schema.macros

trait Parameters extends Context {

  import Helpers._
  import context.universe._

  case class Parameter(name: Tree, typeName: Tree, optional: Boolean, default: Option[Tree], mutable: Boolean) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Parameter]

    override def equals(other: Any): Boolean = other match {
      case that: Parameter =>
        (that canEqual this) &&
          this.name.toString == that.name.toString &&
          this.typeName.toString == that.typeName.toString &&
          this.optional == that.optional &&
          this.default.toString == that.default.toString &&
          this.mutable == that.mutable
      case _               => false
    }

    override def hashCode: Int = List(this.name.toString, this.typeName.toString, this.optional, this.default.toString, this.mutable).hashCode
    def toParamCode: Tree = this match {
      case Parameter(propertyName, propertyType, _, None, _)               => q"val ${ TermName(propertyName.toString) }:${ propertyType }"
      case Parameter(propertyName, propertyType, _, Some(defaultValue), _) => q"val ${ TermName(propertyName.toString) }:${ propertyType } = ${ defaultValue }"
    }

    def toAssignmentCode(schemaItem: Tree): Tree = this match {
      case Parameter(propertyName, propertyType, false, _, _) => q"$schemaItem.properties(${ propertyName.toString }) = $propertyName"
      case Parameter(propertyName, propertyType, true, _, _)  => q"if($propertyName.isDefined) $schemaItem.properties(${ propertyName.toString }) = $propertyName.get"
    }
  }

  case class ParameterList(parameters: List[Parameter], typeName: String, hasOwnFactory: Option[Boolean]) {

    val (withDefault, nonDefault) = parameters.sortBy(_.name.toString).partition(_.default.isDefined)
    val (withDefaultOptional, withDefaultNonOptional) = withDefault.partition(_.optional)
    val ordered = nonDefault ::: withDefaultNonOptional ::: withDefaultOptional
    def toParamCode: List[Tree] = ordered.map(_.toParamCode)
    def toAssignmentCode(schemaItem: Tree): List[Tree] = ordered.map(_.toAssignmentCode(schemaItem))

    def optional = ParameterList(parameters.map {
      case Parameter(name, typeName, false, _, mutable) => Parameter(name, tq"Option[$typeName]", true, Some(q"None"), mutable)
      case Parameter(name, typeName, true, _, mutable)  => Parameter(name, typeName, true, Some(q"None"), mutable)
    }, typeName, hasOwnFactory)

    def supplementMissingParametersOf(that: ParameterList): List[Tree] = {
      this.ordered.map(p => (p, that.ordered.find(_.name.toString == p.name.toString))).map {
        case (_, Some(other)) => other.name
        case (mine, None)     => mine.default.get // we know that we only handle a default param at this point (put into typesystem?)
      }
    }

    def toCallerCode = this.ordered.map(_.name)
  }

  object ParameterList {
    def create(flatStatements: List[Tree], typeName: String, hasOwnFactory: Option[Boolean] = Some(true)): ParameterList = ParameterList(flatStatements.collect {
      case statement@(q"val $propertyName:Option[$propertyType] = $default") => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = false)
      case statement@(q"var $propertyName:Option[$propertyType] = $default") => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = true)
      case statement@(q"val $propertyName:Option[$propertyType]")            => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = false)
      case statement@(q"var $propertyName:Option[$propertyType]")            => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = true)
      case statement@(q"val $propertyName:$propertyType = $default")         => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = false)
      case statement@(q"var $propertyName:$propertyType = $default")         => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = true)
      case statement@(q"val $propertyName:$propertyType")                    => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = false)
      case statement@(q"var $propertyName:$propertyType")                    => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = true)
    }, typeName, hasOwnFactory)
  }
}
