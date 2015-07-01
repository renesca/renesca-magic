package renesca.schema.macros

trait Parameters extends Context {

  import Helpers._
  import context.universe._

  case class Parameter(name: Tree, typeName: Tree, optional: Boolean, default: Option[Tree], mutable: Boolean, unique: Boolean) {
    def canEqual(other: Any): Boolean = other.isInstanceOf[Parameter]

    override def equals(other: Any): Boolean = other match {
      case that: Parameter =>
        (that canEqual this) &&
          this.name.toString == that.name.toString &&
          this.typeName.toString == that.typeName.toString &&
          this.optional == that.optional &&
          this.default.toString == that.default.toString &&
          this.mutable == that.mutable &&
          this.unique == that.unique
      case _               => false
    }

    override def hashCode: Int = List(this.name.toString, this.typeName.toString, this.optional, this.default.toString, this.mutable).hashCode
    def toParamCode: Tree = this match {
      case Parameter(propertyName, propertyType, _, None, _, _)               => q"val ${ TermName(propertyName.toString) }:${ propertyType }"
      case Parameter(propertyName, propertyType, _, Some(defaultValue), _, _) => q"val ${ TermName(propertyName.toString) }:${ propertyType } = ${ defaultValue }"
    }

    def toAssignmentCode(schemaItem: Tree): Tree = this match {
      case Parameter(propertyName, propertyType, false, _, _, _) => q"$schemaItem.properties(${ propertyName.toString }) = $propertyName"
      case Parameter(propertyName, propertyType, true, _, _, _)  => q"if($propertyName.isDefined) $schemaItem.properties(${ propertyName.toString }) = $propertyName.get"
    }
  }

  case class ParameterList(parameters: List[Parameter], typeName: String, hasOwnFactory: Option[Boolean]) {

    val (withDefault, nonDefault) = parameters.sortBy(_.name.toString).partition(_.default.isDefined)
    val (withDefaultOptional, withDefaultNonOptional) = withDefault.partition(_.optional)
    val ordered = nonDefault ::: withDefaultNonOptional ::: withDefaultOptional
    def toParamCode: List[Tree] = ordered.map(_.toParamCode)
    def toAssignmentCode(schemaItem: Tree): List[Tree] = ordered.map(_.toAssignmentCode(schemaItem))

    def optional = ParameterList(parameters.map {
      case Parameter(name, typeName, false, _, mutable, unique) => Parameter(name, tq"Option[$typeName]", true, Some(q"None"), mutable, unique)
      case Parameter(name, typeName, true, _, mutable, unique)  => Parameter(name, typeName, true, Some(q"None"), mutable, unique)
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
    def warnOnUniqueOptional(statement: Tree) = {
      warner.warning("@unique constraints cannot be used with optional properties. Annotation has no effect: " + statement.toString)
    }

    def warnOnUniqueRelation(representsNode: Boolean, statement: Tree) = {
      if(!representsNode)
        warner.warning("@unique constraints can only be used in Nodes, NodeTraits and HyperRelations. Annotation has no effect: " + statement.toString)
    }

    def create(flatStatements: List[Tree], typeName: String, representsNode: Boolean, hasOwnFactory: Option[Boolean] = Some(true)): ParameterList = ParameterList(flatStatements.collect {
      case (q"val $propertyName:Option[$propertyType] = $default")           => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = false, unique = false)
      case (q"var $propertyName:Option[$propertyType] = $default")           => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = true, unique = false)
      case (q"val $propertyName:Option[$propertyType]")                      => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = false, unique = false)
      case (q"var $propertyName:Option[$propertyType]")                      => Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = true, unique = false)
      case (q"val $propertyName:$propertyType = $default")                   => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = false, unique = false)
      case (q"var $propertyName:$propertyType = $default")                   => Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = true, unique = false)
      case (q"val $propertyName:$propertyType")                              => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = false, unique = false)
      case (q"var $propertyName:$propertyType")                              => Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = true, unique = false)
      case s@(q"@unique val $propertyName:Option[$propertyType] = $default") =>
        warnOnUniqueOptional(s)
        Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = false, unique = false)
      case s@(q"@unique var $propertyName:Option[$propertyType] = $default") =>
        warnOnUniqueOptional(s)
        Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"$default"), mutable = true, unique = false)
      case s@(q"@unique val $propertyName:Option[$propertyType]")            =>
        warnOnUniqueOptional(s)
        Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = false, unique = false)
      case s@(q"@unique var $propertyName:Option[$propertyType]")            =>
        warnOnUniqueOptional(s)
        Parameter(q"$propertyName", tq"Option[$propertyType]", optional = true, default = Some(q"None"), mutable = true, unique = false)
      case s@(q"@unique val $propertyName:$propertyType = $default")         =>
        warnOnUniqueRelation(representsNode, s)
        Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = false, unique = representsNode)
      case s@(q"@unique var $propertyName:$propertyType = $default")         =>
        warnOnUniqueRelation(representsNode, s)
        Parameter(q"$propertyName", q"$propertyType", optional = false, default = Some(q"$default"), mutable = true, unique = representsNode)
      case s@(q"@unique val $propertyName:$propertyType")                    =>
        warnOnUniqueRelation(representsNode, s)
        Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = false, unique = representsNode)
      case s@(q"@unique var $propertyName:$propertyType")                    =>
        warnOnUniqueRelation(representsNode, s)
        Parameter(q"$propertyName", q"$propertyType", optional = false, default = None, mutable = true, unique = representsNode)
    }, typeName, hasOwnFactory)
  }

}
