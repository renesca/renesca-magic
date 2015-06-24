# renesca-magic
[![Build Status](https://travis-ci.org/renesca/renesca-magic.svg?branch=master)](https://travis-ci.org/renesca/renesca-magic)
[![Coverage Status](https://coveralls.io/repos/renesca/renesca-magic/badge.svg?branch=master)](https://coveralls.io/r/renesca/renesca-magic?branch=master)

renesca-magic generates typesafe database schemas for the Neo4j database based on [renesca](https://github.com/renesca/renesca) using scala macros.

## Feature summary
- Generate boilerplate classes and factories to wrap Nodes, Relations and Graphs
- Generate getters and setters for properties (primitives and optional primitives)
- Generate accessors for neighbours
- View generated code in ```/magic``` of your sbt project root. (you should add it to your ```.gitignore```)

## Installation

To use renesca-magic in your sbt project, add these dependencies and the marco compiler plugin to your ```build.sbt```:

```scala
libraryDependencies ++= Seq(
  "com.github.renesca" %% "renesca" % "0.3.0",
  "com.github.renesca" %% "renesca-magic" % "0.1.8"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
```

## Feedback
Please don't hesitate to create issues about anything. Ideas, Questions, Bugs, Feature Requests, Criticism, ... . If you are stuck with renesca or renesca-magic for some time, this means something does not work as intended or the API is not intuitive. Let's fix this together.


## Usage Examples
### Wrapping low level entities
In [renesca](https://github.com/renesca/renesca), nodes represent low level graph database entities from the property graph model. This is a bit annoying to work with when you have a schema in mind. For example when we have types of nodes that have a specific label, always read and write the same properties and access neighbours with another specific label.

We can wrap our low-level nodes and relations in classes that take care of and hide the boilerplate. In the following example we provide some hand-written boilerplate to demonstrate this.

Here we have two nodes ```Animal``` and ```Food```, connected with the relation ```Eats```. The nodes have getters and setters for their properties and accessors to their neighbours. There are also Factories to wrap low level entities or create new ones.

```scala
import renesca.graph._
import renesca.parameter._
import renesca.parameter.implicits._

case class Animal(node: Node) {
  val label = Label("ANIMAL")
  def eats: Set[Food] = node.outRelations.filter(_.relationType == Eats.relationType).map(_.endNode).
    filter(_.labels.contains(Food.label)).map(Food.wrap)
  def name: String = node.properties("name").asInstanceOf[StringPropertyValue]
}

object Animal {
  val label = Label("ANIMAL")
  def wrap(node: Node) = new Animal(node)
  def create(name: String): Animal = {
    val wrapped = wrap(Node.create(List(label)))
    wrapped.node.properties.update("name", name)
    wrapped
  }
}

case class Food(node: Node) {
  val label = Label("FOOD")
  def rev_eats: Set[Animal] = node.inRelations.filter(_.relationType == Eats.relationType).map(_.startNode).
    filter(_.labels.contains(Animal.label)).map(Animal.wrap)
  def name: String = node.properties("name").asInstanceOf[StringPropertyValue]
  def amount: Long = node.properties("amount").asInstanceOf[LongPropertyValue]
  def `amount_=`(newValue: Long) { node.properties.update("amount", newValue) }
}

object Food {
  val label = Label("FOOD")
  def wrap(node: Node) = new Food(node)
  def create(amount: Long, name: String): Food = {
    val wrapped = wrap(Node.create(List(label)))
    wrapped.node.properties.update("amount", amount)
    wrapped.node.properties.update("name", name)
    wrapped
  }
}

case class Eats(startNode: Animal, relation: Relation, endNode: Food)

object Eats {
  val relationType = RelationType("EATS")
  def wrap(relation: Relation) = Eats(Animal.wrap(relation.startNode), relation, Food.wrap(relation.endNode))
  def create(startNode: Animal, endNode: Food): Eats = {
    wrap(Relation.create(startNode.node, relationType, endNode.node))
  }
}
```

This is a lot of code for a single relation between two nodes. Writing this by hand for a larger schemas takes a lot of time and is very error prone. We can use renesca-magic to generate this for us. Simply write:

```scala
import renesca.schema.macros

@macros.GraphSchema
object Schema {
  @Node class Animal {val name: String}
  @Node class Food {val name: String; var amount:Long}
  @Relation class Eats(startNode: Animal, endNode: Food)
}
```

You can have a look at the generated code in the folder ```/magic``` created at the root of your sbt project. Files created in ```/magic``` are not needed for compilation. You can safely delete them and put the folder into your ```.gitignore```.

### Graph wrapper
### traits, relations to traits
### multiple inheritance, multiple labels
### hyperrelations

## License
renesca-magic is free software released under the [Apache License, Version 2.0][Apache]

[Apache]: http://www.apache.org/licenses/LICENSE-2.0
