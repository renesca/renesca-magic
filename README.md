# renesca-magic
[![Build Status](https://travis-ci.org/renesca/renesca-magic.svg?branch=master)](https://travis-ci.org/renesca/renesca-magic)
[![Coverage Status](https://coveralls.io/repos/renesca/renesca-magic/badge.svg?branch=master)](https://coveralls.io/r/renesca/renesca-magic?branch=master)

renesca-magic generates typesafe database schemas for the Neo4j database based on [renesca](https://github.com/renesca/renesca) using scala macros.

## Feature summary
- Generate boilerplate classes and factories to wrap Nodes, Relations and Graphs
- Generate getters and setters for properties (primitives and optional primitives)
- Generate accessors for neighbours on Nodes
- Generate filtered lists for each Node/Relation type in a Graph
- View generated code in ```/magic``` of your sbt project root. (you should add it to your ```.gitignore```)
- traits, multiple inheritance
- graph inheritance

## Installation

To use renesca-magic in your sbt project, add these dependencies and the marco compiler plugin to your ```build.sbt```:

```scala
libraryDependencies ++= Seq(
  "com.github.renesca" %% "renesca" % "0.3.0",
  "com.github.renesca" %% "renesca-magic" % "0.3.0"
)

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
```

## Feedback
Please don't hesitate to create issues about anything. Ideas, Questions, Bugs, Feature Requests, Criticism, ... . If you are stuck with renesca or renesca-magic for some time, this means something does not work as intended or the API is not intuitive. Let's fix this together.


## Usage Examples
You can find all of these examples available as sbt project: [renesca/renesca-magic-example](https://github.com/renesca/renesca-magic-example). You can also have a look at the generated code: [renesca-magic-example/magic](https://github.com/renesca/renesca-magic-example/tree/master/magic)

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
  def eats: Set[Food] = node.outRelations.
    filter(_.relationType == Eats.relationType).map(_.endNode).
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

...
```
See the full boilerplate example at [renesca-example/.../Schema.scala](https://github.com/renesca/renesca-example/blob/master/src/main/scala/renesca/example/Schema.scala).

This is a lot of code for a single relation between two nodes. Writing this by hand for a larger schemas takes a lot of time and is very error prone. We can use renesca-magic to generate this for us. Simply write:

```scala
import renesca.schema.macros

@macros.GraphSchema
object ExampleSchemaWrapping {
  // Nodes get their class name as uppercase label
  @Node class Animal {val name: String }
  @Node class Food {
    val name: String
    var amount: Long
  }
  // Relations get their class name as uppercase relationType
  @Relation class Eats(startNode: Animal, endNode: Food)
}

import ExampleSchemaWrapping._

val snake = Animal.create("snake")
val cake = Food.create(name = "cake", amount = 1000)
val eats = Eats.create(snake, cake)

cake.amount -= 100
```

Note that ```Food.name``` is a ```val``` and only generates a getter. ```Food.amount``` is a ```var``` and therefore generates a getter and a setter.

You can have a look at the generated code in the folder ```/magic``` created at the root of your sbt project. Files created in ```/magic``` are not needed for compilation. You can safely delete them and put the folder into your ```.gitignore```.

### Wrapping induced subgraphs
Use the ```@Graph``` annotation to wrap a subgraph. This generates filtered Set accessors for each Node and Relation type.

```scala
import renesca.schema.macros

@macros.GraphSchema
object ExampleSchemaSubgraph {
  @Node class Animal {val name: String }
  @Node class Food {
    val name: String
    var amount: Long
  }
  @Relation class Eats(startNode: Animal, endNode: Food)

  // Relations between specified nodes will be induced
  @Graph trait Zoo {Nodes(Animal, Food) }
}

import ExampleSchemaSubgraph._

val zoo = Zoo(db.queryGraph("MATCH (a:ANIMAL)-[e:EATS]->(f:FOOD) RETURN a,e,f"))
val elefant = Animal.create("elefant")
val pizza = Food.create(name = "pizza", amount = 2)
zoo.add(Eats.create(elefant, pizza))
zoo.animals // Set(elefant)
zoo.relations // Set(elefant eats pizza)
db.persistChanges(zoo)
```

### Traits and relations to traits
```scala
@macros.GraphSchema
object ExampleSchemaTraits {
  // Inheriting Nodes receive their name as additional label
  @Node trait Animal {val name: String }

  // Node with labels FISH and ANIMAL
  @Node class Fish extends Animal
  @Node class Dog extends Animal

  @Relation trait Consumes { val funny:Boolean }

  // Relations can connect Node traits.
  // So we can connect any node extending the trait
  @Relation class Eats(startNode: Animal, endNode: Animal) extends Consumes
  @Relation class Drinks(startNode: Animal, endNode: Animal) extends Consumes

  // Zoo contains all Animals (Animal expands to all subNodes)
  @Graph trait Zoo {Nodes(Animal) }
}

import ExampleSchemaTraits._

val zoo = Zoo.empty
val bello = Dog.create("bello")
val wanda = Fish.create("wanda")

zoo.add(bello)
zoo.add(wanda)
zoo.animals // Set(bello, wanda)

zoo.add(Eats.create(bello, wanda, funny = false))
zoo.add(Drinks.create(wanda, bello, funny = true))
```

### Multiple inheritance, multiple Node labels
```scala
@macros.GraphSchema
object ExampleSchemaMultipleInheritance {
  // Assignments are default values for properties
  // They can also be arbitrary statements
  @Node trait Uuid { val uuid: String = java.util.UUID.randomUUID.toString }
  @Node trait Timestamp { val timestamp: Long = System.currentTimeMillis }
  @Node trait Taggable

  @Node class Article extends Uuid with Timestamp with Taggable {
    val content:String
  }
  @Node class Tag extends Uuid { val name:String }
  @Relation class Categorizes(startNode:Tag, endNode:Taggable)

  @Graph trait Blog {Nodes(Article, Tag)}
}

import ExampleSchemaMultipleInheritance._
val initGraph = Blog.empty
initGraph.add(Tag.create(name="useful"))
initGraph.add(Tag.create(name="important"))
db.persistChanges(initGraph)

val blog = Blog(db.queryGraph("MATCH (t:TAG) return t"))

// automatically set uuid and timestamp
val article = Article.create(content = "Some useful and important content")
blog.add(article)

// set all tags on the article
blog.tags.foreach{ tag =>
  Categorizes.create(tag, article)
}

blog.taggables // Set(article)

db.persistChanges(blog)
```

### HyperRelations
```scala
@macros.GraphSchema
object ExampleSchemaHyperRelations {
  @Node trait Uuid { val uuid: String = java.util.UUID.randomUUID.toString }
  @Node trait Taggable
  @Node class Tag extends Uuid {val name:String}
  @Node class User extends Uuid {val name:String}
  @Node class Article extends Uuid with Taggable {val content:String}

  // A HyperRelation is a node representing a relation:
  // (n)-[]->(hyperRelation)-[]->(m)
  // It behaves like node and relation at the same time
  // and therefore can extend node and relation traits.
  @HyperRelation class Tags(startNode: Tag, endNode: Taggable) extends Uuid
  // Because they are nodes, we can connect a HyperRelation with another Node
  @Relation class Supports(startNode: User, endNode: Tags)
}

import ExampleSchemaHyperRelations._
val user = User.create(name="pumuckl")
val helpful = Tag.create(name="helpful")
val article = Article.create(content="Dog eats Snake")

val tags = Tags.create(helpful, article) // HyperRelation
val supports = Supports.create(user, tags) // Relation from user to HyperRelation
```

## License
renesca-magic is free software released under the [Apache License, Version 2.0][Apache]

[Apache]: http://www.apache.org/licenses/LICENSE-2.0
