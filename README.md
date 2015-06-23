# renesca-magic
[![Build Status](https://travis-ci.org/renesca/renesca-magic.svg?branch=master)](https://travis-ci.org/renesca/renesca-magic)
[![Coverage Status](https://coveralls.io/repos/renesca/renesca-magic/badge.svg?branch=master)](https://coveralls.io/r/renesca/renesca-magic?branch=master)
Generates typesafe database schemas for the Neo4j database based on [renesca](https://github.com/renesca/renesca).

# Concepts
In [renesca](https://github.com/renesca/renesca), nodes represent low level graph database entities from the property graph model. This is a bit annoying to work with when you have schema in mind. For example when we have types of nodes that have a specific label, always read and write the same properties and access neighbours with another specific label.

We can wrap our low-level nodes and relations in classes that take care of and hide the boilerplate. renesca provides useful traits to make this easier:

```
example code
```


* typed neighbour accessors
* traits, relations to traits
* multiple inheritance, labels
* hyperrelations

## License
renesca-magic is free software released under the [Apache License, Version 2.0][Apache]

[Apache]: http://www.apache.org/licenses/LICENSE-2.0
