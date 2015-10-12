# URI Route Trie

[![Build Status](https://travis-ci.org/fcomb/route-trie.svg?branch=develop)](https://travis-ci.org/fcomb/route-trie)
[![License](https://img.shields.io/:license-MIT-green.svg)](http://opensource.org/licenses/MIT)

High performance URI router based on [radix trie](https://en.wikipedia.org/wiki/Radix_tree) structure.

## Add to project

### Add resolvers to your `build.sbt`

```scala
resolvers += Resolver.bintrayRepo("fcomb", "maven")
```

### Add dependencies to your `build.sbt`

```scala
libraryDependencies += "io.fcomb" %% "route-trie" % "0.4.0"
```

## Usage

```scala
import io.fcomb.trie.RouteTrie
import io.fcomb.trie.RouteMethods._

val tree = RouteTrie[Int](
  "/users" -> (GET, 1),
  "/users/:id" -> (GET, 2),
  "/files/*file" -> (POST, 3)
)
tree.get(GET, "/users/12") // will return Some((2, Some(OpenHashMap("id" -> "12"))))
```
