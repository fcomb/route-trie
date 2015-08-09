# URI Route Trie

[![Build Status](https://travis-ci.org/fcomb/route-trie.svg?branch=develop)](https://travis-ci.org/fcomb/route-trie)

High performance URI router based on [radix trie](https://en.wikipedia.org/wiki/Radix_tree) structure.

## Add to project

### Add resolvers to your `build.sbt`

```scala
resolvers += Resolver.bintrayRepo("fcomb", "maven")
```

### Add dependencies to your `build.sbt`

```scala
libraryDependencies += "io.fcomb" %% "route-trie" % "0.2.0"
```

## Usage

```scala
import io.fcomb.trie.RouteTrie
import io.fcomb.trie.RouteMethods._

val tree = RouteTrie(
  "/user" -> (GET, 1),
  "/user/:id" -> (GET, 2),
  "/file/*file" -> (POST, 3)
)
tree.get(GET, "/user/12") // will return Some((2, Some(OpenHashMap("id" -> "12"))))
```

## License

MIT
