# URI Route Trie

High performance URI router based on [radix trie](https://en.wikipedia.org/wiki/Radix_tree) structure.

## Add to project

### Add resolvers to your `build.sbt`

```scala
resolvers += Resolver.bintrayRepo("fcomb", "maven")
```

### Add dependencies to your `build.sbt`

```scala
libraryDependencies += "io.fcomb" %% "route-trie" % "0.1.0"
```

## Usage

```scala
import io.fcomb.trie.RouteTrie

val tree = RouteTrie(
  "/user" -> 1,
  "/user/:id" -> 2,
  "/file/*file" -> 3
)
tree.get("/user/12") // will return Some((12, Some(OpenHashMap("id" -> "12"))))
```

## License

MIT
