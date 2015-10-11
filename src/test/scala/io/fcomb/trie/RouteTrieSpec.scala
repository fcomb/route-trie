package io.fcomb.trie

import RouteMethods._
import org.specs2.mutable._
import scala.collection.mutable.OpenHashMap

class RouteTrieSpec extends Specification {
  "RouteTrie" should {
    "match URIs" in {
      val tree = RouteTrie(
        "/user" -> (GET, 1),
        "/userify" -> (GET, 2),
        "/user/about" -> (GET, 3),
        "/user/:id/x" -> (GET, 4),
        "/user/:id/x/:xid" -> (GET, 12),
        "/user/:id/x/:xid" -> (POST, 13),
        "/user/:id" -> (GET, 5),
        "/useragent" -> (GET, 6),
        "/url/static" -> (GET, 7),
        "/user/g/h/n" -> (GET, 8),
        "/f/*file" -> (GET, 9),
        "/урл/тест/DFSD©Δ§ß∞¢" -> (GET, 10),
        "test" -> (GET, 12),
        "/:wildcard" -> (GET, 11)
      )

      tree.size must_== 14

      tree.get(GET, "/user") must_== Some(1, None)
      tree.get(GET, "/user/12") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12/") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12//") must_== None
      tree.get(GET, "/user/12/x") must_== Some(4, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12/x/333") must_== Some(12, Some(OpenHashMap("id" -> "12", "xid" -> "333")))
      tree.get(POST, "/user/12/x/444/") must_== Some(13, Some(OpenHashMap("id" -> "12", "xid" -> "444")))
      tree.get(GET, "/user/about") must_== Some(3, None)
      tree.get(GET, "/url/static") must_== Some(7, None)
      tree.get(POST, "/url/static") must_== None
      tree.get(GET, "user") must_== None
      tree.get(GET, "/f/тест/DFSD©Δ§ß∞¢") must_== Some(9, Some(OpenHashMap("file" -> "тест/DFSD©Δ§ß∞¢")))
      tree.get(GET, "/урл/тест/DFSD©Δ§ß∞¢") must_== Some(10, None)
      tree.get(GET, "/test") must_== Some(12, None)
      tree.get(GET, "/no_url") must_== Some(11, Some(OpenHashMap("wildcard" -> "no_url")))
    }

    "immutable insert" in {
      val tree = RouteTrie.empty[Int]
      Map(
        "/user" -> (GET, 1),
        "/userify" -> (GET, 2),
        "/user/about" -> (GET, 3),
        "/user/:id/x" -> (GET, 4),
        "/user/:id/x/:xid" -> (GET, 12),
        "/user/:id/x/:xid" -> (POST, 13),
        "/user/:id" -> (GET, 5),
        "/useragent" -> (GET, 6),
        "/url/static" -> (GET, 7),
        "/user/g/h/n" -> (GET, 8),
        "/f/*file" -> (GET, 9),
        "/урл/тест/DFSD©Δ§ß∞¢" -> (GET, 10),
        "test" -> (GET, 12),
        "/:wildcard" -> (GET, 11)
      ).foreach { case (k, (m, v)) =>
        tree + (k, m, v)
      }
      tree.size must_== 0
    }

    "validate wildcardcard children" in {
      RouteTrie(
        "/test/*file" -> (GET, 0),
        "/test/*file/something" -> (GET, 1)
      ) must throwA[IllegalArgumentException]
    }

    "validate route kind with same level" in {
      RouteTrie(
        "/test/:file" -> (GET, 0),
        "/test/*file" -> (GET, 1)
      ) must throwA[IllegalArgumentException]
    }

    "validate route kind name with same level" in {
      RouteTrie(
        "/test/:file" -> (GET, 0),
        "/test/:file2" -> (GET, 1)
      ) must throwA[IllegalArgumentException]
    }

    "validate multiple parameters" in {
      RouteTrie(
        "/test/:first_:second" -> (GET, 0)
      ) must throwA[IllegalArgumentException]
    }

    "validate parameters with same type and name" in {
      RouteTrie(
        "/test/:first/:first" -> (GET, 0)
      ) must throwA[IllegalArgumentException]
    }

    "validate parameters with similar name" in {
      RouteTrie(
        "/test/:first/*first" -> (GET, 0)
      ) must throwA[IllegalArgumentException]
    }

    "normalize URIs" in {
      RouteTrie.normalizeUri("") must_== "/"
      RouteTrie.normalizeUri("./") must_== "/"
      RouteTrie.normalizeUri("/.") must_== "/"
      RouteTrie.normalizeUri("aa/") must_== "/aa"
      RouteTrie.normalizeUri("aa") must_== "/aa"
      RouteTrie.normalizeUri("/a/./") must_== "/a"
      RouteTrie.normalizeUri(".////b") must_== "/b"
    }

    "remove URIs" in {
      val tree = RouteTrie(
        "/user" -> (GET, 1),
        "/userify" -> (GET, 2),
        "/user/about" -> (GET, 3),
        "/user/:id/x" -> (GET, 4),
        "/user/:id/x/:xid" -> (GET, 12),
        "/user/:id/x/:xid" -> (POST, 13),
        "/user/:id" -> (GET, 5),
        "/useragent" -> (GET, 6),
        "/url/static" -> (GET, 7),
        "/user/g/h/n" -> (GET, 8),
        "/f/*file" -> (GET, 9),
        "/урл/тест/DFSD©Δ§ß∞¢" -> (GET, 10),
        "test" -> (GET, 12),
        "/:wildcard" -> (GET, 11)
      )

      var ntree = tree.remove("/user/:id", GET, 5)
      ntree.get(GET, "/user/12") must_== None

      ntree = ntree.remove("/unknown_uri", GET, 0)

      ntree.get(GET, "/user/12/x/333") must_== Some(12, Some(OpenHashMap("id" -> "12", "xid" -> "333")))
      ntree = ntree.remove("/user/:id/x/:xid", GET, 12)
      ntree.get(GET, "/user/12/x/333") must_== None
      ntree.get(POST, "/user/12/x/444/") must_== Some(13, Some(OpenHashMap("id" -> "12", "xid" -> "444")))

      ntree = ntree.remove("/user", GET, 1)
      ntree.get(GET, "/user") must_== None
      ntree.get(POST, "/user/12/x/444/") must_== Some(13, Some(OpenHashMap("id" -> "12", "xid" -> "444")))
      ntree.get(GET, "/user/about") must_== Some(3, None)

      ntree = ntree.remove("/url", GET, 7)
      ntree.get(GET, "/url/static") must_== Some(7, None)
      ntree = ntree.remove("/url/static", POST, 7)
      ntree.get(GET, "/url/static") must_== Some(7, None)

      ntree = ntree.remove("/f/*file", GET, 9)
      ntree.get(GET, "/f/тест/DFSD©Δ§ß∞¢") must_== None

      ntree.get(GET, "/user/about") must_== Some(3, None)
      ntree.get(GET, "/урл/тест/DFSD©Δ§ß∞¢") must_== Some(10, None)
      ntree.get(GET, "/test") must_== Some(12, None)
      ntree.get(GET, "/no_url") must_== Some(11, Some(OpenHashMap("wildcard" -> "no_url")))

      ntree.size must_== 10


      tree.size must_== 14
      tree.get(GET, "/user") must_== Some(1, None)
      tree.get(GET, "/user/12") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12/") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12//") must_== None
      tree.get(GET, "/user/12/x/333") must_== Some(12, Some(OpenHashMap("id" -> "12", "xid" -> "333")))
      tree.get(POST, "/user/12/x/444/") must_== Some(13, Some(OpenHashMap("id" -> "12", "xid" -> "444")))
      tree.get(GET, "/user/about") must_== Some(3, None)
      tree.get(GET, "/url/static") must_== Some(7, None)
      tree.get(POST, "/url/static") must_== None
      tree.get(GET, "user") must_== None
      tree.get(GET, "/f/тест/DFSD©Δ§ß∞¢") must_== Some(9, Some(OpenHashMap("file" -> "тест/DFSD©Δ§ß∞¢")))
      tree.get(GET, "/урл/тест/DFSD©Δ§ß∞¢") must_== Some(10, None)
      tree.get(GET, "/test") must_== Some(12, None)
      tree.get(GET, "/no_url") must_== Some(11, Some(OpenHashMap("wildcard" -> "no_url")))
    }

    "get methods value" in {
      val tree = RouteTrie(
        "/user" -> (GET, 1),
        "/userify" -> (GET, 2),
        "/user/about" -> (GET, 3),
        "/user/:id/x" -> (GET, 4),
        "/user/:id/x/:xid" -> (GET, 12),
        "/user/:id/x/:xid" -> (POST, 13),
        "/user/:id" -> (GET, 5),
        "/useragent" -> (GET, 6),
        "/url/static" -> (GET, 7),
        "/user/g/h/n" -> (GET, 8),
        "/f/*file" -> (GET, 9),
        "/урл/тест/DFSD©Δ§ß∞¢" -> (GET, 10),
        "test" -> (GET, 12),
        "/:wildcard" -> (GET, 11)
      )
      tree.getMethodValue(GET, "/user") must_== Some(1)
      tree.getMethodValue(GET, "/user/") must_== Some(1)
      tree.getMethodValue(GET, "/user/about") must_== Some(3)
      tree.getMethodValue(GET, "/user/:id/x") must_== Some(4)
      tree.getMethodValue(POST, "/user/:id/x") must_== None
      tree.getMethodValue(GET, "/f/*file") must_== Some(9)
      tree.getMethodValue(GET, "/:wildcard") must_== Some(11)
      tree.getMethodValue(GET, "/no_url") must_== None
    }
  }
}
