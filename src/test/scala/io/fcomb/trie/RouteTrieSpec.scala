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
        "/ua/kek" -> (GET, 7),
        "/user/g/h/n" -> (GET, 8),
        "/f/*file" -> (GET, 9),
        "/урл/тест/DFSD©Δ§ß∞¢" -> (GET, 10),
        "/:kek" -> (GET, 11)
      )
      tree.size must_== 13

      tree.get(GET, "/user") must_== Some(1, None)
      tree.get(GET, "/user/12") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12/") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get(GET, "/user/12//") must_== None
      tree.get(GET, "/user/12/x/333") must_== Some(12, Some(OpenHashMap("id" -> "12", "xid" -> "333")))
      tree.get(POST, "/user/12/x/444") must_== Some(13, Some(OpenHashMap("id" -> "12", "xid" -> "444")))
      tree.get(GET, "/user/about") must_== Some(3, None)
      tree.get(GET, "/ua/kek") must_== Some(7, None)
      tree.get(POST, "/ua/kek") must_== None
      tree.get(GET, "user") must_== None
      tree.get(GET, "/f/тест/DFSD©Δ§ß∞¢") must_== Some(9, Some(OpenHashMap("file" -> "тест/DFSD©Δ§ß∞¢/")))
      tree.get(GET, "/урл/тест/DFSD©Δ§ß∞¢") must_== Some(10, None)
      tree.get(GET, "/no_url") must_== Some(11, Some(OpenHashMap("kek" -> "no_url")))
    }

    "validate URI prefix" in {
      def tree = RouteTrie("test" -> (GET, 0))
      tree.size must throwA[IllegalArgumentException]
    }

    "validate wildcard children" in {
      def tree = RouteTrie(
        "/test/*file" -> (GET, 0),
        "/test/*file/something" -> (GET, 1)
      )
      tree.size must throwA[IllegalArgumentException]
    }

    "validate route kind with same level" in {
      def tree = RouteTrie(
        "/test/:file" -> (GET, 0),
        "/test/*file" -> (GET, 1)
      )
      tree.size must throwA[IllegalArgumentException]
    }

    "validate route kind name with same level" in {
      def tree = RouteTrie(
        "/test/:file" -> (GET, 0),
        "/test/:file2" -> (GET, 1)
      )
      tree.size must throwA[IllegalArgumentException]
    }
  }
}
