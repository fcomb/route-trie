package io.fcomb.trie

import scala.collection.mutable.OpenHashMap
import org.specs2.mutable._

class RouteTrieSpec extends Specification {
  "RouteTrie" should {
    "match URLs" in {
      val tree = RouteTrie(
        "/user" -> 1,
        "/userify" -> 2,
        "/user/about" -> 3,
        "/user/:id/x" -> 4,
        "/user/:id/x/:xid" -> 12,
        "/user/:id" -> 5,
        "/useragent" -> 6,
        "/ua/kek" -> 7,
        "/user/g/h/n" -> 8,
        "/f/*file" -> 9,
        "/урл/тест/DFSD©Δ§ß∞¢" -> 10,
        "/:kek" -> 11
      )
      tree.size must_== 12

      tree.get("/user") must_== Some(1, None)
      tree.get("/user/12") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get("/user/12/") must_== Some(5, Some(OpenHashMap("id" -> "12")))
      tree.get("/user/12//") must_== None
      tree.get("/user/12/x/333") must_== Some(12, Some(OpenHashMap("id" -> "12", "xid" -> "333")))
      tree.get("/user/about") must_== Some(3, None)
      tree.get("/ua/kek") must_== Some(7, None)
      tree.get("user") must_== None
      tree.get("/f/тест/DFSD©Δ§ß∞¢") must_== Some(9, Some(OpenHashMap("file" -> "тест/DFSD©Δ§ß∞¢/")))
      tree.get("/урл/тест/DFSD©Δ§ß∞¢") must_== Some(10, None)
      tree.get("/no_url") must_== Some(11, Some(OpenHashMap("kek" -> "no_url")))
    }

    "validate URL prefix" in {
      def tree = RouteTrie("test" -> 0)
      tree.size must throwA[IllegalArgumentException]
    }

    "validate wildcard children" in {
      def tree = RouteTrie(
        "/test/*file" -> 0,
        "/test/*file/something" -> 1
      )
      tree.size must throwA[IllegalArgumentException]
    }

    "validate route kind with same level" in {
      def tree = RouteTrie(
        "/test/:file" -> 0,
        "/test/*file" -> 1
      )
      tree.size must throwA[IllegalArgumentException]
    }

    "validate route kind name with same level" in {
      def tree = RouteTrie(
        "/test/:file" -> 0,
        "/test/:file2" -> 1
      )
      tree.size must throwA[IllegalArgumentException]
    }
  }
}
