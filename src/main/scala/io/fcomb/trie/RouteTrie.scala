package io.fcomb.trie

import java.net.URI
import scala.annotation.tailrec
import scala.collection.generic.{ CanBuildFrom, ImmutableMapFactory }
import scala.collection.mutable.OpenHashMap

private[trie] object RouteKinds {
  sealed trait RouteKind

  @SerialVersionUID(1L)
  case object StaticRoute extends RouteKind // /uri

  @SerialVersionUID(1L)
  case class WildcardRoute( // /*param
      name: String
  ) extends RouteKind {
    require(name.nonEmpty, "name can't be empty")
  }

  @SerialVersionUID(1L)
  case class ParameterRoute( // /:param
      name: String
  ) extends RouteKind {
    require(name.nonEmpty, "name can't be empty")
  }
}
import RouteKinds._

object RouteMethods {
  sealed trait RouteMethod

  @SerialVersionUID(1L)
  case object GET extends RouteMethod

  @SerialVersionUID(1L)
  case object OPTIONS extends RouteMethod

  @SerialVersionUID(1L)
  case object POST extends RouteMethod

  @SerialVersionUID(1L)
  case object PUT extends RouteMethod

  @SerialVersionUID(1L)
  case object PATCH extends RouteMethod

  @SerialVersionUID(1L)
  case object DELETE extends RouteMethod

  @SerialVersionUID(1L)
  case object HEAD extends RouteMethod
}
import RouteMethods._

@SerialVersionUID(1L)
private[trie] case class RouteNode[T](
    key:            String,
    kind:           RouteKind,
    values:         OpenHashMap[RouteMethod, T]     = OpenHashMap.empty[RouteMethod, T],
    children:       OpenHashMap[Char, RouteNode[T]] = OpenHashMap.empty[Char, RouteNode[T]],
    childParameter: Option[RouteNode[T]]            = None
) extends Traversable[(String, (RouteMethod, T))] {
  require(key.nonEmpty, s"Key can't be empty: $this")
  if (kind.isInstanceOf[WildcardRoute])
    require(key.head == '*', s"Wildcard route node must start with prefix symbol '*': $this")
  if (kind.isInstanceOf[ParameterRoute])
    require(key.head == ':', s"Parameter route node must start with prefix symbol ':': $this")
  if (kind == StaticRoute)
    require(!key.exists(nonStaticPrefix), s"Static route node must not start with prefix symbol ':' or '*': $this")

  def get(m: RouteMethod, k: String) =
    getTrie(m, cleanKey(k)) match {
      case Some((RouteNode(_, _, hm, _, _), p)) => hm.get(m) match {
        case Some(v) => Some(v, p)
        case _       => None
      }
      case _ => None
    }

  private def getTrie(
    m:      RouteMethod,
    k:      String,
    params: Option[OpenHashMap[String, String]] = None
  ): Option[(RouteNode[T], Option[OpenHashMap[String, String]])] = {
    @inline def getParams() = params match {
      case Some(m) => m
      case _       => OpenHashMap.empty[String, String]
    }

    val res =
      if (k == key) Some((this, params))
      else if (kind == StaticRoute && k.startsWith(key)) {
        children.get(k(key.length)) match {
          case Some(n) => n.getTrie(m, k.substring(key.length), params)
          case _       => None
        }
      } else if (kind.isInstanceOf[ParameterRoute]) {
        children.get(k.head) match {
          case Some(n) => n.getTrie(m, k, params)
          case _       => None
        }
      } else None

    if (res.nonEmpty) res
    else childParameter match {
      case Some(n) if k.startsWith(key) =>
        val params = getParams
        n.kind match {
          case WildcardRoute(name) =>
            params += ((name, k.substring(key.length)))
            Some((n, Some(params)))
          case ParameterRoute(name) =>
            val pos = k.indexOf('/', key.length)
            val param =
              if (pos > key.length) k.substring(key.length, pos)
              else k.substring(key.length)
            params += ((name, param))
            val offset = param.length + key.length
            if (k.length > offset)
              n.getTrie(m, k.substring(offset), Some(params))
            else Some((n, Some(params)))
        }
      case _ => None
    }
  }

  @inline
  private def cleanKey(k: String) =
    if (k(k.length - 1) != '/') k.concat("/") else k

  @inline
  private def nonStaticPrefix(c: Char): Boolean =
    c == ':' || c == '*'

  @inline
  private def nonStaticPrefix(k: String): Boolean =
    nonStaticPrefix(k.head)

  def `+`(kv: (String, (RouteMethod, T))): RouteNode[T] = {
    require(kv._1.nonEmpty, s"Key can't be empty: $kv")

    val k = cleanKey(kv._1)
    val m = kv._2._1
    val v = kv._2._2

    if (nonStaticPrefix(k)) addToChildParameter(m, k, v)
    else {
      if (k == key)
        RouteNode(k, kind, values += ((m, v)), children, childParameter)
      else if (k.startsWith(key)) {
        val newKey = k.substring(key.length)
        if (nonStaticPrefix(newKey)) addToChildParameter(m, newKey, v)
        else {
          children += (newKey(0) -> addToChildren(m, newKey, v))
          this
        }
      } else if (key.startsWith(k)) {
        val newKey = key.substring(k.length)
        val cc = OpenHashMap(
          newKey(0) -> RouteNode(newKey, kind, values, children, childParameter)
        )
        RouteNode(k, StaticRoute, OpenHashMap(m -> v), cc, None)
      } else {
        val newKey = longestCommonPart(k, key)
        val k1 = key.substring(newKey.length)
        val k2 = k.substring(newKey.length)
        val cc = OpenHashMap(
          k1(0) -> RouteNode(k1, kind, values, children, childParameter),
          k2(0) -> RouteNode(k2, StaticRoute, OpenHashMap(m -> v))
        )
        RouteNode(newKey, StaticRoute, OpenHashMap.empty, cc, None)
      }
    }
  }

  private def addToChildParameter(m: RouteMethod, k: String, v: T) = {
    require(nonStaticPrefix(k), s"Key '$k' must start with ':' or '*'")
    require(kind == StaticRoute, s"$kind cannot contain nested routes: $k")

    val keyName = k.takeWhile(_ != '/')
    val keyPostfix = k.substring(keyName.length)
    val route = keyName.head match {
      case ':' => ParameterRoute(keyName.substring(1))
      case '*' =>
        require(keyPostfix.isEmpty || keyPostfix == "/", s"Wildcard cannot contain nested routes: $keyPostfix")

        WildcardRoute(keyName.substring(1))
    }
    val cp = childParameter match {
      case Some(node) =>
        require(node.kind == route, s"Conflict on ${node.kind} =!= $route")

        node
      case None => RouteNode[T](keyName, route)
    }
    val cn =
      if (route.isInstanceOf[WildcardRoute])
        Some(cp.copy(values = cp.values += ((m, v))))
      else {
        cp.children += (keyPostfix(0) -> cp.addToChildren(m, keyPostfix, v))
        Some(cp)
      }

    this.copy(childParameter = cn)
  }

  private def addToChildren(m: RouteMethod, k: String, v: T): RouteNode[T] = {
    require(!kind.isInstanceOf[WildcardRoute], s"Wildcard cannot contain nested routes: $k")

    children.get(k(0)) match {
      case Some(n) => n + (k -> (m, v))
      case None =>
        val keyPrefix = k.takeWhile(!nonStaticPrefix(_))
        if (keyPrefix.length == k.length)
          RouteNode(keyPrefix, StaticRoute, OpenHashMap(m -> v))
        else if (keyPrefix.nonEmpty)
          RouteNode(keyPrefix, StaticRoute)
            .addToChildParameter(m, k.substring(keyPrefix.length), v)
        else addToChildParameter(m, k, v)
    }
  }

  private def longestCommonPart(a: String, b: String) = {
    @tailrec @inline
    def f(
      a:          String,
      b:          String,
      index:      Int,
      commonPart: StringBuffer
    ): String = {
      (a.headOption, b.headOption) match {
        case (Some(c1), Some(c2)) if c1 == c2 =>
          f(a.substring(1), b.substring(1), index + 1, commonPart.append(c1))
        case _ => commonPart.toString
      }
    }

    f(a, b, 0, new StringBuffer)
  }

  def foreach[U](f: ((String, (RouteMethod, T))) => U): Unit = {
    foreach(f, "")
  }

  private def foreach[U](f: ((String, (RouteMethod, T))) => U, keyPrefix: String): Unit = {
    val fullKey = keyPrefix + key
    values.foreach(v => f(fullKey -> v))
    children.foreach {
      case (_, c: RouteNode[T]) => c.foreach(f, fullKey)
    }
    childParameter.foreach(_.foreach(f, fullKey))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: RouteNode[T] => that.toMap == this.toMap
    case _                  => false
  }

  private def toString(padding: Int): String = {
    val p = "  " * 2 * padding
    val childrenS = children
      .map { case (k, v) => s"$p${v.toString(padding + 1)}" }
      .mkString
    val childS = childParameter
      .map(_.toString(padding + 1))
      .getOrElse("")

    s"""
$p$key -> $values, $kind =>
$p  children: $childrenS
$p  childParameter: $childS
""".stripMargin
  }

  override def toString(): String = toString(0)
}

object RouteTrie {
  def empty[T]: RouteNode[T] = RouteNode("/", StaticRoute)

  def apply[T](kvs: (String, (RouteMethod, T))*): RouteNode[T] =
    kvs.foldLeft[RouteNode[T]](empty) {
      case (t, (k, v)) =>
        require(k.nonEmpty, s"Url can't be empty: $v")
        require(k.head == '/', s"Url must start with prefix symbol '/': $v")

        val uri = new URI(k)
        t + (uri.getRawPath, v)
    }
}
