package io.fcomb.trie

import java.net.URI
import scala.annotation.tailrec
import scala.collection.generic.{ CanBuildFrom, ImmutableMapFactory }
import scala.collection.mutable.{ LongMap, OpenHashMap }

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
  sealed trait RouteMethod {
    val id: Int
  }

  @SerialVersionUID(1L)
  case object GET extends RouteMethod {
    val id = 0
  }

  @SerialVersionUID(1L)
  case object POST extends RouteMethod {
    val id = 1
  }

  @SerialVersionUID(1L)
  case object PUT extends RouteMethod {
    val id = 2
  }

  @SerialVersionUID(1L)
  case object DELETE extends RouteMethod {
    val id = 3
  }

  @SerialVersionUID(1L)
  case object PATCH extends RouteMethod {
    val id = 4
  }

  @SerialVersionUID(1L)
  case object OPTIONS extends RouteMethod {
    val id = 5
  }

  @SerialVersionUID(1L)
  case object HEAD extends RouteMethod {
    val id = 6
  }

  private[trie] val RouteMethodsCount = 7
}
import RouteMethods._

@SerialVersionUID(1L)
case class RouteNode[T](
    key:            String,
    kind:           RouteKind,
    values:         Array[Any]            = null,
    children:       LongMap[RouteNode[T]] = null,
    childParameter: RouteNode[T]          = null
) extends Traversable[(String, (RouteMethod, T))] {
  require(key.nonEmpty, s"Key can't be empty: $this")
  if (kind.isInstanceOf[WildcardRoute])
    require(key.head == '*', s"Wildcard route node must start with prefix symbol '*': $this")
  if (kind.isInstanceOf[ParameterRoute])
    require(key.head == ':', s"Parameter route node must start with prefix symbol ':': $this")
  if (kind == StaticRoute)
    require(!key.exists(nonStaticPrefix), s"Static route node must not start with prefix symbol ':' or '*': $this")

  def get(m: RouteMethod, k: String) = {
    val res = getTrie(k)
    if (res != null && res._1.values != null) {
      val v = res._1.values(m.id)
      if (v == null) None
      else Some(v, Option(res._2))
    } else None
  }

  def getRaw(m: RouteMethod, k: String) = {
    val res = getTrie(k)
    if (res != null && res._1.values != null) {
      val v = res._1.values(m.id)
      if (v == null) null
      else (v, res._2)
    } else null
  }

  private def getTrie(
    k:      String,
    params: OpenHashMap[String, String] = null
  ): (RouteNode[T], OpenHashMap[String, String]) = {
    @inline def getParams() =
      if (params == null) OpenHashMap.empty[String, String]
      else params

    val res =
      if ((kind == StaticRoute && k == key) || k == "/") (this, params)
      else if (kind == StaticRoute && k.startsWith(key)) {
        if (children == null) null
        else children.get(k(key.length)) match {
          case Some(n) => n.getTrie(k.substring(key.length), params)
          case _       => null
        }
      } else if (kind.isInstanceOf[ParameterRoute]) {
        if (children == null) null
        else children.get(k.head) match {
          case Some(n) => n.getTrie(k, params)
          case _       => null
        }
      } else null

    if (res != null) res
    else {
      if (childParameter != null && k.startsWith(key)) {
        val params = getParams
        childParameter.kind match {
          case WildcardRoute(name) =>
            params += ((name, k.substring(key.length)))
            (childParameter, params)
          case ParameterRoute(name) =>
            val pos = k.indexOf('/', key.length)
            val param =
              if (pos > key.length) k.substring(key.length, pos)
              else k.substring(key.length)
            params += ((name, param))
            val offset = param.length + key.length
            if (k.length > offset)
              childParameter.getTrie(k.substring(offset), params)
            else (childParameter, params)
        }
      } else null
    }
  }

  @inline
  private def nonStaticPrefix(c: Char): Boolean =
    c == ':' || c == '*'

  @inline
  private def nonStaticPrefix(k: String): Boolean =
    nonStaticPrefix(k.head)

  def `+`(kv: (String, (RouteMethod, T))): RouteNode[T] = kv match {
    case (kk, (m, v)) =>
      require(kk.nonEmpty, s"Key can't be empty: $kv")

      val k =
        if (kk.length > 1 && kk.last == '/') kk.dropRight(1)
        else kk

      if (nonStaticPrefix(k)) addToChildParameter(m, k, v)
      else {
        if (k == key) {
          val nValues =
            if (values == null) new Array[Any](RouteMethodsCount)
            else values
          nValues(m.id) = v
          RouteNode(k, kind, nValues, children, childParameter)
        } else if (k.startsWith(key)) {
          val newKey = k.substring(key.length)
          if (nonStaticPrefix(newKey)) addToChildParameter(m, newKey, v)
          else {
            val child = newKey(0).toLong -> addToChildren(m, newKey, v)
            if (children == null) this.copy(children = LongMap(child))
            else this.copy(children = children += child)
          }
        } else if (key.startsWith(k)) {
          val newKey = key.substring(k.length)
          val cc = LongMap(
            newKey(0).toLong -> RouteNode(newKey, kind, values, children, childParameter)
          )
          val nValues = new Array[Any](RouteMethodsCount)
          nValues(m.id) = v
          RouteNode(k, StaticRoute, nValues, cc)
        } else {
          val newKey = longestCommonPart(k, key)
          val k1 = key.substring(newKey.length)
          val k2 = k.substring(newKey.length)
          val cc = LongMap(
            k1(0).toLong -> RouteNode(k1, kind, values, children, childParameter)
          )
          val rn = RouteNode[T](newKey, StaticRoute, null, cc)
          if (nonStaticPrefix(k2)) {
            rn.addToChildParameter(m, k2, v)
            rn
          } else {
            val nValues = new Array[Any](RouteMethodsCount)
            nValues(m.id) = v
            val child = k2(0).toLong -> RouteNode[T](k2, StaticRoute, nValues)
            if (rn.children == null) rn.copy(children = LongMap(child))
            else rn.copy(children = rn.children += child)
          }
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
      case null => RouteNode[T](keyName, route)
      case node =>
        require(node.kind == route, s"Conflict on ${node.kind} =!= $route")

        node
    }
    val cn =
      if (keyPostfix.isEmpty || route.isInstanceOf[WildcardRoute]) {
        val cpValues =
          if (cp.values == null) new Array[Any](RouteMethodsCount)
          else cp.values
        cpValues(m.id) = v
        cp.copy(values = cpValues)
      } else {
        val child = (keyPostfix(0).toLong -> cp.addToChildren(m, keyPostfix, v))
        if (cp.children == null) cp.copy(children = LongMap(child))
        else cp.copy(children = cp.children += child)
      }

    this.copy(childParameter = cn)
  }

  private def addToChildren(m: RouteMethod, k: String, v: T): RouteNode[T] = {
    require(!kind.isInstanceOf[WildcardRoute], s"Wildcard cannot contain nested routes: $k")

    val n =
      if (children == null) null
      else children.get(k(0)).getOrElse(null)
    if (n == null) {
      val keyPrefix = k.takeWhile(!nonStaticPrefix(_))
      if (keyPrefix.length == k.length) {
        val nValues = new Array[Any](RouteMethodsCount)
        nValues(m.id) = v
        RouteNode(keyPrefix, StaticRoute, nValues)
      } else if (keyPrefix.nonEmpty)
        RouteNode(keyPrefix, StaticRoute)
          .addToChildParameter(m, k.substring(keyPrefix.length), v)
      else addToChildParameter(m, k, v)
    } else n.+(k -> (m, v))
  }

  private def longestCommonPart(a: String, b: String): String = {
    val minLength = Math.min(a.length, b.length)

    @tailrec
    def f(pos: Int): String = {
      if (pos < minLength && a.charAt(pos) == b.charAt(pos)) f(pos + 1)
      else a.substring(0, pos)
    }
    f(0)
  }

  def repack(): RouteNode[T] = {
    val children = Option(this.children)
      .map(_.map { case (k, v) => (k, v.repack) })
      .getOrElse(null)
    val childParameter = Option(this.childParameter)
      .map(_.repack)
      .getOrElse(null)
    this.copy(
      children = children,
      childParameter = childParameter
    )
  }

  def foreach[U](f: ((String, (RouteMethod, T))) => U): Unit = {
    foreach(f, "")
  }

  private def foreach[U](f: ((String, (RouteMethod, T))) => U, keyPrefix: String): Unit = {
    val fullKey = keyPrefix + key
    Option(values).foreach { values =>
      List(GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS).foreach { m =>
        Option(values(m.id)).foreach { v =>
          f(fullKey -> (m, v.asInstanceOf[T]))
        }
      }
    }
    Option(children).foreach { c =>
      Option(c.values).foreach(_.foreach(_.foreach(f, fullKey)))
    }
    Option(childParameter).foreach(_.foreach(f, fullKey))
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: RouteNode[T] => that.toMap == this.toMap
    case _                  => false
  }

  private def toString(padding: Int): String = {
    val p = "  " * 2 * padding
    val childrenS =
      if (children == null) ""
      else children
        .map { case (k, v) => s"$p${v.toString(padding + 1)}" }
        .mkString
    val childS =
      if (childParameter == null) ""
      else childParameter.toString(padding + 1)
    val valuesS =
      if (values == null) ""
      else values.filter(_ != null).map(_.asInstanceOf[T]).mkString(";")

    s"""
$p$key -> [$valuesS], $kind =>
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
    }.repack
}
