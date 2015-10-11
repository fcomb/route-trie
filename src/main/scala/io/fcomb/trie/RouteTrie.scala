package io.fcomb.trie

import java.net.URI
import scala.annotation.tailrec
import scala.collection.generic.{CanBuildFrom, ImmutableMapFactory}
import scala.collection.mutable.OpenHashMap
import scala.collection.immutable.{LongMap, HashSet}
import scala.util.{Try, Success, Failure}

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
  key: String,
  kind: RouteKind,
  values: Array[Any] = null,
  children: LongMap[RouteNode[T]] = null,
  childParameter: RouteNode[T] = null
) extends Traversable[(String, (RouteMethod, T))] {
  require(key.nonEmpty, s"Key can't be empty: $this")
  kind match {
    case _: WildcardRoute =>
      require(key.head == '*', s"Wildcard route node must start with prefix symbol '*': $this")
    case _: ParameterRoute =>
      require(key.head == ':', s"Parameter route node must start with prefix symbol ':': $this")
    case StaticRoute =>
      require(!nonStaticPrefix(key), s"Static route node must not start with prefix symbol ':' or '*': $this")
  }
  require(
    !Option(childParameter).exists(_ == StaticRoute),
    s"child parameter node kind can't be StaticRoute: $this"
  )

  def get(m: RouteMethod, k: String): Option[(T, Option[OpenHashMap[String, String]])] = {
    val res = getTrie(k)
    if (res != null && res._1.values != null) {
      val v = res._1.values(m.id)
      if (v == null) None
      else Some(v.asInstanceOf[T], Option(res._2))
    } else None
  }

  def getRaw(m: RouteMethod, k: String): (T, OpenHashMap[String, String]) = {
    val res = getTrie(k)
    if (res != null && res._1.values != null) {
      val v = res._1.values(m.id)
      if (v == null) null
      else (v.asInstanceOf[T], res._2)
    } else null
  }

  private def getTrie(
    k: String,
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
          case _ => null
        }
      } else if (kind.isInstanceOf[ParameterRoute]) {
        if (children == null) null
        else children.get(k.head) match {
          case Some(n) => n.getTrie(k, params)
          case _ => null
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
          case StaticRoute =>
            throw new IllegalArgumentException
        }
      } else null
    }
  }

  def getMethodValue(m: RouteMethod, k: String): Option[T] =
    getNode(normalizePath(k)).flatMap { n =>
      Option(n.values).flatMap(v => Option(v(m.id)).map(_.asInstanceOf[T]))
    }

  def getNode(k: String): Option[RouteNode[T]] = {
    if (k == key || k == "/") Some(this)
    else if (k.startsWith(key)) {
      val kk = k.substring(key.length)
      Option(children).flatMap(_.get(k(key.length)).flatMap { n =>
        n.getNode(kk)
      }) match {
        case r @ Some(_) => r
        case None => Option(childParameter).flatMap(_.getNode(kk))
      }
    } else None
  }

  @inline
  private def nonStaticPrefix(c: Char): Boolean =
    c == ':' || c == '*'

  @inline
  private def nonStaticPrefix(k: String): Boolean =
    nonStaticPrefix(k.head)

  @inline
  private def normalizePath(p: String) =
    if (p.length > 1 && p.last == '/') p.dropRight(1)
    else p

  private def addChild(child: (Long, RouteNode[T])): LongMap[RouteNode[T]] =
    Option(children).getOrElse(LongMap.empty) + child

  private def `+`(kv: (String, (RouteMethod, T))): RouteNode[T] = kv match {
    case (path, (m, v)) =>
      require(path.nonEmpty, s"Key can't be empty: $kv")

      val k = normalizePath(path)
      if (nonStaticPrefix(k)) addToChildParameter(m, k, v)
      else {
        if (k == key) {
          val nValues = Option(values).map(_.clone)
            .getOrElse(new Array[Any](RouteMethodsCount))
          nValues(m.id) = v
          RouteNode(k, kind, nValues, children, childParameter)
        } else if (k.startsWith(key)) {
          val newKey = k.substring(key.length)
          if (nonStaticPrefix(newKey)) addToChildParameter(m, newKey, v)
          else this.copy(children = addChild(newKey(0), addToChildren(m, newKey, v)))
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
          if (nonStaticPrefix(k2)) rn.addToChildParameter(m, k2, v)
          else {
            val nValues = new Array[Any](RouteMethodsCount)
            nValues(m.id) = v
            rn.copy(
              children = rn.addChild(k2(0), RouteNode[T](k2, StaticRoute, nValues))
            )
          }
        }
      }
  }

  def `+`(k: String, m: RouteMethod, v: T): RouteNode[T] = {
    val uri = RouteTrie.normalizeUri(k)
    RouteTrie.validateUri(uri) match {
      case Left(e) => throw new IllegalArgumentException(e)
      case Right(_) => this + (uri, (m, v))
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
    val cp = Option(childParameter).getOrElse(RouteNode[T](keyName, route))
    require(cp.kind == route, s"Conflict on ${cp.kind} =!= $route")

    val cn =
      if (keyPostfix.isEmpty || route.isInstanceOf[WildcardRoute]) {
        val cpValues = Option(cp.values).map(_.clone)
          .getOrElse(new Array[Any](RouteMethodsCount))
        cpValues(m.id) = v
        cp.copy(values = cpValues)
      } else cp.copy(
        children = cp.addChild(keyPostfix(0), cp.addToChildren(m, keyPostfix, v))
      )

    this.copy(childParameter = cn)
  }

  private def addToChildren(m: RouteMethod, k: String, v: T): RouteNode[T] = {
    require(!kind.isInstanceOf[WildcardRoute], s"Wildcard cannot contain nested routes: $k")

    val n =
      if (children == null) null
      else children.get(k.head).getOrElse(null)
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

  def `-`(kv: (String, (RouteMethod, T))): RouteNode[T] = kv match {
    case (k, (m, v)) =>
      def fNested(path: String, n: RouteNode[T]): RouteNode[T] =
        if (nonStaticPrefix(path)) n.copy(
          childParameter = f(path, n.childParameter).getOrElse(null)
        )
        else if (n.children != null) {
          n.children.get(path.head) match {
            case Some(cn) =>
              val nc = f(path, cn) match {
                case Some(rn) =>
                  n.children + ((path.head, rn))
                case _ =>
                  n.children - path.head
              }
              n.copy(children = nc)
            case _ => n
          }
        } else n

      def f(path: String, n: RouteNode[T]): Option[RouteNode[T]] = {
        if (n == null) None
        else {
          val key = n.key
          val rn =
            if (nonStaticPrefix(path)) {
              val keyName = path.takeWhile(_ != '/')
              val keyPostfix = path.substring(keyName.length)
              if (keyName == key) {
                if (keyPostfix.isEmpty && n.values != null && n.values(m.id) == v) {
                  val nn = n.copy(values = Array[Any](n.values: _*))
                  nn.values(m.id) = null
                  nn
                } else if (keyPostfix.nonEmpty)
                  fNested(keyPostfix, n)
                else n
              } else n
            } else {
              if (path == key) {
                if (n.values != null && n.values(m.id) == v) {
                  val nn = n.copy(values = Array[Any](n.values: _*))
                  nn.values(m.id) = null
                  nn
                } else n
              } else if (path.startsWith(key))
                fNested(path.drop(key.length), n)
              else n
            }

          if ((rn.children == null || rn.children.isEmpty) &&
            (rn.childParameter == null) &&
            (rn.values == null || rn.values.forall(_ == null))) None
          else Some(rn)
        }
      }

      val path = normalizePath(k) match {
        case p if p.length > 1 && p.head != '/' => s"/$p"
        case p => p
      }
      f(path, this) match {
        case Some(res) => res
        case _ => RouteTrie.empty
      }
  }

  def remove(k: String, m: RouteMethod, v: T) =
    this - (k, (m, v))

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
    case _ => false
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
      case (t, (k, (m, v))) => t + (k, m ,v)
    }.repack

  def normalizeUri(s: String): String = {
    if (s.isEmpty) "/"
    else {
      val uri = new URI(s).normalize.getRawPath
      val path =
        if (uri.length > 1 && uri.last == '/') uri.dropRight(1)
        else uri
      if (path.headOption == Some('/')) path
      else "/".concat(path)
    }
  }

  private val nameFormat = """(:|\*)([\w\-]+)\z""".r

  def validateUri(u: String): Either[String, HashSet[String]] = {
    Try(new URI(u).normalize.getRawPath) match {
      case Success(uri) =>
        if (uri.isEmpty) Left(s"URI can't be empty: $uri")
        else if (uri.head != '/') Left(s"URI must start with prefix symbol '/': $uri")
        else uri.split('/').drop(1).foldLeft[Either[String, HashSet[String]]](Right(HashSet.empty[String])) {
          case (Right(s), p) =>
            val pi = p.count(c => c == ':' || c == '*')
            if (pi == 0) Right(s)
            else if (pi > 1) Left(s"Multiple parameters cannot be nested in the same path part: $p")
            else nameFormat.findFirstMatchIn(p.dropWhile(c => c != ':' && c != '*')) match {
              case Some(m) =>
                val name = m.group(2)
                if (s.contains(name)) Left(s"Duplicate parameter name: $name")
                else Right(s + name)
              case None => Left("Invalid format of parameter name")
            }
          case (e @ Left(_), _) => e
        }
      case Failure(e) => Left(e.getMessage)
    }
  }
}
