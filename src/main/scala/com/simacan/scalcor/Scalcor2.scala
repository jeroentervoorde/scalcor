package com.simacan.scalcor

import com.simacan.scalcor.Scalcor2.Filters.BoolExpr
import com.simacan.scalcor.Scalcor2.Shapes.ObjectShape
import play.api.libs.json._

import scala.reflect.runtime.universe._
/**
 * Created by jeroen on 12/8/15.
 */
object Scalcor2 extends App {

  trait Type[T]

  abstract class ObjectType[T] extends Type[T]
  case class Field[T](name: String) extends Type[T]

  object TupleTypes {
    class Tuple2Type[A, B](t: (A,B)) extends Type[(A,B)]

    implicit def tuple2Type[A,B](t: (A,B)) = new Tuple2Type[A,B](t)
  }

  abstract class Shape[T : Reads] {
    def fromJson(json: JsValue) : JsResult[T]
  }

  object Shapes {
    case class FieldShape[T : Reads](field: Field[T]) extends Shape[T] {
      override def fromJson(json: JsValue): JsResult[T] = (json \ field.name).validate[T]
    }
    implicit def fieldShape[T : Reads](f: Field[T]) : Shape[T] = FieldShape(f)

    case class ObjectShape[T : Reads](obj: ObjectType[T]) extends Shape[T] {
      override def fromJson(json: JsValue): JsResult[T] = json.validate[T]
    }
    implicit def objectShape[T : Reads](f: ObjectType[T]) = ObjectShape(f)

//    case class Tuple2Shape[A,B](tup: (Field[A], Field[B])) extends Shape[(A,B)] {
//      override def fromJson(json: JsValue): JsResult[(A, B)] = {
//      }
//    }
//    implicit def tuple2Shape[A,B](tup: (Field[A], Field[B])) = Tuple2Shape(tup)
  }

  trait Projection[T] {
    def toJson: JsValue
    def fromJson(js: JsValue) : T
  }

  object Filters {
    trait BoolExpr {
      def && (other: BoolExpr) = AndExpr(this, other)
      def || (other: BoolExpr) = OrExpr(this, other)
    }
    case class AndExpr(left: BoolExpr, right: BoolExpr) extends BoolExpr
    case class OrExpr(left: BoolExpr, right: BoolExpr) extends BoolExpr
    case class BinaryBoolExpr(f: Field[_], op: String,  v: String) extends BoolExpr

    implicit class StringFilters(f: Field[String]) {
      def > (other: String) = BinaryBoolExpr(f, "gt", other)
      def < (other: String) = BinaryBoolExpr(f, "lt", other)
      def === (other: String) = BinaryBoolExpr(f, "eq", other)
    }

  }

  case class Route(
    uuid: String,
    wkt: String
  )

  class RouteType extends ObjectType[Route] {
    def uuid = Field[String]("uuid")
    def wkt = Field[String]("wkt")
  }

  object RouteType {
    def objectShape[T : Reads](f: ObjectType[T]) = ObjectShape(f)

    implicit val formatsRoute = Json.format[Route]
    implicit def routeShape(rt: RouteType) : Shape[Route] = objectShape[Route](rt).asInstanceOf[Shape[Route]]
  }

  case class Query[F, T : TypeTag](val p: T, val filter: Option[BoolExpr]= None)(implicit ev: T <:< Type[F], shp: T => Shape[F]) {
    val tag = implicitly[TypeTag[T]]

    def map[F2, T2 : TypeTag](q: T => T2)(implicit ev: T2 <:< Type[F2], shp: T2 => Shape[F2]) : Query[F2, T2] = new Query[F2, T2](q(p), filter)

    def filter(q: T => BoolExpr) : Query[F, T] = {
      val f = q(p)
      val newFilter : Option[BoolExpr] = filter.map(flt => flt.&&(f)).orElse(Some(f))
      new Query(p, newFilter)
    }

    def parseResult(json: JsValue) : JsResult[F] = shp(p).fromJson(json)
  }

  import Filters._
  import Shapes._
  import RouteType._

  object Query {
    def apply[F, T : TypeTag](o: T)(implicit ev: T <:< Type[F], shp: T => Shape[F]) = new Query(o)
  }

  import RouteType._
  val routeJson = Json.toJson(Route("lalal", "lololo"))

  val q2 = for {
    q <- Query(new RouteType()) if q.uuid === "hansworst"
  } yield (q.uuid)

  println(q2.parseResult(routeJson))


  val q3 = Query(new RouteType()).filter(q => q.uuid === "hallo").map(q => q.uuid )

  println(q3.tag)

}
