package com.simacan.scalcor

import com.simacan.scalcor.Scalcor2.Filters.BoolExpr
import com.simacan.scalcor.Scalcor2.TupleTypes.Tuple2Type
import play.api.libs.json._


import scala.reflect.runtime.universe._
/**
 * Created by jeroen on 12/8/15.
 */
object Scalcor2 extends App {

  trait Type[T] {
    type R >: T
  }


  abstract class ObjectType[F] extends Type[F] {
    override type R = F
  }

  case class Field[F](name: String) extends Type[F] {
    override type R = F
  }

  object TupleTypes {

    case class Tuple2Type[A <: Type[_],B <: Type[_]](val t: (A,B)) extends Type[(A#R,B#R)] {
      def tuple = this
    }

    implicit def tuple2Type[A <: Type[_],B <: Type[_]](t: (A,B)) = new Tuple2Type[A,B](t)
  }

  abstract class Shape[T] {
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

    case class Tuple2Shape[A <: Type[_],B <: Type[_]](tup: Tuple2Type[A,B])(implicit shp1: A => Shape[A#R], shp2: B => Shape[B#R]) extends Shape[Tuple2Type[A,B]#R] {
      override def fromJson(json: JsValue): JsResult[(A#R,B#R)] = {
        val a = shp1(tup.t._1).fromJson(json).get
        val b = shp2(tup.t._2).fromJson(json).get

        JsSuccess((a,b))
      }
    }
    implicit def tuple2Shape[A <: Type[_],B <: Type[_]](tup: Tuple2Type[A,B])(implicit shp1: A => Shape[A#R], shp2: B => Shape[B#R]) : Shape[Tuple2Type[A,B]#R] = Tuple2Shape(tup)
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

  case class Query[T <: Type[_]](val p: T, val filter: Option[BoolExpr]= None)(implicit shp: T => Shape[T#R]) {

    def map[T2 <: Type[_]](q: T => T2)(implicit shp: T2 => Shape[T2#R]) : Query[T2] = new Query[T2](q(p), filter)

    def filter(q: T => BoolExpr) : Query[T] = {
      val f = q(p)
      val newFilter : Option[BoolExpr] = filter.map(flt => flt.&&(f)).orElse(Some(f))
      new Query(p, newFilter)
    }

    def parseResult(json: JsValue) : JsResult[T#R] = shp(p).fromJson(json)
  }




  import Filters._
  import Shapes._
  import TupleTypes._

  object Query {
    def apply[T <: Type[_]](o: T)(implicit shp: T => Shape[T#R]) = new Query(o)
  }


  case class Route(
                    uuid: String,
                    wkt: String
                    )

  object Route {
    implicit val formatsRoute = Json.format[Route]
  }

  class RouteType extends ObjectType[Route] {
    def uuid = Field[String]("uuid")
    def wkt = Field[String]("wkt")
  }

  object RouteType extends RouteType

  val routeJson = Json.toJson(Route("lalal", "lololo"))

//  val q2 = Query(new RouteType()).map(q => Tuple2Type(q.wkt, q.uuid))
  val q2 = for {
    q <- Query(new RouteType()) if q.uuid === "hansworst"
  } yield (q.uuid, q.wkt).tuple

  println(q2)
  println(q2.parseResult(routeJson))

  val q3 = Query(new RouteType()).filter(q => q.uuid === "hallo").map(q => (q.uuid, q.wkt).tuple )

  println(q3)

}
