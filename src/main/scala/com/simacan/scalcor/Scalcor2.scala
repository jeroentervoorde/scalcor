package com.simacan.scalcor

import com.simacan.scalcor.Scalcor2.Filters.BoolExpr
import com.simacan.scalcor.Scalcor2.TupleTypes.Tuple2Type
import play.api.libs.json._


import scala.reflect.runtime.universe._
/**
 * Created by jeroen on 12/8/15.
 */
object Scalcor2 extends App {

//  abstract class Type[T](implict ev:) {
//    def ~[F2, T2](t: T2)(implicit ev: T2 <:< Type[F2]) : Tuple2Type((this, t))
//  }

  trait Type {
    type T
  }


  abstract class ObjectType[F] extends Type {
    type T = F

  }
  case class Field[F](name: String) extends Type {
    type T = F
  }

  object TupleTypes {
    case class Tuple2Type[A <: Type,B <: Type](t: (A,B)) extends Type

    implicit def tuple2Type[A <: Type,B <: Type](t: (A,B)) = new Tuple2Type[A,B](t)
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

    case class Tuple2Shape[A <: Type,B <: Type](tup: Tuple2Type[A,B])(implicit shp1: A => Shape[A#T], shp2: B => Shape[B#T]) extends Shape[(A#T,B#T)] {
      override def fromJson(json: JsValue): JsResult[(A#T,B#T)] = {
        val a = shp1(tup.t._1).fromJson(json).get
        val b = shp2(tup.t._2).fromJson(json).get

        JsSuccess((a,b))
      }
    }
    implicit def tuple2Shape[A <: Type,B <: Type](tup: Tuple2Type[A,B])(implicit shp1: A => Shape[A#T], shp2: B => Shape[B#T]) = Tuple2Shape(tup)
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

  case class Query[T <: Type](val p: T, val filter: Option[BoolExpr]= None)(implicit shp: T => Shape[T#T]) {

    def map[T2 <: Type](q: T => T2)(implicit shp: T2 => Shape[T2#T]) : Query[T2] = new Query[T2](q(p), filter)

    def filter(q: T => BoolExpr) : Query[T] = {
      val f = q(p)
      val newFilter : Option[BoolExpr] = filter.map(flt => flt.&&(f)).orElse(Some(f))
      new Query(p, newFilter)
    }

    def parseResult(json: JsValue) : JsResult[T#T] = shp(p).fromJson(json)
  }




  import Filters._
  import Shapes._
  import TupleTypes._

  object Query {
    def apply[T <: Type](o: T)(implicit shp: T => Shape[T#T]) = new Query(o)
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
  } yield (q.uuid, q.wkt)

  println(q2)
  println(q2.parseResult(routeJson))

  val q3 = Query(new RouteType()).filter(q => q.uuid === "hallo").map(q => q.uuid )

  println(q3)

}
