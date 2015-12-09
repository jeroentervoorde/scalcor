package com.simacan.scalcor

import com.simacan.scalcor.Scalcor2.Filters.BoolExpr

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

  trait Projection[T] {
  }
  object Projection {
    class FieldProjection[T <: Field[_]](f: T) extends Projection[T]

    implicit def fieldProjection[T <: Field[_]](f: T) = new FieldProjection[T](f)

    class Tuple2Projection[A,B](tuple: (A,B))(implicit ev1: A => Projection[A], ev2: B => Projection[B]) extends Projection[(A,B)]

    implicit def tup2[A, B](t: (A,B))(implicit ev1: A => Projection[A], ev2: B => Projection[B]) = new Tuple2Projection[A, B](t)

    class ObjectTypeProjection[T <: ObjectType[_]](o: T) extends Projection[T]

    implicit def objectTypeProjection[T <: ObjectType[_]](o: T) : Projection[T] = new ObjectTypeProjection[T](o)
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


  case class Query[T](val p: T, val filter: Option[BoolExpr]= None)(implicit val projection: T => Projection[T]) {
    def map[T2](q: T => T2)(implicit tt: T2 => Projection[T2]) : Query[T2] = new Query[T2](q(p), filter)

    def filter(q: T => BoolExpr) : Query[T] = {
      val f = q(p)
      val newFilter : Option[BoolExpr] = filter.map(flt => flt.&&(f)).orElse(Some(f))
      new Query(p, newFilter)
    }
  }

  import Filters._
  import Projection._

  object Query {
    def apply[T](o: T)(implicit projection: T => Projection[T]) = new Query(o)
  }

  val q2 = for {
    q <- Query(new RouteType()) if q.uuid === "hansworst"
  } yield ( q.uuid )

  val q3 = Query(new RouteType()).filter(q => q.uuid === "hallo").map(q => (q.uuid, q.wkt) )

  println(q2)

  println(q3.projection(q3.p))

}
