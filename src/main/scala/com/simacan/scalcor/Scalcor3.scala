package com.simacan.scalcor

import play.api.libs.json.{JsValue, Json}

/**
  * Created by jeroen on 12/20/15.
  */
object Scalcor3 extends App {
  trait Type[T] {
    type R <: T
    def fromJson(json: JsValue) : T
  }

  object Primitives {
    implicit val str = new Type[String] {
      override def fromJson(json: JsValue): String = json.as[String]
    }
    implicit val bool = new Type[Boolean] {
      override def fromJson(json: JsValue): Boolean = json.as[Boolean]
    }
  }

  case class Field[T](name: String, tt: Type[T]) {
    def value = tt
  }

  trait ObjectType[T] extends Type[T] {
    implicit val implicitType : Type[T] = this

    def field[T](name: String)(implicit tt: Type[T]) : Field[T] = {
      Field(name, tt)
    }

    val mapper: Mapper[T]

    def fromJson(json: JsValue) : T = mapper.fromJson(json)
  }


  import Primitives._

  trait Mapper[T] {
    def fromJson(json: JsValue): T
  }

  case class Object2Mapper[R,A,B](a: Field[A], b: Field[B], constructor: (A,B) => R) extends Mapper[R] {
    def fromJson(json: JsValue): R = {
      val f1 = a.tt.fromJson((json \ a.name).get)
      val f2 = b.tt.fromJson((json \ b.name).get)

      constructor(f1,f2)
    }
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

  import Filters._

  case class Query[T <: Type[_]](val p: T, val filter: Option[BoolExpr]= None) {

    def map[T2 <: Type[_]](q: T => T2) : Query[T2] = new Query[T2](q(p), filter)

    def filter(q: T => BoolExpr) : Query[T] = {
      val f = q(p)
      val newFilter : Option[BoolExpr] = filter.map(flt => flt.&&(f)).orElse(Some(f))
      new Query(p, newFilter)
    }
  }

  import Filters._

  object Query {
    def apply[T <: Type[_]](o: T)= new Query(o)
  }


  case class Address(street: String, city: String)

  class AddressType extends ObjectType[Address] {
    def street = field[String]("street")
    def city = field[String]("city")

    override val mapper: Mapper[Address] = Object2Mapper(street, city, Address.apply)
  }
  object AddressType extends AddressType

  case class Route(uuid: String, address: Address)


  class RouteType extends ObjectType[Route] {

    def uuid = field[String]("uuid")
    def address = field[Address]("address")(AddressType)

    override val mapper: Mapper[Route] = Object2Mapper(uuid, address, Route.apply)
  }

  object RouteType extends RouteType

  implicit val formatsAddress = Json.format[Address]
  implicit val formatsRoute = Json.format[Route]
  val routeJson = Json.toJson(Route("lalal", Address("Esstraat", "Enschede")))


  val q2 = for {
    q <- Query(new RouteType()) if q.uuid === "hansworst"
  } yield q

  println(q2)
  println(q2.p.fromJson(routeJson))


}
