package org.lancegatlin

import scala.language.higherKinds
import scala.language.existentials

object Try4b {
  case class Person(id: Int, name: String, age: Int)

  trait Schema[C] {
    class Field[A](val unapply: C => A) { self =>
      def name: String = {
        // org.lancegatlin.Try1$PersonSchema$id$
        val name = getClass.getName
        // Try1$PersonSchema$id
        val simpleName = name.substring(name.lastIndexOf('.') + 1).dropRight(1)
        // id
        simpleName.substring(simpleName.lastIndexOf('$') + 1)
      }
        // Note: bug in this call for objects
        //getClass.getSimpleName

      override def toString = s"Field($name)"
    }
    def fields: Seq[Field[_]]
  }

  implicit object PersonSchema extends Schema[Person] {
    object id extends Field(_.id)
    object name extends Field(_.name)
    object age extends Field(_.age)
    val fields = Seq(id,name,age)
  }

  trait ToDialect[A,R] extends (A => R)

  sealed trait Ops
  case object Equals extends Ops
  case object NotEquals extends Ops
  case object LessThan extends Ops
  case object LessThanEquals extends Ops
  case object GreaterThan extends Ops
  case object GreatThanEquals extends Ops

  sealed trait Relator
  case object And extends Relator
  case object Or extends Relator

  sealed trait Value[D,A] 
  case class Literal[D,A](value: A)(implicit val toDialect: ToDialect[A,D]) extends Value[D,A]
  case class Field[D,A](field: Schema[_]#Field[A]) extends Value[D,A]

  sealed trait Ast[D]
  case class Test[D,A](op: Ops, _1: Value[D,A], _2: Value[D,A]) extends Ast[D]
  case class R[D](r:Relator,_1: Ast[D], _2:Ast[D]) extends Ast[D]

  implicit class PimpMyField[A](val self: Schema[_]#Field[A]) extends AnyVal {
    def ===[D](value: A)(implicit d:ToDialect[A,D]) = Test(Equals, Field[D,A](self),Literal[D,A](value))
    def <[D](value: A)(implicit d:ToDialect[A,D]) = Test(LessThan,Field[D,A](self),Literal[D,A](value))
  }

  implicit class PimpMyAst[D](val self: Ast[D]) extends AnyVal {
    def and(other: Ast[D]) = R(And,self, other)
    def or(other: Ast[D]) = R(Or,self, other)
  }

  case class Sql(value: String)

  object SqlDialect {
    implicit object sql_Int extends ToDialect[Int,Sql] {
      override def apply(v1: Int): Sql = Sql(v1.toString)
    }
    implicit object sql_String extends ToDialect[String,Sql] {
      override def apply(v1: String): Sql = Sql(s""""$v1"""")
    }
  }

  val ast =
  {
    import PersonSchema._
    import SqlDialect._
    id === 1 and name === "asdf" and age < 30
  }


  def astToSql(e: Ast[Sql]) : String = {
    val builder = new StringBuilder(256)
    def valToString(_val: Value[Sql,_]) : String = {
      _val match {
        case l@Literal(value) => l.toDialect(value).value
        case Field(field) => field.name
      }
    }
    def opToString(op: Ops) : String = {
      op match {
        case Equals => " = "
        case NotEquals => " != "
        case LessThan => " < "
        case LessThanEquals => " <= "
        case GreaterThan => " > "
        case GreatThanEquals => " >= "
      }
    }
    def relatorToString(r: Relator) : String = {
      r match {
        case And => " AND "
        case Or => " OR "
      }
    }
    def loop(ast: Ast[Sql]) : Unit = {
      ast match {
        case Test(op,_1,_2) =>
          builder
            .append(valToString(_1))
            .append(opToString(op))
            .append(valToString(_2))
        case R(relator,_1,_2) =>
          loop(_1)
          builder.append(relatorToString(relator))
          loop(_2)
      }
    }
    loop(e)
    builder.result()
  }

  // TODO: this doesn't work if there is more than one schema involved
  // such as when a joined collection returns a tuple

  // TODO: for in-memory db, translating this to a C => Boolean is preferred
}
