package org.lancegatlin

import scala.language.higherKinds

object Try3b {
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

  sealed trait Ast[D,C,A]
  case class Equals[D,C,A](field: Schema[C]#Field[A], ast: Ast[D,C,A]) extends Ast[D,C,A]
  case class LessThan[D,C,A](field: Schema[C]#Field[A], ast: Ast[D,C,A]) extends Ast[D,C,A]
  case class Value[D,C,A](value: A)(implicit val toDialect: ToDialect[A,D]) extends Ast[D,C,A]
  case class And[D,C,A,B](ast1: Ast[D,C,A],ast2:Ast[D,C,B]) extends Ast[D,C,(A,B)]

  implicit class PimpMyField[C,A](val self: Schema[C]#Field[A]) extends AnyVal {
    def ===[D](value: A)(implicit d:ToDialect[A,D]) = Equals(self,Value[D,C,A](value))
    def <[D](value: A)(implicit d:ToDialect[A,D]) = LessThan(self,Value[D,C,A](value))
  }

  implicit class PimpMyAst[D,C,A](val self: Ast[D,C,A]) extends AnyVal {
    def and[B](other: Ast[D,C,B]) = And(self, other)
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


  def astToSql(ast: Ast[Sql,_,_]) : String = {
    val builder = new StringBuilder(256)
    def loop(ast: Ast[Sql,_,_]) : Unit = {
      ast match {
        case e@Equals(field,ast1) =>
          builder
            .append(field.name)
            .append(" = ")
          loop(ast1)
        case LessThan(field,ast1) =>
          builder
            .append(field.name)
            .append(" < ")
          loop(ast1)
        case v@Value(value) =>
          builder.append(v.toDialect(value).value)
        case a@And(ast1,ast2) =>
          loop(ast1)
          builder.append(" AND ")
          loop(ast2)
      }
    }
    loop(ast)
    builder.result()
  }
}
