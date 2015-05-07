package org.lancegatlin

import scala.language.higherKinds

object Try2b {
  case class Person(id: Int, name: String, age: Int)

  trait Schema[C] {
    class Field[A](val unapply: C => A) {
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

    trait Dialect[D[AA] <: (AA => R),R] {
      sealed trait Ast[A] {
        def and[B](other: Ast[B]) = And(this, other)
      }
      case class Equals[A](field: Field[A], ast: Ast[A]) extends Ast[A]
      case class LessThan[A](field: Field[A], ast: Ast[A]) extends Ast[A]
      case class Value[A](value: A)(implicit val d:D[A]) extends Ast[A]
      case class And[A,B](ast1: Ast[A],ast2:Ast[B]) extends Ast[(A,B)]

//      implicit class PimpMyField[A](val self: Field[A]) extends AnyVal {
//        def ===(value: A)(implicit d:D[A]) = Equals(self,Value(value))
//      }

      def apply[A](ast: Ast[A]) : R
    }

  }

  implicit object PersonSchema extends Schema[Person] {
    object id extends Field(_.id)
    object name extends Field(_.name)
    object age extends Field(_.age)
    val fields = Seq(id,name,age)
  }

  import scala.language.higherKinds

  trait ToSql[A] extends (A => String) {
    def apply(a: A) : String
  }

  implicit object toSql_Int extends ToSql[Int] {
    override def apply(a: Int): String = a.toString
  }
  implicit object toSql_String extends ToSql[String] {
    override def apply(a: String): String = s""""$a""""
  }

  object SqlDialect extends PersonSchema.Dialect[ToSql,String] {
    override def apply[A](ast: Ast[A]): String = {
      val builder = new StringBuilder(256)
      def loop[AA](ast: Ast[AA]) : Unit = {
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
            builder.append(v.d(value))
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

  val ast =
  {
    import PersonSchema._
    import SqlDialect._

    // TODO: make this work somehow?
//    id === 1 and age === 30
    And(And(Equals(id,Value(1)),Equals(age,Value(30))),Equals(name,Value("asdf")))
  }



}
