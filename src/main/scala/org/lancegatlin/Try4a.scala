package org.lancegatlin

import scala.language.existentials

object Try4a {
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
  }

  implicit object PersonSchema extends Schema[Person] {
    object id extends Field(_.id)
    object name extends Field(_.name)
    object age extends Field(_.age)
    val fields = Seq(id,name,age)
  }

  sealed trait EqualityOps
  case object Equals extends EqualityOps
  case object NotEquals extends EqualityOps

  sealed trait ComparisonOps
  case object LessThan extends ComparisonOps
  case object LessThanEquals extends ComparisonOps
  case object GreaterThan extends ComparisonOps
  case object GreatThanEquals extends ComparisonOps

  sealed trait Value[A]
  case class Literal[A](value: A) extends Value[A]
  case class Field[A](field: Schema[_]#Field[A]) extends Value[A]

  sealed trait Ast[A]
  case class EqualityTest[A](op: EqualityOps, _1: Field[A], _2: Literal[A]) extends Ast[A]
  case class ComparisonTest[A](op: ComparisonOps, _1: Field[A], _2: Literal[A]) extends Ast[A]
  case class And[A,B](_1: Ast[A], _2:Ast[B]) extends Ast[(A,B)]
  case class Or[A,B](_1: Ast[A], _2:Ast[B]) extends Ast[(A,B)]
  case class Not[A](_1: Ast[A]) extends Ast[A]

  implicit class PimpMyField[C,A](val self: Schema[C]#Field[A]) extends AnyVal {
    def ===(value: A) =
      EqualityTest(Equals, Field[A](self),Literal[A](value))
//    def ===(field: Schema[C]#Field[A]) =
//      EqualityTest(Equals, Field[A](self),Field[A](field))
    def <(value: A) =
      ComparisonTest(LessThan,Field[A](self),Literal[A](value))
//    def <(field: Schema[C]#Field[A])(implicit o:Ordering[A]) =
//      ComparisonTest(LessThan,Field[A](self),Field[A](field))
  }

  implicit class PimpMyAst[A](val self: Ast[A]) extends AnyVal {
    def and[B](other: Ast[B]) = And(self, other)
  }

  val ast =
  {
    import PersonSchema._
    id === 1 and name === "asdf" and age < 30
  }


    trait ToSql[A] {
    def apply[C](ast: Ast[A], builder: StringBuilder) : Unit = {
      ast match {
        case EqualityTest(op,field,literal) =>
          builder
            .append(field.field.name)
            .append(op match {
              case Equals => " = "
              case NotEquals => " != "
            })
            .append(valToSql(literal.value))
        case ComparisonTest(op,field,literal) =>
          builder
            .append(field.field.name)
            .append(op match {
              case LessThan => " < "
              case GreaterThan => " > "
              case LessThanEquals => " < "
              case GreatThanEquals => " > "
            })
            .append(valToSql(literal.value))

        case Not(_1) =>
          builder.append("Not(")
          apply(_1, builder)
          builder.append(")")

        case _ => throw new UnsupportedOperationException
      }
    }
    def valToSql(v: A) : String
  }

  implicit object toSql_Int extends ToSql[Int] {
    override def valToSql(a: Int): String = a.toString
  }
  implicit object toSql_String extends ToSql[String] {
    override def valToSql(a: String): String = s""""$a""""
  }

  trait ToSql_Tuple2[A,B] extends ToSql[(A,B)] {
    def aToSql : ToSql[A]
    def bToSql : ToSql[B]
    override def apply[C](ast: Ast[(A,B)], builder: StringBuilder) : Unit = {
      ast match {
        case And(ast1,ast2) =>
          aToSql.apply(ast1,builder)
          builder.append(" AND ")
          bToSql.apply(ast2,builder)

        case _ => throw new UnsupportedOperationException
      }
    }
    def valToSql(v: (A,B)) = ???
  }

  implicit def toSql_Tuple2[A,B](implicit
    _aToSql:ToSql[A],
    _bToSql:ToSql[B]
  ) = new ToSql_Tuple2[A,B] {
    implicit def aToSql = _aToSql
    implicit def bToSql = _bToSql
  }

  def astToSql[A](ast: Ast[A])(implicit toSql:ToSql[A]) : String = {
    val builder = new StringBuilder(256)
    toSql.apply(ast, builder)
    builder.result()
  }
}
