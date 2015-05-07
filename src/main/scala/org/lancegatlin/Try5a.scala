package org.lancegatlin

import scala.language.existentials

object Try5a {
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
  case object GreaterThanEquals extends ComparisonOps

  sealed trait Value[A]
  case class Literal[A](value: A) extends Value[A]
  case class Field[A](field: Schema[_]#Field[A]) extends Value[A]

  sealed trait PredicateAst[A] {
    def negate : PredicateAst[A]
  }
  case class EqualityTest[A](op: EqualityOps, _1: Field[A], _2: Literal[A]) extends PredicateAst[A] {
    def negate = op match {
      case Equals => copy(op = NotEquals)
      case NotEquals => copy(op = Equals)
    }
  }
  case class ComparisonTest[A](op: ComparisonOps, _1: Field[A], _2: Literal[A]) extends PredicateAst[A] {
    def negate = op match {
      case LessThan => copy(op = GreaterThanEquals)
      case LessThanEquals => copy(op = GreaterThan)
      case GreaterThan => copy(op = LessThanEquals)
      case GreaterThanEquals => copy(op = LessThan)
    }
  }
  case class And[A,B](_1: PredicateAst[A], _2:PredicateAst[B]) extends PredicateAst[(A,B)] {
    def negate = Or(_1.negate,_2.negate)
  }
  case class Or[A,B](_1: PredicateAst[A], _2:PredicateAst[B]) extends PredicateAst[(A,B)] {
    def negate = And(_1.negate,_2.negate)
  }

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

  implicit class PimpMyAst[A](val self: PredicateAst[A]) extends AnyVal {
    def and[B](other: PredicateAst[B]) = And(self, other)
    def unary_! = self.negate
  }

  val ast =
  {
    import PersonSchema._
    id === 1 and name === "asdf" and age < 30
  }

  implicit class PimpMyStringBuilder(val self: StringBuilder) extends AnyVal {
    def mapAppend(f: StringBuilder => StringBuilder) : StringBuilder =
      f(self)
  }

  object SqlDialect {
    type SqlBuilder = StringBuilder

    sealed trait ToSql[A] extends (PredicateAst[A] => (SqlBuilder => SqlBuilder))

    trait ToSqlValue[A] extends ToSql[A] {
      def apply(value: A) : SqlBuilder => SqlBuilder

      override def apply(ast: PredicateAst[A]) = { builder:SqlBuilder =>
        ast match {
          case EqualityTest(op,field,literal) =>
            builder
              .append(field.field.name)
              .append(op match {
                case Equals => " = "
                case NotEquals => " != "
              })
              .mapAppend(apply(literal.value))

          case ComparisonTest(op,field,literal) =>
            builder
              .append(field.field.name)
              .append(op match {
                case LessThan => " < "
                case GreaterThan => " > "
                case LessThanEquals => " < "
                case GreaterThanEquals => " > "
              })
              .mapAppend(apply(literal.value))

          case _ => throw new UnsupportedOperationException
        }
      }
    }

    implicit object ToSql_Int extends ToSqlValue[Int] {
      override def apply(value: Int) = _.append(value)
    }

    implicit object ToSql_String extends ToSqlValue[String] {
      override def apply(value: String) =
        _.append('"').append(value).append('"')
    }

    class ToSqlBranch[A,B](implicit
      aProc:ToSql[A],
      bProc:ToSql[B]
    ) extends ToSql[(A,B)] {
      override def apply(ast: PredicateAst[(A,B)])  = { builder:StringBuilder =>
        ast match {
          case And(ast1,ast2) =>
            builder
              .mapAppend(aProc(ast1))
              .append(" AND ")
              .mapAppend(bProc(ast2))

          case Or(ast1,ast2) =>
            builder
              .mapAppend(aProc(ast1))
              .append(" OR ")
              .mapAppend(bProc(ast2))

          case _ => throw new UnsupportedOperationException
        }
      }
    }

    implicit def toSqlBranch[A,B](implicit
      aProc:ToSql[A],
      bProc:ToSql[B]
    ) = new ToSqlBranch[A,B]

    def apply[A](ast: PredicateAst[A])(implicit toSql:ToSql[A]) : String = {
      val builder = new StringBuilder(256)
      toSql.apply(ast)(builder)
      builder.result()
    }
  }
}
