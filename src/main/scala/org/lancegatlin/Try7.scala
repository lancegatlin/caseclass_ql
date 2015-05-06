package org.lancegatlin

import scala.language.higherKinds
import scala.language.existentials

object Try7 {
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

  sealed trait EqualityOps
  case object Equals extends EqualityOps
  case object NotEquals extends EqualityOps

  sealed trait ComparisonOps
  case object LessThan extends ComparisonOps
  case object LessThanEquals extends ComparisonOps
  case object GreaterThan extends ComparisonOps
  case object GreatThanEquals extends ComparisonOps

  sealed trait Relator
  case object And extends Relator
  case object Or extends Relator

  sealed trait Value[D,C,A]
  case class Literal[D,C,A](value: A)(implicit val toDialect: ToDialect[A,D]) extends Value[D,C,A]
  case class Field[D,C,A](field: Schema[C]#Field[A]) extends Value[D,C,A]

  sealed trait Ast[D,C]
  case class EqualityTest[D,C,A](op: EqualityOps, _1: Field[D,C,A], _2: Value[D,C,A]) extends Ast[D,C]
  case class ComparisonTest[D,C,A](op: ComparisonOps, _1: Field[D,C,A], _2: Value[D,C,A])(implicit val o: Ordering[A]) extends Ast[D,C]
  case class And[D,C](_1: Ast[D,C], _2:Ast[D,C]) extends Ast[D,C]
  case class Or[D,C](_1: Ast[D,C], _2:Ast[D,C]) extends Ast[D,C]
  case class Not[D,C](_1: Ast[D,C]) extends Ast[D,C]

  implicit class PimpMyField[C,A](val self: Schema[C]#Field[A]) extends AnyVal {
    def ===[D](value: A)(implicit d:ToDialect[A,D]) =
      EqualityTest(Equals, Field[D,C,A](self),Literal[D,C,A](value))
    def ===[D](field: Schema[C]#Field[A]) =
      EqualityTest(Equals, Field[D,C,A](self),Field[D,C,A](field))
    def <[D](value: A)(implicit o:Ordering[A], d:ToDialect[A,D]) =
      ComparisonTest(LessThan,Field[D,C,A](self),Literal[D,C,A](value))
    def <[D](field: Schema[C]#Field[A])(implicit o:Ordering[A]) =
      ComparisonTest(LessThan,Field[D,C,A](self),Field[D,C,A](field))
  }

  implicit class PimpMyAst[D,C](val self: Ast[D,C]) extends AnyVal {
    def and(other: Ast[D,C]) = And(self, other)
    def or(other: Ast[D,C]) = Or(self, other)
  }

  object InMemoryDialect {
    implicit def ignored[A] = new ToDialect[A,Any] {
      override def apply(v1: A) = v1
    }
  }

  val ast : Ast[Any,Person] =
  {
    import PersonSchema._
    import InMemoryDialect._
    id === 1 and name === "asdf" and age < 30
  }


  val ast2 : Ast[Any,Person] = {
    import PersonSchema._
    import InMemoryDialect._
    id === age
  }

  // a macro will work better but just a proof of concept to show its possible
  def astToInMemory[C](ast: Ast[Any,C]) : C => Boolean = {
    def loop(ast1: Ast[Any,C]) : C => Boolean = {
      ast1 match {
        case EqualityTest(op,_1,_2) =>
          op match {
            case Equals =>
              _2 match {
                case Literal(value) =>
                  { c:C => _1.field.unapply(c) == value }
                case Field(field2) =>
                  { c:C => _1.field.unapply(c) == field2.unapply(c) }
              }
            case _ => ???
          }
        case t@ComparisonTest(op,_1,_2) =>
          op match {
            case LessThan =>
              _2 match {
                case Literal(value) =>
                { c:C => t.o.lt(_1.field.unapply(c),value) }
                case Field(field2) =>
                { c:C => t.o.lt(_1.field.unapply(c),field2.unapply(c))}
              }
            case _ => ???
          }
        case And(_1,_2) =>
          val f1 : C => Boolean = loop(_1)
          val f2 : C => Boolean = loop(_2)

          { c:C => f1(c) && f2(c) }
        case Or(_1,_2) =>
          val f1 = loop(_1)
          val f2 = loop(_2)

          { c:C => f2(c) || f2(c) }

        case Not(_1) =>
          val f1 = loop(_1)

          { c:C => !f1(c) }
      }
    }
    loop(ast)
  }
  
  // TODO: this doesn't work if there is more than one schema involved 
  // such as when a joined collection returns a tuple
}
