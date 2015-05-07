package org.lancegatlin

object Try3a {
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

  sealed trait Ast[C,A]
  case class Equals[C,A](field: Schema[C]#Field[A], ast: Ast[C,A]) extends Ast[C,A]
  case class LessThan[C,A](field: Schema[C]#Field[A], ast: Ast[C,A]) extends Ast[C,A]
  case class Value[C,A](value: A) extends Ast[C,A]
  case class And[C,A,B](ast1: Ast[C,A],ast2:Ast[C,B]) extends Ast[C,(A,B)]

  implicit class PimpMyField[C,A](val self: Schema[C]#Field[A]) extends AnyVal {
    def ===(value: A) = Equals(self,Value[C,A](value))
    def <(value: A) = LessThan(self,Value[C,A](value))
  }

  implicit class PimpMyAst[C,A](val self: Ast[C,A]) extends AnyVal {
    def and[B](other: Ast[C,B]) = And(self, other)
  }

  val ast =
  {
    import PersonSchema._
    id === 1 and name === "asdf" and age < 30
  }


  trait ToSql[A] {
    def apply(a: A) : String
  }

  implicit object toSql_Int extends ToSql[Int] {
    override def apply(a: Int): String = a.toString
  }
  implicit object toSql_String extends ToSql[String] {
    override def apply(a: String): String = s""""$a""""
  }

  trait ToSql_Tuple2[A,B] extends ToSql[(A,B)] {
    def aToSql : ToSql[A]
    def bToSql : ToSql[B]
  }

  implicit def toSql_Tuple2[A,B](implicit
    _aToSql:ToSql[A],
    _bToSql:ToSql[B]
  ) = new ToSql_Tuple2[A,B] {
    implicit def aToSql = _aToSql
    implicit def bToSql = _bToSql
    override def apply(a: (A, B)): String = ???
  }

  def astToSql[C,A](ast: Ast[C,A])(implicit outerToSql:ToSql[A]) : String = {
    val builder = new StringBuilder(256)
    def loop2[CC,AA,BB](ast: Ast[CC,(AA,BB)])(implicit toSql:ToSql_Tuple2[AA,BB]) : Unit = {
      ast match {
        case And(ast1,ast2) =>
          loop(ast1)(toSql.aToSql)
          builder.append(" AND ")
          loop(ast2)(toSql.bToSql)
        case _ => ???
      }
    }

    def loop[CC,AA](ast: Ast[CC,AA])(implicit toSql:ToSql[AA]) : Unit = {
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
        case Value(value) =>
          builder.append(toSql(value))
        case a@And(ast1,ast2) =>
          // The type params on And get lost here - end up with And[Any,Any,Any]
          // They shouldn't get lost since for And, AA is (A,B) so it should be
          // possible for the compiler to pass on that info here
          loop2(a)(toSql.asInstanceOf[ToSql_Tuple2[Any,Any]])
      }
    }
    loop(ast)
    builder.result()
  }
}
