package org.lancegatlin

object Try1 {
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


  def astToSql(ast: Ast[_,_]) : String = {
    val builder = new StringBuilder(256)
    def loop(ast: Ast[_,_]) : Unit = {
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
          // Can't just convert to String here, each dialect needs to do this differently
          // This is best done using a serializer type-class for the dialect
          builder.append(value)
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
