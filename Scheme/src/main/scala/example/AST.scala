package example

sealed trait Term
case class IntTerm(value: Integer) extends Term
case class DoubleTerm(value: Double) extends Term
case class StrTerm(value: String) extends Term

case class ListTerm(value: List[Term]) extends Term


sealed trait Expression
case class Name(name: String) extends Expression
case class Define(name: Name, params: List[Name], body: List[Expression]) extends Expression
case class Function(name: Name, args: List[Expression]) extends Expression
case class Lambda(params: List[Name], body: List[Expression]) extends Expression
case class TermExpr(value: Term) extends Expression

case class Quote(data: Expression) extends Expression
case class Unquote(data: Expression) extends Expression
case class Quasiquote(data: Expression) extends Expression
