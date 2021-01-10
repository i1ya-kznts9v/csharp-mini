package example

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Parser extends RegexParsers with PackratParsers {

  private val int = "[0-9]+".r
  private val double = "[0-9]+\\.[0-9]*".r
  private val string = "\"[a-zA-Z][a-zA-Z0-9_]*\"".r
  private val operationName = "[+\\-*/<=>^~.!%]|[a-zA-Z][a-zA-Z0-9_]*".r

  def id: Parser[Name] = operationName ^^ Name

  def term: Parser[TermExpr] = "-" ~> double ^^ (n => TermExpr(DoubleTerm(-n.toDouble))) |
                            double ^^ (s => TermExpr(DoubleTerm(s.toDouble))) |
                            "-" ~> int ^^ (n => TermExpr(IntTerm(-n.toInt))) |
                            int ^^ (n => TermExpr(IntTerm(n.toInt))) |
                            string ^^ (s => TermExpr(StrTerm(s))) |
                            list

  def list: Parser[TermExpr] = "(" ~> "list".r ~ rep(term) <~ ")" ^^ {case _ ~ objects =>
                                                                        TermExpr(ListTerm(objects.map(o => o.value)))}

  def define: Parser[Expression] = "(" ~> "define".r ~ id ~ rep1(expression) <~ ")" ^^
                                     {case _ ~ name ~ v => Define(name, List.empty[Name], v)} |
                                   "(" ~> "define".r ~ ("(" ~> id ~ rep1(id) <~ ")") ~  rep(expression)  <~ ")" ^^
                                     {case _ ~ args ~ v => Define(args._1, args._2, v)}

  def lambda: Parser[Expression] = "(" ~> "lambda".r ~ ("(" ~> rep(id) <~ ")") ~  rep(expression)  <~ ")" ^^
                                     {case _ ~ args ~ v => Lambda(args, v)}

  lazy val member: PackratParser[Expression] = "(" ~> operationName ~ rep1(expression) <~ ")" ^^ operation

  lazy val expression: PackratParser[Expression] = term | define | lambda | member | id | quote | unquote | quasiquote

  def quote: Parser[Expression] = "'" ~ expression ^^ {case _ ~ data => Quote(data)}

  def unquote: Parser[Expression] = "," ~ expression ^^ {case _ ~ data => Unquote(data)}

  def quasiquote: Parser[Expression] = "`" ~ expression ^^ {case  _ ~ data => Quasiquote(data)}

  private def operation(p:  String ~ List[Expression]) = p match {
    case op ~ args => Function(Name(op), args)
  }

  def apply(code: String): Either[ParserError, Expression] =
    parse(expression, new PackratReader(new CharSequenceReader(code))) match {
      case Success(result, _) => Right(result)
      case NoSuccess(msg, _) => Left(ParserError(msg))
    }

  case class ParserError(msg: String)
}

