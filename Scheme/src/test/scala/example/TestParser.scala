package example

import org.scalatest._

class TestParser extends FlatSpec with Matchers {

  "+" should "be parsed" in {
    var v = Parser.apply("(+ 1 2)")
    assert(v == Right(Function(Name("+"),List(TermExpr(IntTerm(1)), TermExpr(IntTerm(2))))))

    v = Parser.apply("(+ 1 2 3 4 5)")
    assert(v == Right(Function(Name("+"),List(TermExpr(IntTerm(1)), TermExpr(IntTerm(2)), TermExpr(IntTerm(3)), TermExpr(IntTerm(4)), TermExpr(IntTerm(5))))))

    v =  Parser.apply("(+ 2 (* 3 4))")
    assert(v == Right(Function(Name("+"),List(TermExpr(IntTerm(2)), Function(Name("*"),List(TermExpr(IntTerm(3)), TermExpr(IntTerm(4))))))))
  }

  "Other standard operations on numbers" should "be parsed" in {
    var v = Parser.apply("(- 1 2)")
    assert(v == Right(Function(Name("-"),List(TermExpr(IntTerm(1)), TermExpr(IntTerm(2))))))

    v = Parser.apply("(* 2 3)")
    assert(v == Right(Function(Name("*"),List(TermExpr(IntTerm(2)), TermExpr(IntTerm(3))))))

    v = Parser.apply("(/ 1 2.0)")
    assert(v == Right(Function(Name("/"),List(TermExpr(IntTerm(1)), TermExpr(DoubleTerm(2.0))))))

    v = Parser.apply("(< 1 2.0)")
    assert(v == Right(Function(Name("<"),List(TermExpr(IntTerm(1)), TermExpr(DoubleTerm(2.0))))))
  }

  "Define variable" should "be parsed" in {
    var v = Parser.apply("(define src 2)")
    assert(v == Right(Define(Name("src"), List(), List(TermExpr(IntTerm(2))))))

    v = Parser.apply("(define a (+ 1 3))")
    assert(v == Right(Define(Name("a"),List(), List(Function(Name("+"),List(TermExpr(IntTerm(1)), TermExpr(IntTerm(3))))))))
  }

  "Unary functions" should "be parsed" in {
    var v = Parser.apply("(+ (square 1) (square 2))")
    assert(v == Right(Function(Name("+"),List(Function(Name("square"), List(TermExpr(IntTerm(1)))), Function(Name("square"),List(TermExpr(IntTerm(2))))))))

    v = Parser.apply("(sin (/ pi 2))")
    assert(v == Right(Function(Name("sin"),List(Function(Name("/"),List(Name("pi"), TermExpr(IntTerm(2))))))))

    v = Parser.apply("(define (~ x y) (+ x y))")
    assert(v == Right(Define(Name("~"),List(Name("x"), Name("y")),List(Function(Name("+"),List(Name("x"), Name("y")))))))
  }

  "Define functions" should "be parsed" in {
    val v = Parser.apply("(define (square x) (* x x))")
    assert(v == Right(Define(Name("square"),List(Name("x")), List(Function(Name("*"),List(Name("x"), Name("x")))))))
  }

  "Lambdas" should "be parsed" in {
    var v = Parser.apply("(lambda (x) (* x x))")
    assert(v == Right(Lambda(List(Name("x")), List(Function(Name("*"),List(Name("x"), Name("x")))))))

    v = Parser.apply("(lambda () (display \"hello\"))")
    assert(v == Right(Lambda(List(),List(Function(Name("display"),List(TermExpr(StrTerm("\"hello\""))))))))

  }

  "List" should "be parsed" in {
    var v = Parser.apply("(list 1 \"string\" 2.0)")
    assert(v == Right(TermExpr(ListTerm(List(IntTerm(1), StrTerm("\"string\""), DoubleTerm(2.0))))))

    v = Parser.apply("(list 12345 (list 1 \"string\" 2.0))")
    assert(v == Right(TermExpr(ListTerm(List(IntTerm(12345), ListTerm(List(IntTerm(1), StrTerm("\"string\""), DoubleTerm(2.0))))))))

    v = Parser.apply("(define lst (list \"hello\" \"my\" \"world\"))")
    assert(v == Right(Define(Name("lst"),List(),List(TermExpr(ListTerm(List(StrTerm("\"hello\""), StrTerm("\"my\""), StrTerm("\"world\""))))))))
  }

  "Quote, Quasiquote" should "be parsed" in {
    var v = Parser.apply("'3")
    assert(v == Right(Quote(TermExpr(IntTerm(3)))))

    v = Parser.apply("'\"hi\"")
    assert(v == Right(Quote(TermExpr(StrTerm("\"hi\"")))))

    v = Parser.apply("'(+ 3 4)")
    assert(v == Right(Quote(Function(Name("+"),List(TermExpr(IntTerm(3)), TermExpr(IntTerm(4)))))))

    v = Parser.apply("'(lambda (x) (+ x 3))")
    assert(v == Right(Quote(Lambda(List(Name("x")),List(Function(Name("+"),List(Name("x"), TermExpr(IntTerm(3)))))))))

    v = Parser.apply("`(+ 1 ,(+ 3 4))")
    assert(v == Right(Quasiquote(Function(Name("+"),List(TermExpr(IntTerm(1)), Unquote(Function(Name("+"),List(TermExpr(IntTerm(3)), TermExpr(IntTerm(4))))))))))
  }

}
