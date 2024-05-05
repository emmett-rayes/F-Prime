package fprime.untyped

import fprime.parsing.{asTokens, summonParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class UntypedLambdaTermTests extends AnyFunSuite:
    private def parseTerm(string: String): UntypedLambda =
        val input = string.asTokens
        val (remaining, output) = summonParser[UntypedLambda].parse(input).get
        assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: ${remaining}")
        output

    test("variable") {
        val term = parseTerm("x")
        term should matchPattern { case UntypedVariable(symbol) if symbol == "x" => }
    }

    test("abstraction") {
        val term = parseTerm("λx. y x")
        term should matchPattern { case _: UntypedAbstraction => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        assert(abstraction.parameter.symbol == "x")
        abstraction.body should matchPattern { case _: UntypedApplication => }
    }

    test("nested abstraction") {
        val term = parseTerm("λx y.x y z")
        term should matchPattern { case _: UntypedAbstraction => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        abstraction.body should matchPattern { case _: UntypedAbstraction => }
    }

    test("application") {
        val term = parseTerm("(λx. x) (λx. x)")
        term should matchPattern { case UntypedApplication(_, _) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case _: UntypedAbstraction => }
        application.argument should matchPattern { case _: UntypedAbstraction => }
    }

    test("application associativity") {
        val term = parseTerm("x y z")
        term should matchPattern { case UntypedApplication(_, _) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case _: UntypedApplication => }
        application.argument should matchPattern { case _: UntypedVariable => }
    }

    test("complex term") {
        val term = parseTerm("λx. a (λt. b x t (f (λu. a u t z) λs. w)) w y")
        term should matchPattern { case _: UntypedAbstraction => }
    }
