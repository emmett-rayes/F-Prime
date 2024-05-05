package fprime.language.untyped

import fprime.expression.Variable
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
        term should matchPattern { case UntypedVariable(Variable(symbol)) if symbol == "x" => }
    }

    test("abstraction") {
        val term = parseTerm("λx. y x")
        term should matchPattern { case UntypedAbstraction(_) => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        assert(abstraction.parameter.symbol == "x")
        abstraction.body should matchPattern { case UntypedApplication(_) => }
    }

    test("nested abstraction") {
        val term = parseTerm("λx y.x y z")
        term should matchPattern { case UntypedAbstraction(_) => }
        val abstraction = term.asInstanceOf[UntypedAbstraction]
        abstraction.body should matchPattern { case UntypedAbstraction(_) => }
    }

    test("application") {
        val term = parseTerm("(λx. x) (λx. x)")
        term should matchPattern { case UntypedApplication(_) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case UntypedAbstraction(_) => }
        application.argument should matchPattern { case UntypedAbstraction(_) => }
    }

    test("application associativity") {
        val term = parseTerm("x y z")
        term should matchPattern { case UntypedApplication(_) => }
        val application = term.asInstanceOf[UntypedApplication]
        application.callable should matchPattern { case UntypedApplication(_) => }
        application.argument should matchPattern { case UntypedVariable(_) => }
    }

    test("complex term") {
        val term = parseTerm("λx. a (λt. b x t (f (λu. a u t z) λs. w)) w y")
        term should matchPattern { case UntypedAbstraction(_) => }
    }
