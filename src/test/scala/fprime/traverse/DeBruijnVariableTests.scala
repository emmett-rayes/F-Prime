package fprime.traverse

import fprime.parsing.{asTokens, summonParser}
import fprime.untyped.{UntypedLambda, given}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnVariableTests extends AnyFunSuite:
    private def parseTerm(string: String): UntypedLambda =
        val input = string.asTokens
        val (remaining, output) = summonParser[UntypedLambda].parse(input).get
        assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: ${remaining}")
        output

    test("free variables") {
        val term = parseTerm("a b c")
        val converted = DeBruijnConverter.convert(term)
        val pretty = PrettyPrinter.pretty(converted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "1 2 3")
    }

    test("nested free variables") {
        val term = parseTerm("b (λx.λy.b)")
        val converted = DeBruijnConverter.convert(term)
        val pretty = PrettyPrinter.pretty(converted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "1 (λ λ 3)")
    }

    test("scopes") {
        val term = parseTerm("(λx.λy.λz. w x y z)")
        val converted = DeBruijnConverter.convert(term)
        val pretty = PrettyPrinter.pretty(converted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ λ λ 4 3 2 1")
    }

    test("nested scopes") {
        val term = parseTerm("(λw. (λx. w x y) (λx. x))")
        val converted = DeBruijnConverter.convert(term)
        val pretty = PrettyPrinter.pretty(converted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ (λ 2 1 3) (λ 1)")
    }
