package fprime.traverse

import fprime.util.parseTerm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnVariableTests extends AnyFunSuite:
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
