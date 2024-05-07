package fprime.traverse

import fprime.util.parseTerm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnShifterTests extends AnyFunSuite:
    test("shift") {
        val term = parseTerm("(λx.λy. x (y w))")
        val converted = DeBruijnConverter.convert(term)
        val shifted = DeBruijnShifter.shift(converted, 2)
        val pretty = PrettyPrinter.pretty(shifted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ λ 2 (1 5)")
    }

    test("nested shift") {
        val term = parseTerm("(λx. x w (λy. y x w))")
        val converted = DeBruijnConverter.convert(term)
        val shifted = DeBruijnShifter.shift(converted, 2)
        val pretty = PrettyPrinter.pretty(shifted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ 1 4 (λ 1 2 5)")
    }
