package fprime.traverse

import fprime.untyped.UntypedTerm
import fprime.untyped.UntypedTerm.UntypedTermParser
import fprime.util.parse
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnShifterTests extends AnyFunSuite:
    test("shift") {
        val term = parse[UntypedTerm]("(λx.λy. x (y w))")
        val converted = DeBruijnConverter.convert(term)
        val shifted = DeBruijnShifter.shift(converted, 2)
        val pretty = PrettyPrinter.pretty(shifted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ λ 2 (1 5)")
    }

    test("nested shift") {
        val term = parse[UntypedTerm]("(λx. x w (λy. y x w))")
        val converted = DeBruijnConverter.convert(term)
        val shifted = DeBruijnShifter.shift(converted, 2)
        val pretty = PrettyPrinter.pretty(shifted, mode = PrettyPrinter.Mode.Indexed)
        assert(pretty == "λ 1 4 (λ 1 2 5)")
    }
