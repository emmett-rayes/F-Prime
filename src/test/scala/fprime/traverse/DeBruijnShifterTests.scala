package fprime.traverse

import fprime.parsing.{asTokens, summonParser}
import fprime.untyped.{UntypedLambda, given}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

class DeBruijnShifterTests extends AnyFunSuite:
    private def parseTerm(string: String): UntypedLambda =
        val input = string.asTokens
        val (remaining, output) = summonParser[UntypedLambda].parse(input).get
        assert(remaining.isEmpty, s"Input was not fully parsed. Remaining: ${remaining}")
        output

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
