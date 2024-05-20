package fprime.expression

import fprime.expression.Variable.VariableParser
import fprime.parsing.{asTokens, summonParser}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{matchPattern, should}

import scala.util.Success

class VariableTests extends AnyFunSuite:
    test("variable parser (green)") {
        val input = "x".asTokens
        val parser = summonParser[Variable[Expression]]
        val result = parser.parse(input)
        assert(result.isSuccess)
        result match
            case Success((_, Variable(symbol, _))) => assert(symbol == "x")
            case _                                 =>
    }

    test("variable parser (red)") {
        val input = "->".asTokens
        val parser = summon[VariableParser[?]].parser
        val result = parser.parse(input)
        assert(result.isFailure)
    }
